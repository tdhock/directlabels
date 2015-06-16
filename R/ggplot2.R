uselegend.ggplot <- function
### Show the ggplot2 legend, for comparison.
(p,
### The ggplot object.
 ...
### Ignored.
 ){
  p
}

geom_dl <- structure(function
### Geom that will plot direct labels.
(mapping=NULL,
### aes(label=variable_that_will_be_used_as_groups_in_Positioning_Methods).
 method,
### Positioning Method.
 ...,
### passed to GeomDirectLabel$new. ie stat= position= debug=
 show_guide=FALSE
### show legend? default FALSE since direct labels replace a legend.
 ){
  require(ggplot2)
  require(proto)
  ## Geom for direct labeling that creates dlgrobs in the draw()
  ## method.
  GeomDirectLabel <- proto(ggplot2:::Geom, {
    draw_groups <- function(., ...) .$draw(...)
    draw <- function(., data, scales, coordinates,
                     method=NULL,debug=FALSE, ...) {
      data$rot <- as.integer(data$angle)
      data$groups <- data$label
      axes2native <- function(data){
        ggplot2:::coord_transform(coordinates,data,scales)
      }
      converted <- axes2native(data)
      dldata <- converted[,names(converted)!="group"]
      dlgrob(dldata,
             method,debug=debug,
             axes2native=axes2native)
    }
    draw_legend <- function(.,data,...){
      nullGrob()
    }
    objname <- "dl"
    desc <- "Direct labels"
    default_stat <- function(.) ggplot2:::StatIdentity
    required_aes <- c("x", "y", "label")
    default_aes <- function(.)
      aes(colour="black", size=5 , angle=0, hjust=0.5, vjust=0.5, alpha = 1)
  })
  GeomDirectLabel$new(mapping, method=method, show_guide=show_guide, ...)
### Layer that will plot direct labels.
},ex=function(){
  library(ggplot2)
  vad <- as.data.frame.table(VADeaths)
  names(vad) <- c("age","demographic","deaths")
  ## color + legend
  leg <- ggplot(vad,aes(deaths,age,colour=demographic))+
    geom_line(aes(group=demographic))+
    xlim(8,80)
  print(direct.label(leg,list("last.points",rot=30)))
  ## this is what direct.label is doing internally:
  labeled <- leg+
    geom_dl(aes(label=demographic),list("last.points",rot=30))+
    scale_colour_discrete(guide="none")
  print(labeled)
  ## no color, just direct labels!
  p <- ggplot(vad,aes(deaths,age))+
    geom_line(aes(group=demographic))+
    geom_dl(aes(label=demographic),method="top.qp")
  print(p)
  ## add color:
  p+aes(colour=demographic)+
    scale_colour_discrete(guide="none")
  ## add linetype:
  p+aes(linetype=demographic)+
    scale_linetype(guide="none")
  ## no color, just direct labels
  library(nlme)
  bwbase <- ggplot(BodyWeight,aes(Time,weight,label=Rat))+
    geom_line(aes(group=Rat))+
    facet_grid(.~Diet)
  bw <- bwbase+geom_dl(method="last.qp")
  print(bw)
  ## add some more direct labels
  bw2 <- bw+geom_dl(method="first.qp")
  print(bw2)
  ## add color
  colored <- bw2+aes(colour=Rat)+
    scale_colour_discrete(guide="none")
  print(colored)
  ## or just use direct.label if you use color:
  direct.label(bwbase+aes(colour=Rat),dl.combine("first.qp","last.qp"))

  ## iris data example
  giris <- ggplot(iris,aes(Petal.Length,Sepal.Length))+
    geom_point(aes(shape=Species))
  giris.labeled <- giris+
    geom_dl(aes(label=Species),method="smart.grid")+
    scale_shape_manual(values=c(setosa=1,virginica=6,versicolor=3),
                       guide="none")
  ##png("~/R/directlabels/www/scatter-bw-ggplot2.png",h=503,w=503)
  print(giris.labeled)
  ##dev.off()
})

direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in
### \code{\link{apply.method}}.
 debug=FALSE
### Show debug output?
 ){
  require(ggplot2)
  getData <- function(colour.or.fill){
    for(L in p$layers){
      m <- p$mapping
      m[names(L$mapping)] <- L$mapping
      ## TODO: what if this is an expression and not a variable name?
      colvar <- m[[colour.or.fill]]
      if(!is.null(colvar)){
        return(list(layer=L, colvar=as.character(colvar)))
      }
    }
  }
  dl.info <- getData("colour")
  if(is.null(dl.info)){
    dl.info <- getData("fill")
  }
  if(is.null(dl.info)){
    stop("Need colour or fill aesthetic to infer default direct labels.")
  }
  L <- dl.info$layer
  colvar <- dl.info$colvar
  ## Try to figure out a good default based on the colored geom
  geom <- L$geom$objname
  if(is.null(method))method <- default.picker("ggplot")
  data <- if( (!is.null(L$data)) && (length(L$data) > 0) ){
    L$data
  }else{
    NULL
  }
  a <- aes_string(label=colvar, colour=colvar)
  a2 <- structure(c(L$mapping, a), class="uneval")
  dlgeom <- geom_dl(a2,method,
                    stat=L$stat,debug=debug,data=data)
  dlgeom$stat_params <- L$stat_params
  ## Look through legends for a colour/fill legend.
  leg.info <- legends2hide(p)
  guide.args <- as.list(rep("none", length(leg.info$hide)))
  names(guide.args) <- leg.info$hide
  guide.args$colour <- "none"
  guide <- do.call(guides, guide.args)
  p+dlgeom+guide
### The ggplot object with direct labels added.
}

### Extract guides to hide from a ggplot.
legends2hide <- function(p){
  plistextra <- ggplot2::ggplot_build(p)
  plot <- plistextra$plot
  scales = plot$scales
  layers = plot$layers
  default_mapping = plot$mapping
  theme <- ggplot2:::plot_theme(plot)
  position <- theme$legend.position
  # by default, guide boxes are vertically aligned
  theme$legend.box <- if(is.null(theme$legend.box)) "vertical" else theme$legend.box
  
  # size of key (also used for bar in colorbar guide)
  theme$legend.key.width <- if(is.null(theme$legend.key.width)) theme$legend.key.size
  theme$legend.key.height <- if(is.null(theme$legend.key.height)) theme$legend.key.size
  # by default, direction of each guide depends on the position of the guide.
  theme$legend.direction <- if(is.null(theme$legend.direction)){
    if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
      switch(position[1], top =, bottom = "horizontal", left =, right = "vertical")
    else
      "vertical"
  }
  # justification of legend boxes
  theme$legend.box.just <-
    if(is.null(theme$legend.box.just)) {
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position, bottom =, top = c("center", "top"), left =, right = c("left", "top"))
      else
        c("center", "center")
    } 
  
  position <- theme$legend.position
  defaults <- function (x, y) {
    c(x, y[setdiff(names(y), names(x))])
  }

  guides <- defaults(plot$guides, guides(colour="legend", fill="legend"))
  labels <- plot$labels
  gdefs <- ggplot2:::guides_train(scales = scales, theme = theme,
                                  guides = guides, labels = labels)
  if (length(gdefs) != 0) {
    gdefs <- ggplot2:::guides_merge(gdefs)
    gdefs <- ggplot2:::guides_geom(gdefs, layers, default_mapping)
  } else (ggplot2:::zeroGrob())
  var.list <- lapply(gdefs, getLegendVariables)
  for(v in c("colour", "fill")){
    for(L in var.list){
      if(v %in% L$var){
        return(list(colour=v, hide=L$var, data=L$data))
      }
    }
  }
### NULL if no legends with colour or fill to hide.
}

### get the aes which are variable in one legend.
getLegendVariables <- function(mb){
  guidetype <- mb$name
  key <- mb$key
  results <- list()
  for(g in mb$geoms){
    orig <- g$data
    geom <- g$geom$objname
    if(nrow(orig)==0) return(data.frame()); # if no rows, return an empty df.
    orig$order <- 1:nrow(orig)
    count.na <- function(x)sum(is.na(x))
    orig.na <- sapply(orig, count.na)>0
    key.na <- sapply(key, count.na)>0
    by <- intersect(names(orig.na)[!orig.na], names(key.na)[!key.na])
    data <- merge(orig, key, by=by)
    data <- data[order(data$order),]
    ## old code above.
    data <- data.frame(orig, key)
    ## if there are no labels, return an empty df.
    if(!".label"%in%names(data)) return(data.frame()); 
    ## remove cols that are entirely na
    results[[length(results)+1]] <- data[,which(colSums(!is.na(data))>0)] 
  }
  results <- results[which(sapply(results, nrow)>0)]
  df <- merge_recurse(results)
  variable <- c()
  for(v in c("colour", "fill", "size", "shape", "linetype")){
    vals <- df[[v]]
    first <- vals[1]
    if(!is.null(first) && !is.na(first)){
      constant <- all(first == vals)
      if(!constant){
        variable <- c(variable, v)
      }
    }
  }
  list(variable=variable,
       data=df)
}

### Copied from reshape.
merge_recurse <- function (dfs, ...) {
  if (length(dfs) == 1) {
    dfs[[1]]
  }
  else if (length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all.x = TRUE, sort = FALSE, ...)
  }
  else {
    merge(dfs[[1]], Recall(dfs[-1]), all.x = TRUE, sort = FALSE, 
          ...)
  }
}

defaultpf.ggplot <- function
### Default method selection method for ggplot2 plots.
(geom,p,L,colvar,...){
  switch(geom,
         density="top.bumptwice",
         line={
           groups <- L$data[[colvar]]
           if(is.null(groups))groups <- p$data[[colvar]]
           if(nlevels(groups)==2)"lines2" else "maxvar.qp"
         },
         point="smart.grid",
         path="bottom.pieces",
         ribbon="maxvar.qp",
         stop("No default label placement for this type of ggplot."))
}

