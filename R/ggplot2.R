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
 data=NULL,
### data.frame to start with for direct label computation.
 ...,
### passed to params.
 method=stop("must specify method= argument"),
### Positioning Method for direct label placement, passed to apply.method.
 debug=FALSE,
### Show directlabels debugging output?
 stat = "identity",
### passed to layer.
 position = "identity",
### passed to layer.
 inherit.aes = TRUE
### inherit aes from global ggplot definition?
 ){
### ggproto object implementing direct labels.
  GeomDl <- ggplot2::ggproto(
    "GeomDl", ggplot2::Geom,
    draw_panel = function(data, panel_scales, coord, method = NULL, debug = FALSE) {
      data$rot <- as.numeric(data[["angle"]])
      groups.col <- if(all(is.na(data[["label.group"]])))"label" else "label.group"
      data$groups <- data[[groups.col]]
      axes2native <- function(data){
        coord$transform(data, panel_scales)
      }
      converted <- axes2native(data)
      ## for some reason ggplot2 gives us a group column even when the
      ## user does not specify one in aes.
      dldata <- converted[, names(converted) != "group"]
      dlgrob(
        dldata, method, debug = debug, axes2native = axes2native)
    },
    draw_legend = ggplot2::draw_key_text,
    required_aes = c("x", "y", "label"),
    default_aes = ggplot2::aes(
      colour = "black", size = 5, angle = 0, hjust = 0.5,
      vjust = 0.5, alpha = 1, label.group = NA)
  )
  ## Geom for direct labeling that creates dlgrobs in the draw()
  ## method.
  ggplot2::layer(
    data = data,
    mapping = mapping,
    geom = GeomDl,
    stat = stat,
    position = position,
    show.legend = FALSE, # since direct labels replace a legend.
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      debug = debug,
      ...)
  )
},ex=function(){
  if(require(ggplot2)){
    vad <- as.data.frame.table(VADeaths)
    names(vad) <- c("age","demographic","deaths")
    ## color + legend
    leg <- ggplot(vad,aes(deaths,age,colour=demographic))+
      geom_line(aes(group=demographic))+
      xlim(8,80)
    print(direct.label(leg,list("last.points",rot=30)))
    ## this is what direct.label is doing internally:
    labeled <- leg+
      geom_dl(aes(label=demographic), method=list("last.points",rot=30))+
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
  }
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
  getData <- function(colour.or.fill){
    for(L in p$layers){
      m <- p$mapping
      m[names(L$mapping)] <- L$mapping
      ## TODO: what if this is an expression and not a variable name?
      colvar <- m[[colour.or.fill]]
      colvar.str <- if(is.null(colvar) || utils::packageVersion("ggplot2") <= "2.2.1"){
        paste(colvar)
      }else{
        rlang::quo_name(colvar)
      }
      if(!is.null(colvar)){
        return(list(layer=L, colvar=colvar.str))
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
  geom <- tolower(sub("^Geom", "", class(L$geom)[1]))
  if(is.null(method))method <- default.picker("ggplot")
  data <- if( (!is.null(L$data)) && (length(L$data) > 0) ){
    L$data
  }else{
    NULL
  }
  a <- ggplot2::aes_string(label=paste0("`", colvar, "`"), colour=paste0("`", colvar, "`"))
  not.label.colour <- L$mapping[!names(L$mapping) %in% names(a)]
  a2 <- structure(c(not.label.colour, a), class="uneval")
  dlgeom <- geom_dl(mapping=a2,method=method,
                    stat=L$stat,debug=debug,data=data)
  dlgeom$stat_params <- L$stat_params
  ## Look through legends for a colour/fill legend.
  leg.info <- tryCatch({
    legends2hide(p)
  }, error=function(E){
    NULL #ignore errors in parsing custom/non-standard ggplots.
  })
  guide.args <- as.list(rep("none", length(leg.info$hide)))
  names(guide.args) <- leg.info$hide
  guide.args$colour <- "none"
  guide <- do.call(ggplot2::guides, guide.args)
  p+dlgeom+guide
### The ggplot object with direct labels added.
}

### https://github.com/tdhock/directlabels/issues/2 CRAN won't
### complain about this version of :::
pkgFun <- function(fun, pkg="ggplot2") {
  get(fun, envir = asNamespace(pkg))
}

### Extract guides to hide from a ggplot.
legends2hide <- function(p){
  plistextra <- ggplot2::ggplot_build(p)
  plot <- plistextra$plot
  scales = plot$scales
  layers = plot$layers
  default_mapping = plot$mapping
  plot_theme <- pkgFun("plot_theme")
  theme <- plot_theme(plot)
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

  guides <- defaults(
    plot$guides, ggplot2::guides(colour="legend", fill="legend"))
  labels <- plot$labels
  guides_train <- pkgFun("guides_train")
  gdefs <- guides_train(scales = scales, theme = theme,
                        guides = guides, labels = labels)
  if (length(gdefs) != 0) {
    guides_merge <- pkgFun("guides_merge")
    gdefs <- guides_merge(gdefs)
    guides_geom <- pkgFun("guides_geom")
    gdefs <- guides_geom(gdefs, layers, default_mapping)
  } else (ggplot2::zeroGrob())
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

