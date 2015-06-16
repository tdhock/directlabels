uselegend.trellis <- function
### Add a legend to a trellis plot, for comparison.
(p,
### The trellis object.
 ...
### Ignored.
 ){
  if(is.null(p$legend))update(p,auto.key=TRUE)
  else p
}
 
### Some lattice plot functions do some magic in the background to
### translate the data you give them into the data points that are
### plotted onscreen. We have to replicate this magic in native
### coordinate space before applying the Positioning Method in cm
### space. These functions accomplish this translation.
lattice.translators <- list(qqmath=function(d,distribution,f.value,qtype=7,...){
  ## Transformation function for 1d qqmath plots. This is a copy-paste
  ## from panel.qqmath. (total hack)
  gapply(d,function(d,...){
    x <- as.numeric(d$x)
    distribution <- if (is.function(distribution)) 
      distribution
    else if (is.character(distribution)) 
      get(distribution)
    else eval(distribution)
    nobs <- sum(!is.na(x))
    if (is.null(f.value)) 
      data.frame(x = distribution(ppoints(nobs)), y = sort(x))
    else data.frame(x = distribution(
                      if (is.numeric(f.value))f.value
                      else f.value(nobs)),
                    y = quantile(x,
                      if (is.numeric(f.value))f.value
                      else f.value(nobs),
                      names = FALSE, type = qtype, na.rm = TRUE))
  })
},densityplot=gapply.fun({
  dens <- density(d$x,na.rm=TRUE)
  data.frame(x=dens$x,y=dens$y)
}))

direct.label.trellis <- function
### Add direct labels to a grouped lattice plot. This works by parsing
### the trellis object returned by the high level plot function, and
### returning it with a new panel function that will plot direct
### labels using the specified method.
(p,
### The lattice plot (result of a call to a high-level lattice
### function).
 method=NULL,
### Method for direct labeling as described in
### \code{\link{apply.method}}.
 debug=FALSE
### Show debug output?
 ){
  #print(p$panel)
  old.panel <- if(is.character(p$panel))get(p$panel) else p$panel
  #print(old.panel)
  lattice.fun.name <- paste(p$call[[1]])
  p$panel <-
    function(panel.groups=paste("panel.",lattice.fun.name,sep=""),...){
      ## TODO: redo this interface... to fix (1) lattice problem with
      ## custom specified color labels and (2) name clash with method
      ## from xYplot which I suppose should work.
      panel.superpose.dl(panel.groups=panel.groups,
                         .panel.superpose=old.panel,
                         method=method,
                         ...)
  }
  p$legend <- NULL
  update(p,debug=debug)
### The lattice plot.
}

panel.superpose.dl <- structure(function
### Call panel.superpose for the data points and then for the direct
### labels. This is a proper lattice panel function that behaves much
### like panel.superpose.
(x,
### Vector of x values.
 y=NULL,
### Vector of y values.
 subscripts,
### Subscripts of x,y,groups.
 groups,
### Vector of group ids.
 panel.groups,
### To be parsed for default labeling method, and passed to
### panel.superpose.
 method=NULL,
### Positioning Method for direct labeling. NULL indicates to choose a
### Positioning Method based on the panel.groups function.
 .panel.superpose=panel.superpose,
### The panel function to use for drawing data points.
 type="p",
### Plot type, used for default method dispatch.
 debug=FALSE,
### passed to dlgrob.
 ...
### passed to real panel function, and to translator.
 ){
  rgs <- list(x=x,subscripts=subscripts,groups=groups,type=type,`...`=...)
  if(!missing(y))rgs$y <- y
  ## FIXME: this is a total hack:
  tryCatch(do.call(".panel.superpose",c(rgs,panel.groups=panel.groups))
           ,error=function(e)do.call(".panel.superpose",rgs))
  if(missing(panel.groups))panel.groups <- "panel.xyplot" #lattice default
  subs <-
    if(is.character(panel.groups))panel.groups else substitute(panel.groups)
  lattice.fun.name <-
    if(is.character(subs))sub("panel.","",subs) else ""
  if(is.null(type))type <- "NULL"
  if(is.null(method))method <- default.picker("trellis")
  groups <- as.factor(groups)
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  d$y <- if(missing(y))NA else y
  type <- type[type!="g"] ## printing the grid twice looks bad.
  col.text <-
    switch(type,p="superpose.symbol",l="superpose.line","superpose.line")
  tpar <- trellis.par.get()
  col.values <- if("col" %in% names(rgs)){
    rgs$col
  }else{
    tpar[[col.text]]$col
  }
  key <- rep(col.values, length.out=nlevels(d$groups))
  names(key) <- levels(d$groups)
  ## maybe eventually allow these to be specified in options()??
  translator <- lattice.translators[[lattice.fun.name]]
  if(!is.null(translator)){
    d <- apply.method(translator,d,...)
  }
  d$colour <- key[as.character(d$groups)]
  g <- dlgrob(d,method,debug=debug)
  grid.draw(g)
},ex=function(){
  loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                     type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
  ## 3 equivalent ways to make the same plot:
  library(lattice)
  print(direct.label( ## most user-friendly
                     densityplot(~ppp,loci,groups=type,n=500)
                     ))
  print(direct.label( ## exactly the same as above but with specific panel fns
                     densityplot(~ppp,loci,groups=type,n=500,
                                 panel=panel.superpose,
                                 panel.groups="panel.densityplot")
                     ))
  ## using panel.superpose.dl as the panel function automatically adds
  ## direct labels
  print(densityplot(~ppp,loci,groups=type,n=500,
                    panel=panel.superpose.dl,panel.groups="panel.densityplot"))

  ## Exploring custom panel and panel.groups functions
  library(ggplot2)
  library(nlme)
  ## Say we want to use a simple linear model to explain rat body weight:
  fit <- lm(weight~Time+Diet+Rat,BodyWeight)
  bw <- fortify(fit,BodyWeight)
  ## lots of examples to come, all with these arguments:
  ratxy <- function(...){
    xyplot(weight~Time|Diet,bw,groups=Rat,type="l",layout=c(3,1),...)
  }
  ## No custom panel functions:
  ##regular <- ratxy(par.settings=simpleTheme(col=c("red","black")))
  regular <- ratxy()
  print(regular) ## normal lattice plot
  print(direct.label(regular)) ## with direct labels

  ## The direct label panel function panel.superpose.dl can be used to
  ## display direct labels as well:
  print(ratxy(panel=panel.superpose.dl,panel.groups="panel.xyplot"))
  print(ratxy(panel=function(...)
              panel.superpose.dl(panel.groups="panel.xyplot",...)))

  ## Not very user-friendly, since default label placement is
  ## impossible, but these should work:
  print(ratxy(panel=panel.superpose.dl,panel.groups=panel.xyplot,
              method=first.points))
  print(ratxy(panel=function(...)
              panel.superpose.dl(panel.groups=panel.xyplot,...),
              method=first.points))

  ## Custom panel.groups functions:
  ## This panel.groups function will display the model fits:
  panel.model <- function(x,subscripts,col.line,...){
    panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
    llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
  }
  pg <- ratxy(panel=panel.superpose,panel.groups=panel.model)
  print(pg)
  ## If you use panel.superpose.dl with a custom panel.groups function,
  ## you need to manually specify the Positioning Method, since the
  ## name of panel.groups is used to infer a default:
  print(direct.label(pg,method="first.qp"))
  print(ratxy(panel=panel.superpose.dl,panel.groups="panel.model",
              method="first.qp"))

  ## Custom panel function that draws a box around values:
  panel.line1 <- function(ps=panel.superpose){
    function(y,...){
      panel.abline(h=range(y))
      ps(y=y,...)
    }
  }
  custom <- ratxy(panel=panel.line1())
  print(custom)
  print(direct.label(custom))
  ## Alternate method, producing the same results, but using
  ## panel.superpose.dl in the panel function. This is useful for direct
  ## label plots where you use several datasets.
  print(ratxy(panel=panel.line1(panel.superpose.dl),panel.groups="panel.xyplot"))

  ## Lattice plot with custom panel and panel.groups functions:
  both <- ratxy(panel=panel.line1(),panel.groups="panel.model")
  print(both)
  print(direct.label(both,method="first.qp"))
  print(ratxy(panel=panel.line1(panel.superpose.dl),
              panel.groups=panel.model,method="first.qp"))
})

defaultpf.trellis <- function
### If no Positioning Method specified, choose a default using this
### function. The idea is that this is called with all the variables
### in the environment of panel.superpose.dl, and this can be
### user-customizable by setting the directlabels.defaultpf.lattice
### option to a function like this.
(lattice.fun.name,groups,type,...){
  ldefault <- function(){
    if(nlevels(groups)==2)"lines2" else "maxvar.qp"
  }
  lattice.fun.name <-
    switch(lattice.fun.name,
           qqmath="xyplot",
           lattice.fun.name)
  switch(lattice.fun.name,
         dotplot=ldefault(),
         xyplot=if("p"%in%type)"smart.grid" else ldefault(),
         densityplot="top.bumptwice",
         rug="rug.mean",
         xyplot.zoo=ldefault(),
         stop("No default direct label placement method for '",
              lattice.fun.name,"'.\nPlease specify method."))
}
