### Process data points using the Positioning Method and draw the
### resulting direct labels. This is called for every panel with
### direct labels, every time the plot window is resized.
drawDetails.dlgrob <- function
(x,
### The dlgrob list object. x$method should be a Positioning Method
### list and x$data should be a data.frame with the following
### variables: \describe{
### \item{x,y}{numeric horizontal and vertical positions of direct
### labels, in native units. These are converted to cm units before
### applying the Positioning Method.}
### \item{groups}{factor that indices the different groups, and
### colour indicates the corresponding group colour.}
### \item{hjust and vjust}{(optional) numeric values usually in
### [0,1] that control the justification of the text label relative to
### the x,y position.}
### \item{rot}{(optional) numeric value in [0,360] that specifies
### the degrees which the text should be rotated.}
### \item{cex, alpha, fontface, fontfamily}{(optional) passed to
### gpar.}
### } Additionally, x$debug should be set to TRUE or
### FALSE, and x$axestonative should be a function that converts units
### shown on the axes to native units of x$data[,c("x","y")].
 recording
 ){
  ## calculate x and y position in cm --- by this time we should have
  ## done any preprocessing necessary to convert 1d data to 2d data!
  cm.data <- x$data
  cm.data$x <- convertX(unit(cm.data$x,"native"),"cm",valueOnly=TRUE)
  cm.data$y <- convertY(unit(cm.data$y,"native"),"cm",valueOnly=TRUE)
  cm.data$groups <- factor(cm.data$groups)
  ## save original levels for later in case Positioning Methods mess
  ## them up.
  levs <- unique(cm.data[,c("groups","colour")])
  code <- as.character(levs$colour)
  names(code) <- as.character(levs$groups)
  ## apply ignore.na function -- these points are not plotted
  cm.data <- ignore.na(cm.data)
  if(is.null(cm.data$label)){
    cm.data$label <- cm.data$groups
  }
  cm.data <- apply.method(
    x$method,
    cm.data,
    debug=x$debug,
    axes2native=x$axes2native)
  if(nrow(cm.data)==0)return()## empty data frames can cause many bugs
  ## Take col from colour or groups.
  colour <- cm.data[["colour"]]
  cm.data$col <- if(is.null(colour)){
    code[as.character(cm.data$groups)]
  } else {
    colour
  }
  ## defaults for grid parameter values:
  defaults <- list(hjust=0.5,vjust=0.5,rot=0)
  for(p in names(defaults)){
    if(!p %in% names(cm.data))cm.data[,p] <- NA
    cm.data[is.na(cm.data[,p]),p] <- defaults[[p]]
  }
  cm.data <- unique(cm.data)
  gpargs <- c("cex","alpha","fontface","fontfamily","col")
  gp <- do.call(gpar,cm.data[names(cm.data)%in%gpargs])
  if(x$debug){
    print(cm.data)
    ##browser()
  }
  text.name <- paste0(
    "directlabels.text.",
    if(is.character(x$method))x$method)
  with(cm.data, grid.text(
    label,x,y,hjust=hjust,vjust=vjust,rot=rot,default.units="cm",
    gp=gp,
    name=text.name))
}

dlgrob <- function
### Make a grid grob that will draw direct labels.
(data,
### Data frame including points to plot in native coordinates.
 method,
### Positioning Method.
 debug=FALSE,
 axes2native=identity,
 ...
 ){
  grob(data=data,method=method,debug=debug,axes2native=axes2native,
       cl="dlgrob",
       name=if(is.character(method)){
         sprintf("GRID.dlgrob.%s",method[1])
       }else{
         "GRID.dlgrob"
       },...)
}

direct.label <- structure(function # Direct labels for color decoding
### Add direct labels to a plot, and hide the color legend. Modern
### plotting packages like lattice and ggplot2 show automatic legends
### based on the variable specified for color, but these legends can
### be confusing if there are too many colors. Direct labels are a
### useful and clear alternative to a confusing legend in many common
### plots.
(p,
### The "trellis" or "ggplot" object with things drawn in different
### colors.
 method=NULL,
### Positioning Method, which determines the positions of the direct
### labels as a function of the plotted data. If NULL, we examine the
### plot p and try to choose an appropriate default. See
### \code{\link{apply.method}} for more information about Positioning
### Methods.
 debug=FALSE
### Show debug output?
 ){
  ##alias<< directlabels
  if(is.character(method)&&method[1]=="legend")
    UseMethod("uselegend")
  else
    UseMethod("direct.label")
### A plot with direct labels and no color legend.
},ex=function(){
  if(require(ggplot2)){
    ## Add direct labels to a ggplot2 scatterplot, making sure that each
    ## label is close to its point cloud, and doesn't overlap points or
    ## other labels.
    scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                     main="Fuel efficiency depends on car size")
    print(direct.label(scatter))
  }

  ## direct labels for lineplots that do not overlap and do not go off
  ## the plot.
  library(nlme)
  library(lattice)
  oldopt <- lattice.options(panel.error=NULL)
  ratplot <-
    xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
  ## Using the default Positioning Method (maxvar.qp), the labels are
  ## placed on the side which is most spread out, so in multipanel
  ## plots they sometimes end up on different sides.
  print(direct.label(ratplot))
  ## To put them on the same side, just manually specify the
  ## Positioning Method.
  print(direct.label(ratplot,"last.qp")) 

  lattice.options(oldopt)
})

default.picker <- function
### Look at options() for a user-defined default Positioning Method
### picker, and use that (or the hard-coded default picker), with the
### calling environment to figure out a good default.
(f
### Object class to look for (trellis or ggplot).
 ){
  varname <- paste("defaultpf.",f,sep="")
  p <- getOption(paste("directlabels.",varname,sep=""))
  if(is.null(p))p <- get(varname)
  do.call(p,as.list(parent.frame()))
}

