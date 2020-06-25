\name{geom_dl}
\alias{geom_dl}
\title{geom dl}
\description{Geom that will plot direct labels.}
\usage{geom_dl(mapping = NULL, 
    data = NULL, ..., 
    method = stop("must specify method= argument"), 
    debug = FALSE, stat = "identity", 
    position = "identity", 
    inherit.aes = TRUE)}
\arguments{
  \item{mapping}{aes(label=variable_that_will_be_used_as_groups_in_Positioning_Methods).}
  \item{data}{data.frame to start with for direct label computation.}
  \item{\dots}{passed to params.}
  \item{method}{Positioning Method for direct label placement, passed to \code{\link{apply.method}}.}
  \item{debug}{Show directlabels debugging output?}
  \item{stat}{passed to layer.}
  \item{position}{passed to layer.}
  \item{inherit.aes}{inherit aes from global ggplot definition?}
}



\author{Toby Dylan Hocking}




\examples{
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
}
