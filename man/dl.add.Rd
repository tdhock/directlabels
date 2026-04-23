\name{dl.add}
\alias{dl.add}
\title{dl add}
\description{Make a function that adds a constant value to some data columns (typically x,y columns with units in cm).}
\usage{dl.add(...)}
\arguments{
  \item{\dots}{One or more named arguments. Each name is a column to modify, and each value will be added to that column.}
}



\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}




\examples{

set.seed(1)
count.df <- data.frame(people=1:20, Role=rep(c("aut","ctb"),each=10), release=runif(10))
space.cm <- 0.2 # space between polygon point and data point.
geom_dl_poly <- function(position, direction)directlabels::geom_dl(aes(
  label=people, label.group=release),
  method=list(
    paste0(position,".points"),
    cex=0.7, # text size of direct labels.
    dl.add(y=direction*space.cm),
    directlabels::polygon.method(position, offset.cm=0.5)))
if(require("ggplot2")){
  ggplot(count.df, aes(
    release, people, color=Role))+
    geom_line(linewidth=1)+
    geom_point(shape=21, fill="white")+
    scale_y_continuous(limits=c(-5, 25))+
    geom_dl_poly("bottom", -1)+
    geom_dl_poly("top", 1)
}

}
