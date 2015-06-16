\name{dlcompare}
\alias{dlcompare}
\title{Direct label comparison plot}
\description{Compare several plots and/or label placement methods. This creates
a custom grid graphics display based on lattice and/or ggplot2
output. Plots will be on the columns and positioning methods will
be on the rows.}
\usage{dlcompare(plots, pos.funs, rects = TRUE, row.items = "plots", 
    debug = FALSE)}
\arguments{
  \item{plots}{List of ggplot2 or lattice plots. List names will be used to
annotate the plot.}
  \item{pos.funs}{List of label placement methods to apply to each plot. List names,
or function names if specified as character strings, will be used
to annotate the plot.}
  \item{rects}{Draw rectangles around each plot, creating a grid?}
  \item{row.items}{If "plots" then put plots on the rows and method on the
columns. Otherwise, do the opposite.}
  \item{debug}{Show debug output?}
}



\author{Toby Dylan Hocking}




\examples{
library(lattice)
library(ggplot2)
oldopt <- lattice.options(panel.error=NULL)

## Compare two plots of the same data using lattice and ggplot2.
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
require(reshape2)
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
plots <- list(lattice=
              xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-15,80)),
              ggplot2=
              qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))

if(names(dev.cur())!="postscript"){##to avoid error on pkg check Try
  ## Use some exotic labeling options with different rotation, font
  ## face, family, and alpha transparency.
  exotic <- list("last.points",
                 rot=c(0,180),
                 fontsize=c(10,20),
                 fontface=c("bold","italic"),
                 fontfamily=c("mono","serif"),
                 alpha=c(0.25,1))
  dlcompare(plots,list(exotic))
}

## Compare a legend with direct labels on the same plot.
library(nlme)
ggrat <- qplot(Time,weight,data=BodyWeight,
               colour=Rat,geom="line",facets=.~Diet)
pfuns <- list("legend","direct labels"="last.qp")
dlcompare(list(ggrat),pfuns,rects=FALSE,row.items="posfuns")

lattice.options(oldopt)
}
