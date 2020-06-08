\name{dlcompare}
\alias{dlcompare}
\title{Direct label comparison plot}
\description{Compare several \code{plots} and/or label placement methods. This creates
a custom grid graphics display based on lattice and/or ggplot2
output. Plots will be on the columns and positioning methods will
be on the rows.}
\usage{dlcompare(plots, pos.funs, 
    rects = TRUE, row.items = "plots", 
    debug = FALSE)}
\arguments{
  \item{plots}{List of ggplot2 or lattice \code{plots}. List names will be used to
annotate the plot.}
  \item{pos.funs}{List of label placement methods to apply to each plot. List names,
or function names if specified as character strings, will be used
to annotate the plot.}
  \item{rects}{Draw rectangles around each plot, creating a grid?}
  \item{row.items}{If "plots" then put \code{plots} on the rows and method on the
columns. Otherwise, do the opposite.}
  \item{debug}{Show \code{debug} output?}
}



\author{Toby Dylan Hocking}




\examples{
library(lattice)
oldopt <- lattice.options(panel.error=NULL)

## Compare two plots of the same data using lattice and ggplot2.
deaths.by.sex <- list(male=mdeaths, female=fdeaths)
deaths.list <- list()
for(sex in names(deaths.by.sex)){
  deaths.ts <- deaths.by.sex[[sex]]
  deaths.list[[sex]] <-
    data.frame(year=as.numeric(time(deaths.ts)),
               sex,
               deaths=as.integer(deaths.ts))
}
deaths <- do.call(rbind, deaths.list)
death.plot.list <-
  list(lattice=xyplot(deaths~year,deaths,groups=sex,type="l"))
if(require(ggplot2)){
  death.plot.list$ggplot2 <- 
    qplot(year,deaths,data=deaths,colour=sex,geom="line")
}

if(names(dev.cur())!="postscript"){##to avoid error on pkg check.
  ## Use some exotic labeling options with different rotation, font
  ## face, family, and alpha transparency.
  exotic <- list("last.points",
                 rot=c(0,180),
                 fontsize=c(10,20),
                 fontface=c("bold","italic"),
                 fontfamily=c("mono","serif"),
                 alpha=c(0.25,1))
  dlcompare(death.plot.list, list(exotic))
}
  
lattice.options(oldopt)

## Compare a legend with direct labels on the same plot.
library(nlme)
if(require(ggplot2)){
  ggrat <- qplot(Time,weight,data=BodyWeight,
                 colour=Rat,geom="line",facets=.~Diet)
  pfuns <- list("legend","direct labels"="last.qp")
  dlcompare(list(ggrat),pfuns,rects=FALSE,row.items="posfuns")
}
}
