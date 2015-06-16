\name{direct.label}
\alias{direct.label}
\title{Direct labels for color decoding}
\description{Add direct labels to a plot, and hide the color legend. Modern
plotting packages like lattice and ggplot2 show automatic legends
based on the variable specified for color, but these legends can
be confusing if there are too many colors. Direct labels are a
useful and clear alternative to a confusing legend in many common
plots.}
\usage{direct.label(p, method = NULL, debug = FALSE)}
\arguments{
  \item{p}{The "trellis" or "ggplot" object with things drawn in different
colors.}
  \item{method}{Positioning Method, which determines the positions of the direct
labels as a function of the plotted data. If NULL, we examine the
plot p and try to choose an appropriate default. See
\code{\link{apply.method}} for more information about Positioning
Methods.}
  \item{debug}{Show debug output?}
}

\value{A plot with direct labels and no color legend.}

\author{Toby Dylan Hocking}




\examples{
## Add direct labels to a ggplot2 scatterplot, making sure that each
## label is close to its point cloud, and doesn't overlap points or
## other labels.
library(ggplot2)
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
print(direct.label(scatter))

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
}
