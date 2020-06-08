\name{drawDetails.dlgrob}
\alias{drawDetails.dlgrob}
\title{drawDetails dlgrob}
\description{Process data points using the Positioning Method and draw the
resulting direct labels. This is called for every panel with
direct labels, every time the plot window is resized.}
\usage{\method{drawDetails}{dlgrob}(x, 
    recording)}
\arguments{
  \item{x}{The \code{\link{dlgrob}} list object. x$method should be a Positioning Method
list and x$data should be a data.frame with the following
variables: \describe{
\item{x,y}{numeric horizontal and vertical positions of direct
labels, in native units. These are converted to cm units before
applying the Positioning Method.}
\item{groups}{factor that indices the different groups, and
colour indicates the corresponding group colour.}
\item{hjust and vjust}{(optional) numeric values usually in
[0,1] that control the justification of the text label relative to
the x,y position.}
\item{rot}{(optional) numeric value in [0,360] that specifies
the degrees which the text should be rotated.}
\item{cex, alpha, fontface, fontfamily}{(optional) passed to
gpar.}
} Additionally, x$debug should be set to TRUE or
FALSE, and x$axestonative should be a function that converts units
shown on the axes to native units of x$data[,c("x","y")].}
  \item{recording}{
}
}



\author{Toby Dylan Hocking}





