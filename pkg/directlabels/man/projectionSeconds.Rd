\name{projectionSeconds}
\alias{projectionSeconds}
\docType{data}
\title{
Timings of projection algorithms
}
\description{
Timings of seconds for 3 projection algorithms.
}
\usage{data(projectionSeconds)}
\format{
  A data frame with 603 observations on the following 6 variables.
  \describe{
    \item{\code{vector.length}}{a numeric vector}
    \item{\code{method}}{a factor with levels
      \code{Heap} \code{Random} \code{Sort}}
    \item{\code{mean}}{a numeric vector}
    \item{\code{sd}}{a numeric vector}
    \item{\code{min}}{a numeric vector}
    \item{\code{max}}{a numeric vector}
  }
}
\source{
  Mark Schmidt's prettyPlot code for MATLAB
  http://www.di.ens.fr/~mschmidt/Software/prettyPlot.html
}
\keyword{datasets}
