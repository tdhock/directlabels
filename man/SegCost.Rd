\name{SegCost}
\alias{SegCost}
\docType{data}
\title{
  Cost of segmentation models
}
\description{
20 segmentation models were fit to 2 simulated signals, and several
different error measures were used to quantify the model fit.
}
\usage{data(SegCost)}
\format{
  A data frame with 560 observations on the following 5 variables.
  \describe{
    \item{\code{bases.per.probe}}{a factor with levels \code{374}
      \code{7}: the sampling density of the signal.}
    \item{\code{segments}}{numeric: the model complexity
      measured using number of segments.}
    \item{\code{cost}}{numeric: the cost value.}
    \item{\code{type}}{a factor with levels \code{Signal}
      \code{Breakpoint} \code{Complete} \code{Incomplete}
      \code{Positive}: how to judge model fit? Signal: log mean squared
      error between latent signal and estimated signal. Breakpoint:
      exact breakpoint error. Complete: annotation error with a complete
    set of annotations. Incomplete: annotation error with only half of
    those annotations. Positive: no negative annotations.}
    \item{\code{error}}{a factor with levels \code{E} \code{FP}
      \code{FN} \code{I}: what kind of error? FP = False
      Positive, FN = False Negative, I = Imprecision, E = Error
      (sum of the other terms).}
  }
}
\source{
  PhD thesis of Toby Dylan Hocking, chapter Optimal penalties for
  breakpoint detection using segmentation model selection.
}
\keyword{datasets}
