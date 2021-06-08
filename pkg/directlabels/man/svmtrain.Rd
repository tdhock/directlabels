\name{svmtrain}
\alias{svmtrain}
\docType{data}
\title{
False positive rates from several 1-SVM models
}
\description{
Support Vector Machine density estimation (1-SVM) was applied to a set
of negative control samples, and then used to test on a positive control.
}
\usage{data(svmtrain)}
\format{
  A data frame with 378 observations on the following 5 variables.
  \describe{
    \item{\code{replicate}}{a factor with levels \code{1} \code{2}
      \code{3}, the experimental replicate. We fit 1-SVM models to each
      replicate separately.}
    \item{\code{rate}}{a numeric vector, the percent of observations
      that were outside the trained model.}
    \item{\code{data}}{a factor with levels \code{KIF11} \code{test}
      \code{train}, which set of observations did we measure. test and
      train are each 50\% random splits of the negative controls in the
      experiment, and KIF11 is the positive control in the experiment.}
    \item{\code{gamma}}{a numeric vector, the tuning parameter of the
      radial basis function kernel.}
    \item{\code{nu}}{a numeric vector, the regularization parameter
      of the 1-SVM.}
  }
}
\keyword{datasets}
