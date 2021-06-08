\name{LOPART.ROC}
\alias{LOPART.ROC}
\docType{data}
\title{
  ROC curve for LOPART algorithm and competitors
}
\description{
  For the LOPART paper we computed ROC curves for predictions of
  changepoint detection algorithms.
}
\usage{data("LOPART.ROC")}
\format{
A named list of two data frames: points has one row per
model/algorithm, roc has one row per point on the ROC curve.
}
\source{
  Figure/paper describing LOPART algorithm and R package,
  https://github.com/tdhock/LOPART-paper/blob/master/figure-cv-BIC.R
}
\keyword{datasets}
