\name{LOPART100}
\alias{LOPART100}
\docType{data}
\title{
  Labeled Optimal Partitioning (LOPART) results
}
\description{
  Results of running LOPART algorithm (for changepoint detection in
  partially labeled data sequence) on a simulated data set of size 100.
}
\usage{data("LOPART100")}
\format{
Named list of data frames: signal has one row per data point, labels has
one row per label, segments has one row per segment, cost has one row
per feasible last changepoint for model up to t=100 data.
}
\source{
  Figure/paper describing LOPART algorithm and R package,
  https://github.com/tdhock/LOPART-paper/blob/master/figure-candidates.R
}
\keyword{datasets}
