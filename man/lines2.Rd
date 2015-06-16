\name{lines2}
\alias{lines2}
\title{lines2}
\description{Positioning Method for 2 groups of longitudinal data. One curve
is on top of the other one (on average), so we label the top one
at its maximal point, and the bottom one at its minimal
point. Vertical justification is chosen to minimize collisions
with the other line. This may not work so well for data with high
variability, but then again lineplots may not be the best for
these data either.}
\usage{lines2(d, offset = 0.3, ...)}
\arguments{
  \item{d}{The data.}
  \item{offset}{Offset from 0 or 1 for the vjust values.}
  \item{\dots}{ignored.}
}



\author{Toby Dylan Hocking}





