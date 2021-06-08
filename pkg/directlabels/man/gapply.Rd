\name{gapply}
\alias{gapply}
\title{gapply}
\description{apply a Positioning Method to every group. works like ddply from
plyr package, but the grouping column is always called \code{groups}, and
the Positioning Method is not necessarily a function (but can be).}
\usage{gapply(d, method, ..., 
    groups = "groups")}
\arguments{
  \item{d}{data frame with column \code{groups}.}
  \item{method}{Positioning Method to apply to every group separately.}
  \item{\dots}{additional arguments, passed to Positioning Methods.}
  \item{groups}{can also be useful for piece column.}
}

\value{data frame of results after applying FUN to each group in \code{d}.}

\author{Toby Dylan Hocking}





