\name{empty.grid}
\alias{empty.grid}
\title{empty grid}
\description{Label placement method for scatterplots that ensures labels are
placed in different places. A grid is drawn over the whole
plot. Each cluster is considered in sequence and assigned to the
point on this grid which is closest to the point given by
the input data points. Makes use of attr(d,"orig.data").}
\usage{empty.grid(d, debug = FALSE, ...)}
\arguments{
  \item{d}{Data frame of target points on the scatterplot for each label.}
  \item{debug}{Show debugging info on the plot?}
  \item{\dots}{ignored.}
}

\value{Data frame with columns groups x y, 1 line for each group, giving
the positions on the grid closest to each cluster.}

\author{Toby Dylan Hocking}





