\name{project.onto.segments}
\alias{project.onto.segments}
\title{project onto segments}
\description{Given a point and a set of line segments representing a convex or
alpha hull, calculate the closest point on the segments.}
\usage{project.onto.segments(m, 
    h, debug = FALSE, 
    ...)}
\arguments{
  \item{m}{\code{m} is 1 row, a center of a point cloud, we need to find the
distance to the closest point on each segment of the convex
hull.}
  \item{h}{Data frame describing the line segments of the convex or alpha
hull.}
  \item{debug}{
}
  \item{\dots}{ignored}
}



\author{Toby Dylan Hocking}





