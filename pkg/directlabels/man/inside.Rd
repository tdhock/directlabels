\name{inside}
\alias{inside}
\title{inside}
\description{Calculate for each box how many \code{points} are inside.}
\usage{inside(boxes, points)}
\arguments{
  \item{boxes}{Data frame of box descriptions, each row is 1 box, need columns
left right top bottom.}
  \item{points}{Data frame of \code{points}, each row is 1 point, need columns x y.}
}

\value{Vector of point counts for each box.}

\author{Toby Dylan Hocking}





