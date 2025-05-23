\name{polygon.method}
\alias{polygon.method}
\title{polygon method}
\description{Make a Positioning Method that places non-overlapping speech
polygons at the first or last points.}
\usage{polygon.method(top.bottom.left.right, 
    offset.cm = 0.1, 
    padding.cm = 0.05, 
    custom.colors = NULL)}
\arguments{
  \item{top.bottom.left.right}{Character string indicating what side of the plot to label.}
  \item{offset.cm}{Offset from the polygon to the most extreme data point.}
  \item{padding.cm}{Padding \code{\link{inside}} the polygon.}
  \item{custom.colors}{Positioning method applied just before \code{\link{draw.polygons}}, can set
box.color and text.color for custom colors.}
}



\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}





