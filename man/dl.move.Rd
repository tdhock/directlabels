\name{dl.move}
\alias{dl.move}
\title{Manually move a direct label}
\description{Sometimes there is 1 label that is placed oddly by another
Positioning Function. This function can be used to manually place
that label in a good spot.}
\usage{dl.move(group, x, y, 
    ...)}
\arguments{
  \item{group}{Group to change.}
  \item{x}{Horizontal position of the new label.}
  \item{y}{Vertical position of the new label. If missing(\code{y}) and !missing(\code{x})
then we will calculate a new \code{y} value using linear interpolation.}
  \item{\dots}{Variables to change for the specified \code{group}}
}

\value{A Positioning Function that moves a label into a good spot.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}




\examples{
if(require(ggplot2) && require(lattice)){
  scatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)
  dlcompare(list(scatter),
            list("extreme.grid",
                 `+dl.move`=list(extreme.grid,dl.move("suv",15,15))))
  p <- qplot(log10(gamma),rate,data=svmtrain,group=data,colour=data,
             geom="line",facets=replicate~nu)
  adjust.kif <- dl.move("KIF11",-0.9,hjust=1,vjust=1)
  dlcompare(list(p+xlim(-8,7)),
            list("last.points",
                 `+dl.move`=list(last.points,adjust.kif)))
}
}
