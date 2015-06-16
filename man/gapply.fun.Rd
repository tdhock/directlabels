\name{gapply.fun}
\alias{gapply.fun}
\title{Direct label groups independently}
\description{Makes a function you can use to specify the location of each group
independently.}
\usage{gapply.fun(expr)}
\arguments{
  \item{expr}{Expression that takes a subset of the d data frame, with data from
only a single group, and returns the direct label position.}
}

\value{A Positioning Function.}

\author{Toby Dylan Hocking}




\examples{
complicated <- list(dl.trans(x=x+10),
                    gapply.fun(d[-2,]),
                    rot=c(30,180))
library(lattice)
direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
}
