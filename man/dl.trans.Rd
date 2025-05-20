\name{dl.trans}
\alias{dl.trans}
\title{Direct label data transform}
\description{Make a function that transforms the data. This is for conveniently
making a function that calls transform on the data frame, with the
arguments provided. See examples.}
\usage{dl.trans(...)}
\arguments{
  \item{\dots}{Arguments to pass to transform.}
}

\value{A Positioning Function.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}




\examples{
complicated <- list(dl.trans(x=x+10),
                    gapply.fun(d[-2,]),
                    rot=c(30,180))
if(require(lattice)){
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
}
}
