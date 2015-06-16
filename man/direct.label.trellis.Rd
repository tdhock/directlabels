\name{direct.label.trellis}
\alias{direct.label.trellis}
\title{direct label trellis}
\description{Add direct labels to a grouped lattice plot. This works by parsing
the trellis object returned by the high level plot function, and
returning it with a new panel function that will plot direct
labels using the specified method.}
\usage{\method{direct.label}{trellis}(p, method = NULL, debug = FALSE)}
\arguments{
  \item{p}{The lattice plot (result of a call to a high-level lattice
function).}
  \item{method}{Method for direct labeling as described in
\code{\link{apply.method}}.}
  \item{debug}{Show debug output?}
}

\value{The lattice plot.}

\author{Toby Dylan Hocking}





