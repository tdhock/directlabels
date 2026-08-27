\name{isoreg_dp}
\alias{isoreg_dp}
\alias{isoreg_dp_interface}
\title{isoreg dp}
\description{Dynamic programming for isotonic regression}
\usage{isoreg_dp(data_vec)}
\arguments{
  \item{data_vec}{vector of numeric input data}
}

\value{For isoreg_dp, vector of numeric output data, same size as input, but containing non-decreasing mean values closest to inputs. For \code{\link{isoreg_dp_interface}}, list of two numeric vectors: cluster_mean and cluster_size, with Inf/0 at the end for any unused elements.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}





