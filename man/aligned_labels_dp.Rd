\name{aligned_labels_dp}
\alias{aligned_labels_dp}
\title{aligned labels dp}
\description{Dynamic programming for aligned label positions}
\usage{aligned_labels_dp(target, 
    half.size, B.lo, 
    B.hi)}
\arguments{
  \item{target}{numeric vector of ideal label positions}
  \item{half.size}{numeric vector, half size of each label}
  \item{B.lo}{finite numeric lower bound of plotting area}
  \item{B.hi}{finite numeric upper bound of plotting area}
}

\value{numeric vector of label positions which are closest to targets, but overlapping neither the bounds nor any neighbors.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}





