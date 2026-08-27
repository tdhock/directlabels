isoreg_dp <- function
### Dynamic programming for isotonic regression
##alias<< isoreg_dp_interface
(data_vec
### vector of numeric input data
){
  out.list <- isoreg_dp_interface(data_vec)
  with(out.list, rep(cluster_mean, cluster_size))
### For isoreg_dp, vector of numeric output data, same size as input, but containing non-decreasing mean values closest to inputs. For isoreg_dp_interface, list of two numeric vectors: cluster_mean and cluster_size, with Inf/0 at the end for any unused elements.
}

aligned_labels_dp <- function
### Dynamic programming for aligned label positions
(target,
### numeric vector of ideal label positions
  half.size,
### numeric vector, half size of each label
  B.lo,
### finite numeric lower bound of plotting area
  B.hi
### finite numeric upper bound of plotting area
){
  N <- length(target)
  if(any(diff(target)<0)){
    stop("target must be non-decreasing")
  }
  if(length(half.size) != N){
    stop("target and half.size should have same length")
  }
  if(!(is.numeric(B.lo) && length(B.lo)==1 && is.finite(B.lo))){
    stop("B.lo must be finite numeric lower bound")
  }
  if(!(is.numeric(B.hi) && length(B.hi)==1 && is.finite(B.hi))){
    stop("B.hi must be finite numeric upper bound")
  }
  if(B.hi-B.lo < sum(half.size)*2){
    stop("half.size values too large for B limits")
  }
  lo.vec <- cumsum(c(B.lo, half.size[-N]) + half.size)
  diff.target <- target-lo.vec
  diff.opt <- isoreg_dp(diff.target)
  hi <- B.hi-half.size[N]-lo.vec[N]
  diff.bounded <- fcase(
    diff.opt < 0, 0,
    hi < diff.opt, hi,
    default=diff.opt)
  diff.bounded+lo.vec
### numeric vector of label positions which are closest to targets, but overlapping neither the bounds nor any neighbors.
}
