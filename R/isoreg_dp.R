isoreg_dp <- function(data_vec){
  out.list <- isoreg_dp_interface(data_vec)
  with(out.list, rep(cluster_mean, cluster_size))
}
aligned_labels_dp <- function(target, half.size, B.lo, B.hi){
  N <- length(target)
  if(length(half.size) != N){
    stop("target and half.size should have same length")
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
}
