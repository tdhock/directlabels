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

aligned_labels_qp <- function(){
  ## These are the standard form matrices described in the
  ## directlabels poster.
  target <- d[,target.var]
  k <- nrow(d)
  D <- diag(rep(1,k))
  Ik <- diag(rep(1,k-1))
  A <- rbind(0,Ik)-rbind(Ik,0)
  y.up <- d[,upper.var]
  y.lo <- d[,lower.var]
  b0 <- (y.up-target)[-k] + (target-y.lo)[-1]

  ## limit constraints.
  if(is.function(limits)){
    if(is.finite(l[1])){
      c.vec <- rep(0,k)
      c.vec[1] <- 1
      A <- cbind(A,c.vec)
      b0 <- c(b0,l[1]+target[1]-y.lo[1])
    }
    if(is.finite(l[2])){
      c.vec <- rep(0,k)
      c.vec[k] <- -1
      A <- cbind(A,c.vec)
      b0 <- c(b0,y.up[k]-target[k]-l[2])
    }
  }
    sol <- solve.QP(D,target,A,b0)
    sol$solution
}
