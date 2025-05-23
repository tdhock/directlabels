\name{dl.combine}
\alias{dl.combine}
\title{Combine output of several methods}
\description{Apply several Positioning methods to the original data frame.}
\usage{dl.combine(...)}
\arguments{
  \item{\dots}{Several Positioning Methods.}
}

\value{A Positioning Method that returns the combined data frame after
applying each specified Positioning Method.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}




\examples{

## Simple example: label the start and endpoints
if(require(nlme) && require(lattice)){
  ratplot <- xyplot(
    weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
  both <- dl.combine("first.points","last.points")
  rat.both <- direct.label(ratplot,"both")
  print(rat.both)
  ## same as repeated call to direct.label:
  rat.repeated <-
    direct.label(direct.label(ratplot,"last.points"),"first.points")
  print(rat.repeated)
}

## same with ggplot2:
if(require(nlme) && require(ggplot2)){
  rp2 <- qplot(
    Time,weight,data=BodyWeight,geom="line",facets=.~Diet,colour=Rat)
  print(direct.label(direct.label(rp2,"last.points"),"first.points"))
  print(direct.label(rp2,"both"))
}

## more complex example: first here is a function for computing the
## lasso path.
mylars <- function
## Least angle regression algorithm for calculating lasso solutions.
(x,
 ## Matrix of predictor variables.
 y,
 ## Vector of responses.
 epsilon=1e-6
 ## If correlation < epsilon, we are done.
 ){
  xscale <- scale(x) # need to work with standardized variables
  b <- rep(0,ncol(x))# coef vector starts at 0
  names(b) <- colnames(x)
  ycor <- apply(xscale,2,function(xj)sum(xj*y))
  j <- which.max(ycor) # variables in active set, starts with most correlated
  alpha.total <- 0
  out <- data.frame()
  while(1){## lar loop
    xak <- xscale[,j] # current variables
    r <- y-xscale\%*\%b # current residual
    ## direction of parameter evolution
    delta <- solve(t(xak)\%*\%xak)\%*\%t(xak)\%*\%r
    ## Current correlations (actually dot product)
    intercept <- apply(xscale,2,function(xk)sum(r*xk))
    ## current rate of change of correlations
    z <- xak\%*\%delta
    slope <- apply(xscale,2,function(xk)-sum(z*xk))
    ## store current values of parameters and correlation
    out <- rbind(out,data.frame(variable=colnames(x),
                                coef=b,
                                corr=abs(intercept),
                                alpha=alpha.total,
                                arclength=sum(abs(b)),
                                coef.unscaled=b/attr(xscale,"scaled:scale")))
    if(sum(abs(intercept)) < epsilon)#corr==0 so we are done
      return(transform(out,s=arclength/max(arclength)))
    ## If there are more variables we can enter into the regression,
    ## then see which one will cross the highest correlation line
    ## first, and record the alpha value of where the lines cross.
    d <- data.frame(slope,intercept)
    d[d$intercept<0,] <- d[d$intercept<0,]*-1
    d0 <- data.frame(d[j[1],])# highest correlation line
    d2 <- data.frame(rbind(d,-d),variable=names(slope))#reflected lines
    ## Calculation of alpha for where lines cross for each variable
    d2$alpha <- (d0$intercept-d2$intercept)/(d2$slope-d0$slope)
    subd <- d2[(!d2$variable\%in\%colnames(x)[j])&d2$alpha>epsilon,]
    subd <- subd[which.min(subd$alpha),]
    nextvar <- subd$variable
    alpha <- if(nrow(subd))subd$alpha else 1
    ## If one of the coefficients would hit 0 at a smaller alpha
    ## value, take it out of the regression and continue.
    hit0 <- xor(b[j]>0,delta>0)&b[j]!=0
    alpha0 <- -b[j][hit0]/delta[hit0]
    takeout <- length(alpha0)&&min(alpha0) < alpha
    if(takeout){
      i <- which.min(alpha0)
      alpha <- alpha0[i]
    }
    b[j] <- b[j]+alpha*delta ## evolve parameters
    alpha.total <- alpha.total+alpha
    ## add or remove a variable from the active set
    j <- if(takeout)j[j!=which(names(i)==colnames(x))]
         else c(j,which(nextvar==colnames(x)))
  }
}

## Calculate lasso path, plot labels at two points: (1) where the
## variable enters the path, and (2) at the end of the path.
if(require(lars) && require(lattice)){
  data(diabetes,envir=environment())
  dres <- with(diabetes,mylars(x,y))
  P <- xyplot(coef~arclength,dres,groups=variable,type="l")
  mylasso <- dl.combine("lasso.labels", "last.qp")
  plot(direct.label(P,"mylasso"))
}

}
