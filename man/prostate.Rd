\name{prostate}
\alias{prostate}
\docType{data}
%## Example page 47
\title{ Prostate Cancer Data }
\description{
  Data to examine the correlation between the level of prostate-specific
  antigen and a number of clinical measures in men who were about to 
  receive a radical prostatectomy.
}
\usage{data(prostate)}
\format{
  A data frame with 97 observations on the following 10 variables.
  \describe{
    \item{lcavol}{log cancer volume}
    \item{lweight}{log prostate weight}
    \item{age}{in years}
    \item{lbph}{log of the amount of benign prostatic hyperplasia}
    \item{svi}{seminal vesicle invasion}
    \item{lcp}{log of capsular penetration}
    \item{gleason}{a numeric vector}
    \item{pgg45}{percent of Gleason score 4 or 5}
    \item{lpsa}{response}
    \item{train}{a logical vector}
  }
}
\details{
  The last column indicates which 67 observations were used as the 
  "training set" and which 30 as the test set, as described on page 48
  in the book.
}
\note{There was an error in this dataset in earlier versions of the
  package, as indicated in a footnote on page 3 of the second edition of
the book. As of version 2012.04-0 this was corrected.}
\source{ Stamey, T., Kabalin, J., McNeal, J., Johnstone, I.,
    Freiha, F., Redwine, E. and Yang, N (1989) Prostate specific antigen
    in the diagnosis and treatment of adenocarcinoma of the prostate
    II. Radical prostatectomy treted patients, Journall of Urology 16: 1076--1083.
}
\examples{
if(interactive())par(ask=TRUE)
str( prostate )
cor( prostate[,1:8] )
pairs( prostate[,1:9], col="violet" )
train <- subset( prostate, train==TRUE )[,1:9]
test  <- subset( prostate, train=FALSE )[,1:9]
#
if( require(leaps)) {
# The book (page 56) uses only train subset, so we the same:
prostate.leaps <- regsubsets( lpsa ~ . , data=train, nbest=70, #all!
                     really.big=TRUE )
prostate.leaps.sum <- summary( prostate.leaps )
prostate.models <- prostate.leaps.sum$which
prostate.models.size <- as.numeric(attr(prostate.models, "dimnames")[[1]])
hist( prostate.models.size )
prostate.models.rss <- prostate.leaps.sum$rss
prostate.models.best.rss <- 
     tapply( prostate.models.rss, prostate.models.size, min )
prostate.models.best.rss
# Let us add results for the only intercept model
prostate.dummy <- lm( lpsa ~ 1, data=train )
prostate.models.best.rss <- c(
         sum(resid(prostate.dummy)^2), 
          prostate.models.best.rss)
# Making a plot:
plot( 0:8, prostate.models.best.rss, ylim=c(0, 100), 
      type="b", xlab="subset size", ylab="Residual Sum Square", 
      col="red2" )
points( prostate.models.size, prostate.models.rss, pch=17, col="brown",cex=0.7 )
}
# For a better plot, should remove the best for each size from last call!
# Now with ridge regression: 
# Ridge regression in R is multiply implemented, at least:
# MASS: lm.ridge
# mda : gen.ridge
#( survival: ridge)
# Design: pentrace
# mgcv: pcls (very general)
# simple.ridge (in this package)
#
library(mda)
#
prostate.ridge.list <- lapply(list(lambda=seq(0,8,by=0.4)), function(lambda) 
    gen.ridge(train[,1:8], y=train[,9,drop=FALSE],   lambda=lambda))
# Problems with this usage.
# simpler usage:
#
prostate.ridge <- gen.ridge(train[,1:8], y=train[,9,drop=FALSE], lambda=1)
#
# Since there is some problems with the mda functions, we use our own:
#
prostate.ridge <- simple.ridge( train[,1:8], train[,9], df=1:8 )
#
# coefficient traces:
#
matplot( prostate.ridge$df, t(prostate.ridge$beta), type="b", 
        col="blue", pch=17, ylab="coefficients" )
# Calculations for the lasso:
#
if(require(lasso2))  {
prostate.lasso <- l1ce( lpsa ~ ., data=train, trace=TRUE, sweep.out=~1,  
                     bound=seq(0,1,by=0.1) )
prostate.lasso.coef <- sapply(prostate.lasso, function(x) x$coef)
colnames(prostate.lasso.coef) <- seq( 0,1,by=0.1 )
matplot( seq(0,1,by=0.1), t(prostate.lasso.coef[-1,]), type="b", 
        xlab="shrinkage factor", ylab="coefficients", 
        xlim=c(0, 1.2), col="blue", pch=17 )
}
#
# lasso with lars:
if (require(lars)) {
#
prostate.lasso.lars <- lars( as.matrix(train[,1:8]), train[,9], 
         type="lasso", trace=TRUE )
cv.lars( as.matrix(train[,1:8]), train[,9], 
         type="lasso", trace=TRUE, K=10 )
}
#
# CV (cross-validation) using package boot:
#
library(boot)
prostate.glm <- glm( lpsa ~ ., data=train )
# repeat this some times to make clear that cross-validation is
# a random procedure
#
cv.glm( train, prostate.glm, K=10 )$delta                              
#
# This is a two-component vector, raw cross-validated estimate and
#   adjusted cross-validated estimate.
summary( prostate.glm )
#
}
\keyword{datasets}



