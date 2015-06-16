\name{reduce.cex.lr}
\alias{reduce.cex.lr}
\title{reduce cex lr}
\description{If left or right edges of the text are going out of the plotting
region, then decrease cex until it fits. We call calc.boxes
inside, so you should set cex before using this.}
\usage{reduce.cex.lr(d, ...)}
\arguments{
  \item{d}{
}
  \item{\dots}{
}
}



\author{Toby Dylan Hocking}




\examples{
if(require(ElemStatLearn)){
  pros <- subset(prostate,select=-train,train==TRUE)
  ycol <- which(names(pros)=="lpsa")
  x <- as.matrix(pros[-ycol])
  y <- pros[[ycol]]
  library(lars)
  fit <- lars(x,y,type="lasso")
  beta <- scale(coef(fit),FALSE,1/fit$normx)
  arclength <- rowSums(abs(beta))
  library(reshape2)
  path <- data.frame(melt(beta),arclength)
  names(path)[1:3] <- c("step","variable","standardized.coef")
  library(ggplot2)
  p <- ggplot(path,aes(arclength,standardized.coef,colour=variable))+
    geom_line(aes(group=variable))

  ## the legend isn't very helpful.
  print(p)

  ## add direct labels at the end of the lines.
  direct.label(p, "last.points")

  ## on my screen, some of the labels go off the end, so we can use
  ## this Positioning Method to reduce the text size until the labels
  ## are on the plot.
  direct.label(p, list("last.points","reduce.cex.lr"))

  ## the default direct labels for lineplots are similar.
  direct.label(p)
}
}
