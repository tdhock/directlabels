library(directlabels)
data(prostate,package="ElemStatLearn")
pros <- subset(prostate,select=-train,train==TRUE)
ycol <- which(names(pros)=="lpsa")
x <- as.matrix(pros[-ycol])
y <- pros[[ycol]]
library(lars)
fit <- lars(x,y,type="lasso")
beta <- scale(coef(fit),FALSE,1/fit$normx)

## Degenerate case: one 0 curves.
beta[, "gleason"] <- 0
arclength <- rowSums(abs(beta))
library(reshape2)
path <- data.frame(melt(beta),arclength)
names(path)[1:3] <- c("step","variable","standardized.coef")
library(ggplot2)
p <- ggplot(path,aes(arclength,standardized.coef,colour=variable))+
  geom_line(aes(group=variable))+
  ggtitle("LASSO path for prostate cancer data calculated using the LARS")+
  xlim(0,20)
print(direct.label(p,"lasso.labels"))

## Even more degenerate case: all 0 curves.
beta[] <- 0
step <- 1:nrow(beta)
path <- data.frame(melt(beta),step)
names(path)[1:3] <- c("step","variable","standardized.coef")
p <- ggplot(path,aes(step,standardized.coef,colour=variable))+
  geom_line(aes(group=variable))+
  ggtitle("LASSO path for prostate cancer data calculated using the LARS")+
  xlim(0,20)
print(direct.label(p,"lasso.labels"))
  
