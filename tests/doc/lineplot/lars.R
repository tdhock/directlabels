data(prostate)
pros <- subset(prostate, train==TRUE, -train)
ycol <- which(names(pros)=="lpsa")
x <- as.matrix(pros)[, -ycol]
y <- pros[[ycol]]
fit <- lars::lars(x,y,type="lasso")
beta.mat <- scale(coef(fit),FALSE,1/fit[["normx"]])
arclength <- rowSums(abs(beta.mat))
path <- data.frame(
  arclength,
  variable=colnames(beta.mat)[as.integer(col(beta.mat))],
  standardized.coef=as.numeric(beta.mat),
  step=as.integer(row(beta.mat)))
library(ggplot2)
ggplot(path,aes(arclength,standardized.coef,colour=variable))+
  geom_line(aes(group=variable))+
  ggtitle("LASSO path for prostate cancer data")+
  xlim(-1,21)
