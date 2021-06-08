library(directlabels)
data(mpg,package="ggplot2")
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
slab <- direct.label(scatter,list(extreme.grid,dl.move("suv",15,15)))

myridge <- function(f,data,lambda=c(exp(-seq(-15,15,l=200)),0)){
  require(MASS)
  fit <- lm.ridge(f,data,lambda=lambda)
  X <- data[-which(names(data)==as.character(f[[2]]))]
  Xs <- svd(scale(X)) ## my d's should come from the scaled matrix
  dsq <- Xs$d^2
  ## make the x axis degrees of freedom
  df <- sapply(lambda,function(l)sum(dsq/(dsq+l)))
  D <- data.frame(t(fit$coef),lambda,df) # scaled coefs
  molt <- melt(D,id=c("lambda","df"))
  ## add in the points for df=0
  limpts <- transform(subset(molt,lambda==0),lambda=Inf,df=0,value=0)
  rbind(limpts,molt)
}
data(prostate,package="ElemStatLearn")
pros <- subset(prostate,train==TRUE,select=-train)
m <- myridge(lpsa~.,pros)
p <- xyplot(value~df,m,groups=variable,type="o",pch="+",
            panel=function(...){
              panel.xyplot(...)
              panel.abline(h=0)
              panel.abline(v=5,col="grey")
            },
            main="Ridge regression shrinks least squares coefficients",
            ylab="scaled coefficients",
            sub="grey line shows coefficients chosen by cross-validation",
            xlab=expression(df(lambda)))
linelab <- direct.label(update(p,xlim=c(0,9.25)),list(last.smart,cex=0.75,dl.trans(x=x+0.1)))

pdf("figures.pdf",h=6,w=12)
dlcompare(list(slab,linelab),"legend",FALSE)
##dlcompare(list(update(p,auto.key=list(space="right"))),list("legend",last.smart))
dev.off()
##system("xpdf figures.pdf")
