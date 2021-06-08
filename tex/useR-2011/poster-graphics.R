library(lattice)
set.seed(1)
loci <- data.frame(score=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.25)),
  type=factor(c(rep("Neutral",800),rep("Positive",100),rep("Balancing",100)))) 
head(loci)
dens <- densityplot(~score,loci,groups=type,auto.key=list(space="top",columns=3),n=500,main=tit <- "Distribution of scores by selection type")
pdf("confusing-legend.pdf",h=4)
print(dens)
dev.off()
library(directlabels)
labeled <- direct.label(dens)
pdf("direct-labeled-density.pdf",h=4)
print(labeled)
dev.off()
library(ggplot2)
theme_set(theme_grey())
pdf("ggplot2-density.pdf",h=4)
ggdensity <- qplot(score,data=loci,color=type,geom="density",main=tit)
print(direct.label(ggdensity))
dev.off()

data(BodyWeight,package="nlme")
ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1),
                  auto.key=list(space="right",points=FALSE,lines=TRUE),
                  main="Rat weight over time by treatment")
pdf("rat.pdf",h=4.5,w=7)
print(ratplot)
dev.off()
pdf("directlabel-rat.pdf",h=4.5,w=7)
print(direct.label(update(ratplot,xlim=c(0,72)),last.qp))
dev.off()
## what is a Positioning Method?
last.points <- function(AllPoints,...){
  Labels <- data.frame()
  PointList <- split(AllPoints,as.character(AllPoints$groups))
  print(PointList)
  for(Points in PointList){
    Labels <- rbind(Labels,subset(Points,x==max(x)))
  }
  data.frame(Labels,hjust=0)
}
endpoints <- function(Points,...){
  subset(Points,x==max(x))
}
## or use gapply
group.endpoints <- function(AllPoints,...){
  gapply(AllPoints,endpoints)
}
highest <- function(Points,...){
  subset(Points,y==max(y))
}
group.highest <- function(Points,...){
  gapply(Points,highest)
}
## or use gapply.fun
last.points <- gapply.fun({
  data.frame(subset(d,x==max(x)),hjust=0)
})
library(lattice)
VADF <- melt(VADeaths)
names(VADF) <- c("age","population","death.rate")
head(VADF)
dp <- xyplot(age~death.rate,VADF,xlim=c(8,84),type="o",groups=population)
dp <- ggplot(VADF)+
  aes(death.rate,age,colour=population)+
  geom_line(aes(group=population))+
  geom_point()+
  xlim(8,84)
pdf("vadeaths.pdf")
print(dp)
dev.off()
direct.label(dp,group.endpoints,TRUE)
pdf("vadeaths-directlabels.pdf")
print(direct.label(dp,list(group.endpoints,rot=30,hjust=-0.05),TRUE))
dev.off()



data(mpg,package="ggplot2")
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
library(lattice)
carpanels <- xyplot(jitter(hwy)~jitter(cty)|manufacturer,mpgf,groups=class,
  main="City and highway fuel efficiency depends on manufacturer and car class")
pdf("cars.pdf",h=10,w=14)
direct.label(carpanels,smart.grid)
dev.off()

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
    r <- y-xscale%*%b # current residual
    ## direction of parameter evolution
    delta <- solve(t(xak)%*%xak)%*%t(xak)%*%r
    ## Current correlations (actually dot product)
    intercept <- apply(xscale,2,function(xk)sum(r*xk))
    ## current rate of change of correlations
    z <- xak%*%delta
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
    subd <- d2[(!d2$variable%in%colnames(x)[j])&d2$alpha>epsilon,]
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

## Calculate lasso path
data(prostate,package="ElemStatLearn")
pros <- subset(prostate,select=-train,train==TRUE)
ycol <- which(names(pros)=="lpsa")
x <- as.matrix(pros[-ycol])
y <- unlist(pros[ycol])
res <- mylars(x,y)
P <- xyplot(coef~arclength,res,groups=variable,type="l",xlim=c(-0.17,2.7),
            main=paste("Direct labeled lasso path visualizes",
              "important predictors of prostate cancer"))
pdf("lasso-labels.pdf")
print(direct.label(P,list(cex=1.5,dl.combine(lasso.labels,last.qp))))
dev.off()

data(diabetes,package="lars")
dres <- with(diabetes,mylars(x,y))
P <- xyplot(coef~arclength,dres,groups=variable,type="l")
plot(direct.label(P,dl.combine(lasso.labels,last.qp)))


set.seed(1)
p <- xyplot(jitter(Sepal.Length)~jitter(Petal.Length),iris,groups=Species,
            main="Fisher's iris data with species labeled")
pdf("iris.pdf")
print(direct.label(p))
dev.off()
