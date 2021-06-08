library(latticedl)
direct.label(densityplot(~gcsescore,Chem97,groups=factor(score)))


## Easy fix for confusing legend: direct labels
library(latticedl)
long <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
direct.label(long)

## Even works in black and white
longbw <- update(long,par.settings=standard.theme(color=FALSE))
direct.label(longbw)

## Change label positions with the method argument
direct.label(long,method=last.points)

## Make your own positioning function using dl.indep
direct.label(long,method=dl.indep(d[which.max(d$x),]))

## You can change text parameters (same as grid::grid.text)
direct.label(dots2,method=list("last.points",rot=30))

pdf("method.pdf",h=5,w=6)
direct.label(xyplot(age~deaths,vad,groups=demographic,type="l",xlim=c(5,90),ylim=c(1:6)),method=list("last.points",rot=30))
dev.off()
system("evince method.pdf")


## Load some data on car fuel efficiency
data(mpg,package="ggplot2")
head(mpg)

## Plot city versus highway fuel efficiency
xyplot(cty~hwy,mpg,aspect=1)

## Add a reference line x=y
panel.xyref <- function(...){
  panel.xyplot(...)
  panel.abline(0,1)
}
xyplot(cty~hwy,mpg,aspect=1,panel=panel.xyref)

## Jitter the data to see all the points
xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,panel=panel.xyref)

## Group data by number of cylinders in the engine
direct.label(xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,panel=panel.xyref,groups=factor(cyl)))

## Group data by car class
direct.label(xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,panel=panel.xyref,groups=class))

## Compare direct labeling methods
compare.methods(c("empty.grid","empty.grid.2"),xyplot,mpg,jitter(cty)~jitter(hwy),class,aspect=1,panel=panel.xyref,horiz=TRUE)

