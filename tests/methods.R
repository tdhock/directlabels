library(directlabels)
data(BodyWeight, package="nlme")
library(lattice)
oldopt <- lattice.options(panel.error=NULL)
p <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',
            layout=c(3,1),xlim=c(-10,75))
stop.if.same.colour <- function(d, ...){
  col.tab <- table(d$colour)
  if(1 == length(col.tab)){
    print(d)
    stop("only one colour")
  }
  d
}
dl <- direct.label(p, list("angled.boxes", "stop.if.same.colour"))
print(dl)
lattice.options(oldopt)
