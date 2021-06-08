data(BodyWeight,package="nlme")
library(lattice)
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',
       layout=c(3,1),xlim=c(-10,75))
