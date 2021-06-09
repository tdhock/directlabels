data(Chem97,package="mlmRev")
library(lattice)
densityplot(~gcsescore|gender,Chem97,
            groups=factor(score),layout=c(1,2),
            n=500,plot.points=FALSE)
