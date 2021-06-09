library(reshape2)
iris2 <- melt(iris,id="Species")
library(lattice)
densityplot(~value|variable,iris2,groups=Species,scales="free")
