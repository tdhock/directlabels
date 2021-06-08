data(Chem97,package="mlmRev")
library(lattice)
qqmath(~gcsescore|gender,Chem97,groups=factor(score),
       type=c('l','g'),f.value=ppoints(100))
