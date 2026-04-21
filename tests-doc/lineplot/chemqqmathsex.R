data(Chem97,package="mlmRev")
library(lattice)
qqmath(~gcsescore,Chem97,groups=gender,
       type=c("l","g"),f.value=ppoints(100))
