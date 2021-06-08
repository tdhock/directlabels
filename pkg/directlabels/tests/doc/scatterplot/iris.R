library(lattice)
xyplot(jitter(Sepal.Length)~jitter(Petal.Length),iris,groups=Species)
