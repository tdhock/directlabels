data(mpg,package="ggplot2")
library(lattice)
xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,
       main="Fuel efficiency depends on car size")
