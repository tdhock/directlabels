library(ggplot2)
data(mpg,package="ggplot2")
qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
      main="Fuel efficiency depends on car size")
