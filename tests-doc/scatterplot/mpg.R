library(ggplot2)
data(mpg,package="ggplot2")
ggplot()+
  geom_point(aes(
    jitter(hwy), jitter(cty), colour=class),
    data=mpg)+
  ggtitle("Fuel efficiency depends on car size")
