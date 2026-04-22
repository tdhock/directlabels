vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
library(ggplot2)
ggplot()+
  geom_point(aes(
    deaths, age, group=demographic, color=demographic),
    data=vad)+
  xlim(8,80)
