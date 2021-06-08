## example from help(stat_contour)
library(reshape2)
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
library(ggplot2)
ggplot(volcano3d, aes(x, y, z = z))+
  stat_contour(aes(colour = ..level..))
