data(normal.l2.cluster,package="directlabels")
library(ggplot2)
ggplot(normal.l2.cluster$path,aes(x,y))+
  geom_path(aes(group=row),colour="grey")+
  geom_point(aes(size=lambda),colour="grey")+
  geom_point(aes(colour=class),data=normal.l2.cluster$pts,pch=21,fill="white")+
  coord_equal()
