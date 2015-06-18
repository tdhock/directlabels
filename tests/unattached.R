if("package:directlabels" %in% search()){
  detach(package:directlabels)
}
library(ggplot2)
no.labels <- ggplot(iris, aes(Petal.Length, Petal.Width, color=Species))+
  geom_point()
with.labels <- no.labels+
  directlabels::geom_dl(aes(label=Species), method="smart.grid")
print(with.labels)
