\name{normal.l2.cluster}
\alias{normal.l2.cluster}
\docType{data}
\title{
Clustering of some normal data in 2d with the l2 clusterpath
}
\description{
The l2 clustering algorithm from the clusterpath package was applied to
some randomly generated data in 2 dimensions, and the solutions found
using the descent algorithm are stored in this data frame.
}
\usage{data(normal.l2.cluster)}
\format{
  The format is:
List of 2
 $ pts :'data.frame':	320 obs. of  3 variables:
  ..$ class: Factor w/ 8 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
  ..$ x    : num [1:320] -2.73 -3.63 -2.13 -1.27 -2.98 ...
  ..$ y    : num [1:320] -3.89 -3.43 -3.42 -3.17 -2.75 ...
 $ path:Classes 'l2', 'clusterpath' and 'data.frame':	21760 obs. of  7 variables:
  ..$ x     : num [1:21760] -2.73 -3.63 -2.13 -1.27 -2.98 ...
  ..$ y     : num [1:21760] -3.89 -3.43 -3.42 -3.17 -2.75 ...
  ..$ lambda: num [1:21760] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ row   : Factor w/ 320 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
  ..$ gamma : Factor w/ 1 level "0.1": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ norm  : Factor w/ 1 level "2": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ solver: Factor w/ 1 level "descent.nocheck": 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "data")= num [1:320, 1:2] -2.73 -3.63 -2.13 -1.27 -2.98 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:2] "x" "y"
  ..- attr(*, "alphacolnames")= chr [1:2] "x" "y"
  ..- attr(*, "weight.pts")= num [1:320, 1:2] -2.73 -3.63 -2.13 -1.27 -2.98 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:2] "x" "y"
}
\source{
clusterpath package
}
\references{
clusterpath article
}
\examples{
data(normal.l2.cluster)
library(ggplot2)
p <- ggplot(normal.l2.cluster$path,aes(x,y))+
  geom_path(aes(group=row),colour="grey")+
  geom_point(aes(size=lambda),colour="grey")+
  geom_point(aes(colour=class),data=normal.l2.cluster$pts)+
  coord_equal()
print(direct.label(p))
}
\keyword{datasets}
