\name{iris.l1.cluster}
\alias{iris.l1.cluster}
\docType{data}
\title{Clustering of the iris data with the l1 clusterpath}

\description{ The l1 clustering algorithm from the clusterpath package
  was applied to the iris dataset and the breakpoints in the solution
  path are stored in this data frame.}

\usage{data(iris.l1.cluster)}
\format{
  A data frame with 9643 observations on the following 8 variables.
  \describe{
    \item{\code{row}}{a numeric vector: row of the original iris data matrix}
    \item{\code{Species}}{a factor with levels \code{setosa}
      \code{versicolor} \code{virginica}: Species from corresponding row}
    \item{\code{alpha}}{a numeric vector: the value of the optimal
      solution.}
    \item{\code{lambda}}{a numeric vector: the regularization parameter
      (ie point in the path).}
    \item{\code{col}}{a factor with levels \code{Sepal.Length}
      \code{Sepal.Width} \code{Petal.Length} \code{Petal.Width}: column
      from the original iris data.}
    \item{\code{gamma}}{a factor with levels \code{0}: parameter from clustering.}
    \item{\code{norm}}{a factor with levels \code{1} parameter from clustering.}
    \item{\code{solver}}{a factor with levels \code{path} algorithm used for
    clustering.}
  }
}
\source{
clusterpath package
}
\references{
clusterpath article
}
\examples{
data(iris.l1.cluster,package="directlabels")
iris.l1.cluster$y <- iris.l1.cluster$alpha
library(ggplot2)
p <- ggplot(iris.l1.cluster,aes(lambda,y,group=row,colour=Species))+
  geom_line(alpha=1/4)+
  facet_grid(col~.)
p2 <- p+xlim(-0.0025,max(iris.l1.cluster$lambda))
print(direct.label(p2,list(first.points,get.means)))
}
\keyword{datasets}
