\name{reduce.cex}
\alias{reduce.cex}
\title{reduce cex}
\description{If edges of the text are going out of the plotting
region, then decrease cex until it fits. We call \code{\link{calc.boxes}}
\code{\link{inside}}, so you should set cex before using this.}
\usage{reduce.cex(sides)}
\arguments{
  \item{sides}{string: lr (left and right) or tb (top and bottom).}
}



\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre] (ORCID: <https://orcid.org/0000-0002-3146-0865>)}




\examples{

if(require(lars) && require(ggplot2)){
  data(diabetes,package="lars",envir=environment())
  X <- diabetes$x
  colnames(X) <- paste(colnames(X), colnames(X))
  fit <- lars(X,diabetes$y,type="lasso")
  beta <- scale(coef(fit),FALSE,1/fit$normx)
  arclength <- rowSums(abs(beta))
  path.list <- list()
  for(variable in colnames(beta)){
    standardized.coef <- beta[, variable]
    path.list[[variable]] <-
      data.frame(step=seq_along(standardized.coef),
                 arclength,
                 variable,
                 standardized.coef)
  }
  path <- do.call(rbind, path.list)
  p <- ggplot(path,aes(arclength,standardized.coef,colour=variable))+
    geom_line(aes(group=variable))
  ## the legend isn't very helpful.
  print(p)
  ## add direct labels at the end of the lines.
  direct.label(p, "last.points")
  ## on my screen, some of the labels go off the end, so we can use
  ## this Positioning Method to reduce the text size until the labels
  ## are on the plot.
  direct.label(p, list("last.points",reduce.cex("lr")))
  ## the default direct labels for lineplots are similar.
  direct.label(p)
}

}
