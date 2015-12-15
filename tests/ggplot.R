if(require(ggplot2)){
  library(directlabels)
  data(mpg,package="ggplot2")
  plots <-
    list(qplot=qplot(hwy,cty,data=mpg,colour=class),
         ggplot=ggplot(mpg,aes(hwy,cty,colour=class))+geom_point(),
         aes2=ggplot(,aes(hwy,cty))+geom_point(aes(colour=class),data=mpg),
         aes22=ggplot(,aes(colour=class))+geom_point(aes(hwy,cty),data=mpg),
         geom=ggplot()+geom_point(aes(hwy,cty,colour=class),data=mpg),
         mix=ggplot(mpg)+geom_point(aes(hwy,cty,colour=class)),
         mix2=ggplot(,aes(hwy,cty,colour=class))+geom_point(data=mpg))
  for(i in seq_along(plots)){
    p <- plots[[i]]
    p.name <- names(plots)[[i]]
    print(p.name)
    dl <- direct.label(p)
    print(dl)
  }


  ## Test for legend hiding.
  iris0 <- ggplot(iris, aes(Sepal.Width, Petal.Width))+geom_point()+
    geom_density2d(aes(group=Species))
  iris1 <- iris0+aes(colour=Species)
  iris2 <- iris1+aes(size=Petal.Length)
  ## two legends combined:
  iris1leg <- iris1+aes(size=Species)
  iris2leg <- iris2+aes(shape=Species)
  ## only fill:
  data(projectionSeconds, package="directlabels")
  pFill <- ggplot(projectionSeconds, aes(vector.length/1e6, mean))+
    geom_ribbon(aes(ymin=min, ymax=max, 
                    fill=method, group=method), alpha=1/2)
  pShape <- pFill+geom_point(aes(shape=method))
  pLine <- pFill+
    geom_line(aes(group=method, colour=method))
  pLinetype <- pLine+aes(linetype=method)
  pSize <- pLinetype+geom_point(aes(size=sd))
  expect <- function(p, col=NULL, hide=NULL)list(plot=p, colour=col, hide=hide)
  library(nlme)
  rp2 <- qplot(Time,weight,data=BodyWeight,geom="line",facets=.~Diet,colour=Rat)
  to.test <- list(expect(iris0),
                  expect(rp2, "colour", "colour"),
                  expect(iris1, "colour", "colour"),
                  expect(iris2, "colour",  "colour"),
                  expect(iris1leg, "colour", c("colour", "size")),
                  expect(iris2leg, "colour", c("colour", "shape")),
                  expect(pFill, "fill", "fill"),
                  expect(pShape, "fill", c("fill", "shape")),
                  expect(pLine, "colour", c("fill", "colour")),
                  expect(pLinetype, "colour", c("fill", "colour", "linetype")),
                  expect(pSize, "colour", c("fill", "colour", "linetype")))
  for(L in to.test){
    ## Visual check: in the direct labeled plot on the right, is the
    ## colour legend missing? Is the other legend still there?
    
    result <- legends2hide(L$plot)
    if(is.null(result$hide) && is.null(L$hide)){
      ##negative control, ok!
    }else{
      dlcompare(list(L$plot), list("legend", "get.means"))
      
      stopifnot(result$hide %in% L$hide)
      stopifnot(length(result$hide) == length(L$hide))
      stopifnot(L$colour == result$colour)
      ## label it and check for different legends.
      dl <- direct.label(L$plot)
      after <- legends2hide(dl)
      stopifnot(is.null(after))
    }
  }
}
