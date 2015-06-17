### Find the point on each curve which maximizes the distance to the
### plot border or to another curve.
far.from.others.borders <- function(all.groups,...,debug=FALSE){
  group.data <- split(all.groups, all.groups$group)
  group.list <- list()
  for(groups in names(group.data)){
    ## Run linear interpolation to get a set of points on which we
    ## could place the label (this is useful for e.g. the lasso path
    ## where there are only a few points plotted).
    approx.list <- with(group.data[[groups]], approx(x, y))
    if(debug){
      with(approx.list, grid.points(x, y, default.units="cm"))
    }
    group.list[[groups]] <- data.frame(approx.list, groups)
  }
  output <- data.frame()
  for(group.i in seq_along(group.list)){
    one.group <- group.list[[group.i]]
    ## From Mark Schmidt: "For the location of the boxes, I found the
    ## data point on the line that has the maximum distance (in the
    ## image coordinates) to the nearest data point on another line or
    ## to the image boundary."
    dist.mat <- matrix(NA, length(one.group$x), 3)
    colnames(dist.mat) <- c("x","y","other")
    ## dist.mat has 3 columns: the first two are the shortest distance
    ## to the nearest x and y border, and the third is the shortest
    ## distance to another data point.
    for(xy in c("x", "y")){
      xy.vec <- one.group[,xy]
      xy.mat <- rbind(xy.vec, xy.vec)
      lim.fun <- get(sprintf("%slimits", xy))
      diff.mat <- xy.mat - lim.fun()
      dist.mat[,xy] <- apply(abs(diff.mat), 2, min)
    }
    other.groups <- group.list[-group.i]
    other.df <- do.call(rbind, other.groups)
    for(row.i in 1:nrow(dist.mat)){
      r <- one.group[row.i,]
      other.dist <- with(other.df, (x-r$x)^2 + (y-r$y)^2)
      dist.mat[row.i,"other"] <- sqrt(min(other.dist))
    }
    shortest.dist <- apply(dist.mat, 1, min)
    picked <- calc.boxes(one.group[which.max(shortest.dist),])
    ## Mark's label rotation: "For the angle, I computed the slope
    ## between neighboring data points (which isn't ideal for noisy
    ## data, it should probably be based on a smoothed estimate)."
    left <- max(picked$left, min(one.group$x))
    right <- min(picked$right, max(one.group$x))
    neighbors <- approx(one.group$x, one.group$y, c(left, right))
    slope <- with(neighbors, (y[2]-y[1])/(x[2]-x[1]))
    picked$rot <- 180*atan(slope)/pi
    output <- rbind(output, picked)
  }
  output
}

label.endpoints <- function
### Make a Positioning Method that labels a certain x value.
(FUN,
### FUN(d$x) should return an index of which point to label. for
### example you can use which.min or which.max.
 HJUST
### hjust of the labels.
 ){
  stopifnot(is.function(FUN))
  stopifnot(length(HJUST)==1)
  stopifnot(is.numeric(HJUST))
  stopifnot(is.finite(HJUST))
  function(d,...)gapply(d,function(d,...){
    i <- FUN(d$x)==d$x
    if(length(i)==0){
      data.frame()
    }else{
      sub.df <- d[i,]
      if(nrow(sub.df) > 1){
        y.target <- mean(range(sub.df$y))
        sub.df <- sub.df[1,]
        sub.df$y <- y.target
      }
      sub.df$hjust <- HJUST
      sub.df$vjust <- 0.5
      sub.df
    }
  })
### A Positioning Method like first.points or last.points.
}

dl.combine <- structure(function # Combine output of several methods
### Apply several Positioning methods to the original data frame.
(...
### Several Positioning Methods.
 ){
  FUNS <- list(...)
  pf <- function(d,...){
    dfs <- lapply(FUNS,apply.method,d,...)
    res <- data.frame()
    for(df in dfs){
      ## if cex is undefined, we will get NAs which will not be
      ## plotted.
      if(!"cex"%in%names(df)){
        df$cex <- 1
      }
      
      ## we need to do merge to keep all the columns around.
      if(nrow(res))res <- merge(df,res,all=TRUE)
      else res <- df
    }
    res
  }
  pf
### A Positioning Method that returns the combined data frame after
### applying each specified Positioning Method.
},ex=function(){
  ## Simple example: label the start and endpoints
  library(nlme)
  library(lattice)
  ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
  ##ratplot <- qplot(Time,weight,data=BodyWeight,group=Rat,colour=Rat,geom="line",facets=.~Diet)
  both <- dl.combine("first.points","last.points")
  rat.both <- direct.label(ratplot,"both")
  print(rat.both)
  ##   grid.edit(gPath("panel-3-3",".*","GRID.dlgrob"),
  ##             method=list(cex=2,fontfamily="bold","both"),
  ##             grep=TRUE)
  ## can also do this by repeatedly calling direct.label
  rat.repeated <-
    direct.label(direct.label(ratplot,"last.points"),"first.points")
  print(rat.repeated)
  ##   grid.edit(gPath("panel-3-5",".*","GRID.dlgrob.first.points"),
  ##             method=list(cex=2,fontfamily="bold","both"),
  ##             grep=TRUE)
  library(ggplot2)
  rp2 <- qplot(Time,weight,data=BodyWeight,geom="line",facets=.~Diet,colour=Rat)
  print(direct.label(direct.label(rp2,"last.points"),"first.points"))
  print(direct.label(rp2,"both"))

  mylars <- function
  ## Least angle regression algorithm for calculating lasso solutions.
  (x,
   ## Matrix of predictor variables.
   y,
   ## Vector of responses.
   epsilon=1e-6
   ## If correlation < epsilon, we are done.
   ){
    xscale <- scale(x) # need to work with standardized variables
    b <- rep(0,ncol(x))# coef vector starts at 0
    names(b) <- colnames(x)
    ycor <- apply(xscale,2,function(xj)sum(xj*y))
    j <- which.max(ycor) # variables in active set, starts with most correlated
    alpha.total <- 0
    out <- data.frame()
    
    while(1){## lar loop
      xak <- xscale[,j] # current variables
      r <- y-xscale%*%b # current residual
      ## direction of parameter evolution
      delta <- solve(t(xak)%*%xak)%*%t(xak)%*%r
      ## Current correlations (actually dot product)
      intercept <- apply(xscale,2,function(xk)sum(r*xk))
      ## current rate of change of correlations
      z <- xak%*%delta
      slope <- apply(xscale,2,function(xk)-sum(z*xk))
      ## store current values of parameters and correlation
      out <- rbind(out,data.frame(variable=colnames(x),
                                  coef=b,
                                  corr=abs(intercept),
                                  alpha=alpha.total,
                                  arclength=sum(abs(b)),
                                  coef.unscaled=b/attr(xscale,"scaled:scale")))

      if(sum(abs(intercept)) < epsilon)#corr==0 so we are done
        return(transform(out,s=arclength/max(arclength)))
      
      ## If there are more variables we can enter into the regression,
      ## then see which one will cross the highest correlation line
      ## first, and record the alpha value of where the lines cross.
      d <- data.frame(slope,intercept)
      d[d$intercept<0,] <- d[d$intercept<0,]*-1
      d0 <- data.frame(d[j[1],])# highest correlation line
      d2 <- data.frame(rbind(d,-d),variable=names(slope))#reflected lines
      ## Calculation of alpha for where lines cross for each variable
      d2$alpha <- (d0$intercept-d2$intercept)/(d2$slope-d0$slope)
      subd <- d2[(!d2$variable%in%colnames(x)[j])&d2$alpha>epsilon,]
      subd <- subd[which.min(subd$alpha),]
      nextvar <- subd$variable
      alpha <- if(nrow(subd))subd$alpha else 1
      
      ## If one of the coefficients would hit 0 at a smaller alpha
      ## value, take it out of the regression and continue.
      hit0 <- xor(b[j]>0,delta>0)&b[j]!=0
      alpha0 <- -b[j][hit0]/delta[hit0]
      takeout <- length(alpha0)&&min(alpha0) < alpha
      if(takeout){
        i <- which.min(alpha0)
        alpha <- alpha0[i]
      }
      
      b[j] <- b[j]+alpha*delta ## evolve parameters
      alpha.total <- alpha.total+alpha
      ## add or remove a variable from the active set
      j <- if(takeout)j[j!=which(names(i)==colnames(x))]
      else c(j,which(nextvar==colnames(x)))
    }
  }

  ## Calculate lasso path, plot and label
  mylasso <- dl.combine(lasso.labels,last.qp)
  if(require(ElemStatLearn)){
    pros <- subset(prostate,select=-train,train==TRUE)
    ycol <- which(names(pros)=="lpsa")
    x <- as.matrix(pros[-ycol])
    y <- unlist(pros[ycol])
    res <- mylars(x,y)
    P <- xyplot(coef~arclength,res,groups=variable,type="l")
    plot(direct.label(P,"mylasso"))
    p <- ggplot(res,aes(arclength,coef,colour=variable))+
      geom_line(aes(group=variable))
    direct.label(p,"mylasso")
  }

  if(require(lars)){
    data(diabetes,envir=environment())
    dres <- with(diabetes,mylars(x,y))
    P <- xyplot(coef~arclength,dres,groups=variable,type="l")
    plot(direct.label(P,"mylasso"))
  }
})

gapply.fun <- structure(function # Direct label groups independently
### Makes a function you can use to specify the location of each group
### independently.
(expr
### Expression that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  foo <- substitute(expr)
  f <- function(d,...)eval(foo)
  src <- paste("gapply.fun(",paste(deparse(foo),collapse="\n"),")",sep="")
  pf <- structure(function(d,...)gapply(d,f,...),"source"=src)
  pf
### A Positioning Function.
},ex=function(){
  complicated <- list(dl.trans(x=x+10),
                      gapply.fun(d[-2,]),
                      rot=c(30,180))
  library(lattice)
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
})

dl.trans <- structure(function # Direct label data transform
### Make a function that transforms the data. This is for conveniently
### making a function that calls transform on the data frame, with the
### arguments provided. See examples.
(...
### Arguments to pass to transform.
 ){
  L <- as.list(match.call())[-1]
  pf <- function(d,...)do.call("transform",c(list(d),L))
  pf
### A Positioning Function.
},ex=function(){
  complicated <- list(dl.trans(x=x+10),
                      gapply.fun(d[-2,]),
                      rot=c(30,180))
  library(lattice)
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
})

dl.move <- structure(function # Manually move a direct label
### Sometimes there is 1 label that is placed oddly by another
### Positioning Function. This function can be used to manually place
### that label in a good spot.
(group,
### Group to change.
 x,
### Horizontal position of the new label.
 y,
### Vertical position of the new label. If missing(y) and !missing(x)
### then we will calculate a new y value using linear interpolation.
 ...
### Variables to change for the specified group
 ){
  L <- list(...)
  pos <- list()
  if(!missing(x))pos$x <- x
  if(!missing(y))pos$y <- y
  pf <- function(d,...,axes2native){
    native <- axes2native(do.call(data.frame,pos))
    ## first convert user-specified axes units to cm
    for(var in names(pos)){
      u <- unit(native[[var]],"native")
      L[[var]] <- convertUnit(u,"cm",var,"location",var,"location")
    }
    v <- d$groups==group
    for(N in names(L))
      d[v,N] <- L[[N]]
    ## maybe generalize this to be symmetric on x and y one day?
    if("x" %in% names(L) && (!"y" %in% names(L))){
      orig <- attr(d,"orig.data")
      orig <- orig[orig$groups==group,]
      ## do linear interpolation to find a good y-value
      f <- with(orig,approxfun(x,y))
      d[v,"y"] <- f(L$x)
    }
    d
  }
  pf
### A Positioning Function that moves a label into a good spot.
},ex=function(){
  library(ggplot2)
  library(lattice)
  scatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)
  dlcompare(list(scatter),
            list("extreme.grid",
                 `+dl.move`=list(extreme.grid,dl.move("suv",15,15))))

  p <- qplot(log10(gamma),rate,data=svmtrain,group=data,colour=data,
             geom="line",facets=replicate~nu)
  adjust.kif <- dl.move("KIF11",-0.9,hjust=1,vjust=1)
  dlcompare(list(p+xlim(-8,7)),
            list("last.points",
                 `+dl.move`=list(last.points,adjust.kif)))
})

### Jitter the label positions.
dl.jitter <- dl.trans(x=jitter(x),y=jitter(y))

calc.boxes <- function
### Calculate boxes around labels, for collision detection.
(d,
 debug=FALSE,
 ...
 ){
  vp <- current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      pushViewport(vp)
      if(debug)grid.rect() ##highlight current viewport
      w <- conv(stri(as.character(groups[i])),"cm")
      popViewport()
      w
    }))
  }
  ## abs since we have a weird bug with ggplot2 sometimes
  d$w <- abs(convert("Width"))
  d$h <- abs(convert("Height"))
  calc.borders(d)
}

### Calculate big boxes around the means of each cluster.
big.boxes <- list("get.means","calc.boxes","enlarge.box")

### Point halfway between the min and max
midrange <- function(x){
  r <- range(x)
  (r[2]-r[1])/2+r[1]
}

### Point in the middle of the min and max for each group.
visualcenter <- gapply.fun(dl.summarize(d,x=midrange(x),y=midrange(y)))

### Positioning Function for the mean of each cluster of points.
get.means <-
  gapply.fun(dl.summarize(d,x=mean(x),y=mean(y)))

calc.borders <- function
### Calculate bounding box based on newly calculated width and height.
(d,
### Data frame of point labels, with new widths and heights in the w
### and h columns.
 ...
### ignored.
 ){
  for(just in c("hjust","vjust")){
    if(!just %in% names(d)){
      d[,just] <- 0.5
    }
  }
  d$top <- d$y+(1-d$vjust)*d$h
  d$bottom <- d$y-d$vjust*d$h
  d$right <- d$x+(1-d$hjust)*d$w
  d$left <- d$x-d$hjust*d$w
  d
}

### Make a Positioning Method that places non-overlapping speech
### polygons at the first or last points.
polygon.method <- function(method, space, data.col, na.col){
  list(method, "calc.boxes",
       function(d,...){
         for(xy in c("x", "y")){
           d[[sprintf("%s.%s", data.col, xy)]] <- d[[xy]]
           d[[sprintf("%s.%s", na.col, xy)]] <- NA
         }
         d$x <- d$x + space
         d
       },
       "reduce.cex.lr",
       function(d,...){
         d$h <- d$h * 1.5
         d
       },
       "calc.borders",
       qp.labels("y","bottom","top", make.tiebreaker("x","y"), ylimits),
       "calc.borders", draw.polygons)
}

### Draw polygons around label positions.
draw.polygons <- function(d,...){
  stopifnot(c("left.y", "left.x", "right.y", "right.x") %in% names(d))
  if(! "box.color" %in% names(d)){
    d$box.color <- "black"
  }
  for(i in 1:nrow(d)){
    with(d[i,], {
      L <- 
        list(x=c(left.x, left, right, right.x, right, left),
             y=c(left.y, top, top, right.y, bottom, bottom))
      for(xy.name in names(L)){
        xy <- L[[xy.name]]
        L[[xy.name]] <- xy[!is.na(xy)]
      }
      grid.polygon(L$x, L$y,
                   default.units="cm", gp=gpar(col=box.color, fill=colour))
    })
  }
  d$colour <- "white"
  d
}

### Positioning Function that draws boxes around label positions. Need
### to have previously called calc.boxes. Does not edit the data
### frame.
draw.rects <- function(d,...){
  if(is.null(d$box.color))d$box.color <- "black"
  if(is.null(d$fill))d$fill <- "white"
  for(i in 1:nrow(d)){
    with(d[i,],{
      grid.rect(gp = gpar(col = box.color, fill = fill),
                vp = viewport(x, y, w, h, "cm", c(hjust, vjust), angle=rot))
    })
  }
  d
}

### Sequentially bump labels up, starting from the bottom, if they
### collide with the label underneath.
bumpup <- function(d,...){
  ## If there is only 1, then there is no collision detection to do.
  if(nrow(d) == 1)return(d)
  d <- calc.boxes(d)[order(d$y),]
  "%between%" <- function(v,lims)lims[1]<v&v<lims[2]
  obox <- function(x,y){
    tocheck <- with(x,c(left,(right-left)/2+left,right))
    tocheck %between% with(y,c(left,right))
  }
  for(i in 2:nrow(d)){
    dif <- d$bottom[i]-d$top[i-1]
    ## here we are trying to test if box i can possibly collide with
    ## the box below it! Originally we checked if the bottom points of
    ## this box fall in the box below it, but this causes problems
    ## since we are reassigning box positions. If all boxes start at
    ## the same place, 2 will get moved up, 3 will not since its
    ## bottom points are no longer inside box 2. Solution: Look at box
    ## left and right limits and see if they collide!

    ## GOTCHA: If all the boxes are exactly the same size, on top of
    ## each other, then if we only examine left and right points of
    ## each box, none of the boxes will be detected as
    ## overlapping. One way to fix this is change > to >= in %between%
    ## but this is a bad idea since you can have boxes right next to
    ## each other that we don't want to move, that would be detected
    ## as overlapping. Solution: use the midpoint of the box as well!
    overlap <- c(obox(d[i,],d[i-1,]),obox(d[i-1,],d[i,]))
    if(dif < 0 && any(overlap)){
      d$bottom[i] <- d$bottom[i]-dif
      d$top[i] <- d$top[i]-dif
      d$y[i] <- d$y[i]-dif
    }
  }
  d
}

### Remove rows for which either x or y is NA
ignore.na <- function(d,...){
  not.na <- is.finite(d$x)
  if("y"%in% names(d)){
    not.na <- not.na & is.finite(d$y)
  }
  d[not.na,]
}

### If left or right edges of the text are going out of the plotting
### region, then decrease cex until it fits. We call calc.boxes
### inside, so you should set cex before using this.
reduce.cex.lr <- structure(function(d,...){
  d <- calc.boxes(d)
  l <- xlimits()
  positive.part <- function(x)ifelse(x>0,x,0)
  right <- positive.part(d$right-l[2])
  left <- positive.part(l[1]-d$left)
  w <- d$right-d$left
  if(is.null(d$cex)){
    d$cex <- 1
  }
  d$cex <- (w-right)/w * (w-left)/w * d$cex
  calc.boxes(d)
},ex=function(){
  if(require(ElemStatLearn)){
    pros <- subset(prostate,select=-train,train==TRUE)
    ycol <- which(names(pros)=="lpsa")
    x <- as.matrix(pros[-ycol])
    y <- pros[[ycol]]
    library(lars)
    fit <- lars(x,y,type="lasso")
    beta <- scale(coef(fit),FALSE,1/fit$normx)
    arclength <- rowSums(abs(beta))
    library(reshape2)
    path <- data.frame(melt(beta),arclength)
    names(path)[1:3] <- c("step","variable","standardized.coef")
    library(ggplot2)
    p <- ggplot(path,aes(arclength,standardized.coef,colour=variable))+
      geom_line(aes(group=variable))

    ## the legend isn't very helpful.
    print(p)

    ## add direct labels at the end of the lines.
    direct.label(p, "last.points")

    ## on my screen, some of the labels go off the end, so we can use
    ## this Positioning Method to reduce the text size until the labels
    ## are on the plot.
    direct.label(p, list("last.points","reduce.cex.lr"))

    ## the default direct labels for lineplots are similar.
    direct.label(p)
  }
})

qp.labels <- structure(function# Make a Positioning Method for non-overlapping lineplot labels
### Use a QP solver to find the best places to put the points on a
### line, subject to the constraint that they should not overlap.
(target.var,
### Variable name of the label target.
 lower.var,
### Variable name of the lower limit of each label bounding box.
 upper.var,
### Variable name of the upper limit of each label bounding box.
 order.labels=function(d)order(d[,target.var]),
### Function that takes the data.frame of labels and returns an
### ordering, like from the order function. That ordering will be used
### to reorder the rows. This is useful to e.g. break ties when two
### groups have exactly the same value at the endpoint near the label.
 limits=NULL
### Function that takes the data.frame of labels an returns a numeric
### vector of length 2. If finite, these values will be used to add
### constraints to the QP: limits[1] is the lower limit for the first
### label's lower.var, and limits[2] is the upper limit for the last
### labels's upper.var. Or NULL for no limits.
 ){
  ## Reality checks. These also have the side effect of forcing
  ## evaluation of all the arguments in the returned closure.
  stopifnot(is.function(order.labels))
  essential <- list(target.var,upper.var,lower.var)
  for(v in essential){
    stopifnot(is.character(v))
    stopifnot(length(v)==1)
  }
  stopifnot(is.function(limits)||is.null(limits))

  function(d,...){

    ## If there is only 1 label, there is no collision detection to
    ## do, so just return it.
    if(nrow(d)==1)return(d)

    ## Reality checks.
    for(v in essential){
      if(! v %in% names(d)){
        stop("need to have calculated ",v)
      }
    }

    ## sorts data so that target_1 <= target_2 <= ... <= target_n.
    d <- d[order.labels(d),]

    ## check limits to see if there is enough space, given specified
    ## cex.
    if(is.function(limits)){
      l <- limits(d)
      stopifnot(is.numeric(l))
      stopifnot(length(l)==2)
      stopifnot(l[1]<l[2])

      h.available <- l[2] - l[1]
      h <- d[,upper.var]-d[,lower.var]
      h.occupied <- sum(h)
      if(h.occupied > h.available){ ## then the feasible set is empty.
        ## total hack:
        cex <- h.available / h.occupied  * 0.9
        if("cex" %in% names(d)){
          d$cex <- d$cex * cex
        }else{
          d$cex <- cex
        }
        d <- calc.boxes(d)
      }
    }
    
    ## These are the standard form matrices described in the
    ## directlabels poster.
    target <- d[,target.var]
    k <- nrow(d)
    D <- diag(rep(1,k))
    Ik <- diag(rep(1,k-1))
    A <- rbind(0,Ik)-rbind(Ik,0)
    y.up <- d[,upper.var]
    y.lo <- d[,lower.var]
    b0 <- (y.up-target)[-k] + (target-y.lo)[-1]

    ## limit constraints.
    if(is.function(limits)){
      if(is.finite(l[1])){
        c.vec <- rep(0,k)
        c.vec[1] <- 1
        A <- cbind(A,c.vec)
        b0 <- c(b0,l[1]+target[1]-y.lo[1])
      }
      if(is.finite(l[2])){
        c.vec <- rep(0,k)
        c.vec[k] <- -1
        A <- cbind(A,c.vec)
        b0 <- c(b0,y.up[k]-target[k]-l[2])
      }
    }

    ##print(A)
    ##print(b0)
    ##browser()
    sol <- solve.QP(D,target,A,b0)
    d[,target.var] <- sol$solution
    d
  }
### Positioning Method that adjusts target.var so there is no overlap
### of the label bounding boxes, as specified by upper.var and
### lower.var.
},ex=function(){
  SegCost$error <- factor(SegCost$error,c("FP","FN","E","I"))
  library(ggplot2)
  fp.fn.colors <- c(FP="skyblue",FN="#E41A1C",I="black",E="black")
  fp.fn.sizes <- c(FP=2.5,FN=2.5,I=1,E=1)
  fp.fn.linetypes <- c(FP="solid",FN="solid",I="dashed",E="solid")
  err.df <- subset(SegCost,type!="Signal")
  if(!"theme"%in%ls("package:ggplot2")){
    theme <- opts
  }
kplot <- ggplot(err.df,aes(segments,cost))+
  geom_line(aes(colour=error,size=error,linetype=error))+
  facet_grid(type~bases.per.probe)+
  scale_linetype_manual(values=fp.fn.linetypes)+
  scale_colour_manual(values=fp.fn.colors)+
  scale_size_manual(values=fp.fn.sizes)+
  scale_x_continuous(limits=c(0,20),breaks=c(1,7,20),minor_breaks=NULL)+
  theme_bw()+theme(panel.margin=unit(0,"lines"))

  ## The usual ggplot without direct labels.
  print(kplot)

  ## Get rid of legend for direct labels.
  no.leg <- kplot+guides(colour="none",linetype="none",size="none")

  ## Default direct labels.
  direct.label(no.leg)

  ## Explore several options for tiebreaking and limits. First let's
  ## make a qp.labels Positioning Method that does not tiebreak.
  no.tiebreak <- list("first.points",
                      "calc.boxes",
                      qp.labels("y","bottom","top"))
  direct.label(no.leg, no.tiebreak)

  ## Look at the weird labels in the upper left panel. The E curve is
  ## above the FN curve, but the labels are the opposite! This is
  ## because they have the same y value on the first points, which are
  ## the targets for qp.labels. We need to tiebreak.
  qp.break <- qp.labels("y","bottom","top",make.tiebreaker("x","y"))
  tiebreak <- list("first.points",
                   "calc.boxes",
                   "qp.break")
  direct.label(no.leg, tiebreak)

  ## Enlarge the text size and spacing.
  tiebreak.big <- list("first.points",
                       cex=2,
                       "calc.boxes",
                       dl.trans(h=1.25*h),
                       "calc.borders",
                       "qp.break")
  direct.label(no.leg, tiebreak.big)

  ## Even on my big monitor, the FP runs off the bottom of the screen
  ## in the top panels. To avoid that you can specify a limits
  ## function.

  ## Below, the ylimits function uses the limits of each panel, so
  ## labels appear inside the plot region. Also, if you resize your
  ## window so that it is small, you can see that the text size of the
  ## labels is decreased until they all fit in the plotting region.
  qp.limited <-  qp.labels("y","bottom","top",make.tiebreaker("x","y"),ylimits)
  tiebreak.lim <- list("first.points",
                       cex=2,
                       "calc.boxes",
                       dl.trans(h=1.25*h),
                       "calc.borders",
                       "qp.limited")
  direct.label(no.leg, tiebreak.lim)

})


### Make text bounding box larger by some amount.
enlarge.box <- function(d,...){
  if(!"h"%in%names(d))stop("need to have already calculated height and width.")
  calc.borders(within(d,{
    w <- w+h
    h <- h+h
  }))
}

in1which <- function
### Calculate which points fall in a box.
(p,
### data frame of points with columns x and y and many rows.
 box
### data frame of 1 row with columns left right top bottom.
 ){
  p$x>=box$left & p$x<=box$right & p$y<=box$top & p$y>=box$bottom
}

### Calculate how many points fall in a box.
in1box <- function(p,box)sum(in1which(p,box))

### Make a Positioning Method that will, for every piece, select
### points and assign a vjust value.
label.pieces <- function(FUN,VJUST){
  function(d,...){
    processed <- gapply(d,function(d,...)d[FUN(d$y),],groups="piece")
    transform(processed,hjust=0.5,vjust=VJUST)
  }
}

inside <- function
### Calculate for each box how many points are inside.
(boxes,
### Data frame of box descriptions, each row is 1 box, need columns
### left right top bottom.
 points
### Data frame of points, each row is 1 point, need columns x y.
 ){
  sapply(1:nrow(boxes),function(i)in1box(points,boxes[i,]))
### Vector of point counts for each box.
}

dl.summarize <- function
### summarize which preserves important columns for direct labels.
(OLD,
### data frame
 ...
 ){
  rownames(OLD) <- NULL
  NEW <- unique(transform(OLD,...))
  to.copy <- names(OLD)[!names(OLD)%in%names(NEW)]
  for(N in to.copy)
    NEW[,N] <- OLD[,N]
  NEW
}

gapply <- function
### apply a Positioning Method to every group. works like ddply from
### plyr package, but the grouping column is always called groups, and
### the Positioning Method is not necessarily a function (but can be).
(d,
### data frame with column groups.
 method,
### Positioning Method to apply to every group separately.
 ...,
### additional arguments, passed to Positioning Methods.
 groups="groups"
### can also be useful for piece column.
 ){
  stopifnot(is.data.frame(d))
  dfs <- split(d,as.character(d[[groups]]))
  f <- function(d,...){
    res <- apply.method(method,d,columns.to.check=c("x","y"),...)
    res[[groups]] <- d[[groups]][1]
    res
  }
  results <- lapply(dfs,f,...)
  if(any(!sapply(results,is.data.frame))){
    print(results)
    stop("Positioning Method did not return data.frame")
  }
  do.call(rbind,results)
### data frame of results after applying FUN to each group in d.
}

### Label the points furthest from the middle for each group.
extreme.points <- function(d,...){
  d$dist.from.center <- sqrt((d$x-midrange(d$x))^2+(d$y-midrange(d$y))^2)
  gapply(d,function(d,...)d[which.max(d$dist.from.center),])
}

edges.to.outside <- function
### Given a list of edges from the convex or alpha hull, and a list of
### cluster centers, calculate a point near to each cluster on the
### outside of the hull.
(edges,centers,debug=FALSE,...){
  if(debug){
    with(centers,grid.points(x,y,pch="+",default.units="cm"))
    with(edges,grid.segments(x1,y1,x2,y2,default.units="cm"))
  }
  closepts <- gapply(centers,project.onto.segments,edges,debug=debug,...)
  closepts$vjust <- ifelse(closepts$y-centers$y>0,0,1)
  closepts$hjust <- ifelse(closepts$x-centers$x>0,0,1)
  r <- apply.method("big.boxes",closepts)
  r$x <- (r$right-r$left)/2+r$left
  r$y <- (r$top-r$bottom)/2+r$bottom
  r$hjust <- 0.5
  r$vjust <- 0.5
  r
}

### Calculate closest point on the alpha hull with size of the boxes,
### and put it outside that point.
outside.ahull <- function(d,...){
  edges.to.outside(ahull.points(d),visualcenter(d),...)
}

### Calculate closest point on the convex hull and put it outside that
### point. Assume d is the center for each point cloud and then use
### orig.data to calculate hull.
outside.chull <- function(d,...){
  edges.to.outside(chull.points(d),visualcenter(d),...)
}

project.onto.segments <- function
### Given a point and a set of line segments representing a convex or
### alpha hull, calculate the closest point on the segments.
(m,
### m is 1 row, a center of a point cloud, we need to find the
### distance to the closest point on each segment of the convex
### hull.
 h,
### Data frame describing the line segments of the convex or alpha
### hull.
 debug=FALSE,
 ...
### ignored
 ){
  h$s <- (h$y2-h$y1)/(h$x2-h$x1)
  ## the closest point on the line formed by expanding this line
  ## segment (this expression is calculated by finding the minimum
  ## of the distance function).
  h$xstar <- (m$x + m$y*h$s + h$x1*h$s^2 - h$s*h$y1)/(h$s^2+1)
  h$minval <- apply(cbind(h$x1,h$x2),1,min)
  h$maxval <- apply(cbind(h$x1,h$x2),1,max)
  ## xopt is the closest point on the line segment
  h$xopt <- ifelse(h$xstar<h$minval,h$minval,
                   ifelse(h$xstar>h$maxval,h$maxval,h$xstar))
  h$yopt <- h$s*(h$xopt-h$x1)+h$y1
  ## distance to each point on line segment from the center
  h$d <- (m$x-h$xopt)^2+(m$y-h$yopt)^2
  i <- which.min(h$d)
  result <- with(h[i,],data.frame(x=xopt,y=yopt))
  if(debug){
    grid.segments(m$x,m$y,result$x,result$y,default.units="cm")
  }
  result
}

### Make a Positioning Function from a set of points on a vertical
### line that will be spaced out using qp.labels.
vertical.qp <- function(M){
  avoid.collisions <-
    qp.labels("y","bottom","top",make.tiebreaker("x","y"),ylimits)
  list(M,"reduce.cex.lr",avoid.collisions)
}

### Make a tiebreaker function that can be used with qp.labels.
make.tiebreaker <- function(x.var,tiebreak.var){
  force(x.var)
  force(tiebreak.var)
  function(d,...){
    orig <- attr(d,"orig.data")
    xvals <- unique(orig[,x.var])
    x <- unique(d[,x.var])
    if(length(x)>1){
      stop("labels are not aligned")
    }
    xvals <- xvals[order(abs(xvals-x))]
    group.dfs <- split(orig,orig$groups)
    m <- do.call(cbind,lapply(d$groups,function(g){
      df <- group.dfs[[as.character(g)]]
      approx(df[,x.var],df[,tiebreak.var],xvals)$y
    }))
    ## useful for debugging:
    ##print(m)
    L <- lapply(1:nrow(m),function(i)m[i,])
    do.call(order,L)
  }
}

### Calculate the default alpha parameter for ashape based on the
### average size of label boxes.
default.ahull <- function(d,...){
  labels <- apply.method("big.boxes",d,...)
  mean(unlist(labels[,c("w","h")]))
}

### Calculate the points on the ashape.
ahull.points <- function(d,...,ahull=default.ahull(d)){
  require(alphahull)
  xy <- unique(d[,c("x","y")])
  as <- ashape(xy,alpha=ahull)
  as.data.frame(as$edges)
}

### Calculate the points on the convex hull.
chull.points <- function(d,...){
  bpts <- d[with(d,chull(x,y)),]
  r <- data.frame(i1=1:nrow(bpts),i2=c(2:nrow(bpts),1))
  r$x1 <- bpts$x[r$i1]
  r$y1 <- bpts$y[r$i1]
  r$x2 <- bpts$x[r$i2]
  r$y2 <- bpts$y[r$i2]
  r
}

check.for.columns <- function
### Stop if a data.frame does not have some columns.
(d,
### data.frame to check.
 must.have
### column names to check.
 ){
  stopifnot(is.character(must.have))
  for(N in must.have){
    if(! N %in% names(d)){
      stop("data must have a column named ",N)
    }
  }
}

apply.method <- function # Apply a Positioning Method
### Run a Positioning Method list on a given data set. This function
### contains all the logic for parsing a Positioning Method and
### sequentially applying its elements to the input data to obtain the
### label positions.
(method,
### Direct labeling Positioning Method. Starting from the data frame
### of points to plot for the panel, the elements of the Positioning
### Method list are applied in sequence, and then each row of the
### resulting data frame is used to draw a direct label. The
### elements of a Positioning Method list can be
### \itemize{
### \item a Positioning Function is any function(d,...) which takes a
### data.frame d with columns x,y,groups and returns another
### data.frame representing the positions of the desired direct
### labels. For a description of all the columns that are interpreted
### for drawing direct labels, see \code{\link{drawDetails.dlgrob}}.
### For example, maxvar.points is a Positioning Function that returns
### a data.frame with columns x,y,groups,hjust,vjust.
### \item a character vector of length 1 is treated as the name of an
### R object. For example, specifying "maxvar.points" means to look up
### the variable called maxvar.points and use that. Using the name of
### a Positioning Function is preferable to specifying the Positioning
### Function itself, since then the name is visible in the Positioning
### Method list, which is more interpretable when debugging.
### \item a named list element is used to add or update variables in
### the data.frame of direct labels to plot. For example
### list("first.points",cex=1.5) means take only the first points of
### every group and then set the cex column to 1.5.
### \item an element of a Positioning Method list can be another
### Positioning Method list, in which case the elements of the inner
### list are applied.
### }
 d,
### Data frame to which we apply the Positioning Method. The x and y
### columns should be in centimeters (cm), so that Positioning Methods
### can easily calculate the L2/Euclidean/visual distance between
### pairs of points.
 columns.to.check=c("x","y","groups"),
### After applying each Positioning Method list element, we check for
### the presence of these columns, and if not found we stop with an
### error.
 ...,
### Named arguments, passed to Positioning Functions.
 debug=FALSE
### If TRUE, print each Positioning Method list elmenent and the
### direct label data.frame that results from its evaluation.
 ){
  attr(d,"orig.data") <- d ##DONT DELETE: if the first Positioning
                           ##Method needs orig.data, this needs to be
                           ##here!
  check.for.columns(d,columns.to.check)
  if(!is.list(method))method <- list(method)
  isconst <- function(){
    m.var <- names(method)[1]
    !(is.null(m.var)||m.var=="")
  }
  islist <- function()is.list(method[[1]])
  isref <- function()(!isconst())&&is.character(method[[1]])
  while(length(method)){
    if(debug)print(method[1])##not [[1]] --- named items!
    ##browser()
    ## Resolve any names or nested lists
    while(islist()||isref()){
      if(islist()){
        method <- c(method[[1]],method[-1])
      }else{ #must be character -> get the fun(s)
        if(length(method[[1]])>1){
          warning("using first element of character vector")
          method[[1]] <- method[[1]][1]
        }
        method <- c(get(method[[1]]),method[-1])
      }
    }
    if(isconst())
      d[[names(method)[1]]] <- method[[1]]
    else{ #should be a Positioning Function
      old <- d
      group.dfs <- split(d,d$groups)
      group.specific <- lapply(group.dfs,only.unique.vals)
      to.restore <- Reduce(intersect,lapply(group.specific,names))
      d <- method[[1]](d,debug=debug,...)
      check.for.columns(d,columns.to.check)
      ## do not restore if they are present in the returned list!
      to.restore <- to.restore[!to.restore %in% names(d)]
      for(N in to.restore){
        d[[N]] <- NA
        for(g in unique(d$groups)){
          d[d$groups==g,N] <- group.specific[[g]][,N]
        }
      }
      attr(d,"orig.data") <-
        if(is.null(attr(old,"orig.data")))old
        else attr(old,"orig.data")
    }
    if(debug){
      print(d)
    }
    method <- method[-1]
  }
  d
### The final data frame returned after applying all of the items in
### the Positioning Method list, with x and y in units of cm.
}

### Create a 1-row data.frame consisting of only the columns for which
### there is only 1 unique value.
only.unique.vals <- function(d,...){
  unique.vals <- lapply(d,unique)
  n.vals <- sapply(unique.vals,length)
  do.call(data.frame,unique.vals[n.vals==1])
}

### to hard-code label positions...
static.labels <- function(x,y,groups,...){
  L <- list(...)
  force(x)
  force(y)
  force(groups)
  function(d,...,axes2native){
    native <- axes2native(data.frame(x,y))
    L$x <- convertX(unit(native$x,"native"),"cm",valueOnly=TRUE)
    L$y <- convertY(unit(native$y,"native"),"cm",valueOnly=TRUE)
    L$groups <- groups
    do.call(data.frame,L)
  }
}

### Return the positions of the plot vertical limits in cm, for use as
### the limit argument to qp.labels.
ylimits <- function(...){
  convertY(unit(c(0,1),"npc"),"cm",valueOnly=TRUE)
}

### Return the positions of the plot horizontal limits in cm, for use
### as the limit argument to qp.labels.
xlimits <- function(...){
  convertX(unit(c(0,1),"npc"),"cm",valueOnly=TRUE)
}

empty.grid <- function
### Label placement method for scatterplots that ensures labels are
### placed in different places. A grid is drawn over the whole
### plot. Each cluster is considered in sequence and assigned to the
### point on this grid which is closest to the point given by
### the input data points. Makes use of attr(d,"orig.data").
(d,
### Data frame of target points on the scatterplot for each label.
 debug=FALSE,
### Show debugging info on the plot?
 ...
### ignored.
 ){
  NREP <- 10
  orig <- attr(d,"orig.data")
  all.points <- orig[,c("x","y")]
  if(any(table(d$groups)>1))d <- get.means(d)
  label.targets <- d
  ranges <- list(x=convertX(unit(c(0,1),"npc"),"cm",valueOnly=TRUE),
                 y=convertY(unit(c(0,1),"npc"),"cm",valueOnly=TRUE))
  gl <- function(v){
    s <- seq(min(all.points[,v]),max(all.points[,v]),l=NREP)
    if(expand){
      dif <- s[2]-s[1]
      s <- seq(min(ranges[[v]])-expand*dif,
               max(ranges[[v]])+expand*dif,
               l=NREP+2*expand)
    }
    list(centers=s,diff=s[2]-s[1])
  }
  hgrid <- function(x,w){
    hboxes <- floor(diff(ranges[[x]])/r[,w])
    (-expand:(hboxes+expand-1))*r[,w]+r[,w]/2+min(ranges[[x]])
  }
  if(debug)with(label.targets,{
    grid.points(x,y,default.units="cm",gp=gpar(col="green"))
  })
  draw <- function(g){
    gridlines <- with(g,list(x=unique(c(left,right)),y=unique(c(top,bottom))))
    drawlines <- function(a,b,c,d)
      grid.segments(a,b,c,d,"cm",gp=gpar(col="grey"))
    with(gridlines,drawlines(min(x),y,max(x),y))
    with(gridlines,drawlines(x,min(y),x,max(y)))
  }
  res <- data.frame()
  label.targets <-
    label.targets[order(nchar(as.character(label.targets$groups))),]
  for(v in label.targets$groups){
    r <- label.targets[label.targets$groups==v,]
    no.points <- data.frame()
    expand <- 0
    while(nrow(no.points)==0){
      boxes <- if("left"%in%names(label.targets)){
        list(x=hgrid("x","w"),y=hgrid("y","h"),w=r$w,h=r$h)
      }else{
        L <- sapply(c("x","y"),gl,simplify=FALSE)
        list(x=L$x$centers,y=L$y$centers,w=L$x$diff,h=L$y$diff)
      }
      boxes <- calc.borders(do.call(expand.grid,boxes))
      boxes <- cbind(boxes,data=inside(boxes,all.points))
      no.points <- transform(subset(boxes,data==0))
      expand <- expand+1 ## look further out if we can't find any labels inside
    }
    if(debug)draw(boxes)
    
    ## TDH 29 Aug 2012. For every box, figure out the class of the
    ## point which is its nearest neighbor.
    no.points$nearest <- NA
    for(i in 1:nrow(no.points)){
      b <- no.points[i,]
      d.orig <- with(orig,(b$x-x)^2+(b$y-y)^2)
      no.points[i,"nearest"] <- as.character(orig$groups[which.min(d.orig)])
    }
    ## Only consider boxes that are closest to this class.
    closest <- no.points[no.points$nearest == rownames(r),]
    if(nrow(closest) == 0){
      closest <- no.points
    }
    closest$len <- with(closest,(r$x-x)^2+(r$y-y)^2)
    best <- closest[closest$len == min(closest$len), ][1, ]

    res <- rbind(res,transform(r,x=best$x,y=best$y))
    ## add points to cloud
    newpts <- with(best,{
      expand.grid(x=seq(left,right,l=3),
                  y=seq(top,bottom,l=3))
    })
    all.points <- rbind(all.points,newpts)
  }
  if(debug)with(all.points,grid.points(x,y,default.units="cm"))
  res
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}

