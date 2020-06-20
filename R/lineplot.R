### Label points at the zero before the first nonzero y value.
lasso.labels <-
  list(rot=60,
       gapply.fun({ ## figure out where the path hits 0
         d <- d[order(d$x),]
         zero <- d$y[1]
         i <- which(d$y!=zero)[1]
         if(!is.na(i)){
           just <- as.integer(d[i,"y"]>zero)
           transform(d[i-1,],hjust=just,vjust=just)
         }
       }),
       "calc.boxes",
       ## calculate how wide the tilted box is
       dl.trans(hyp=h/sin(2*pi*rot/360)),
       dl.trans(left=x-hyp/2,right=x+hyp/2),
       ## avoid collisions between tilted boxes
       function(d,...){
         solver <- qp.labels("x","left","right")
         ## apply the solver independently for top and bottom labels.
         solution.list <- list()
         for(vj in c(0,1)){
           these <- d$vjust == vj
           if(any(these)){
             one.side <- d[these,]
             solved <- solver(one.side)
             solution.list[[paste(vj)]] <- solved
           }
         }
         do.call(rbind, solution.list)
       })

### Positioning Method for the first of a group of points.
first.points <- label.endpoints(min,1)

### Positioning Method for the last of a group of points.
last.points <- label.endpoints(max,0)

### Positioning Method for the first of a group of points.
left.points <- first.points

### Positioning Method for the last of a group of points.
right.points <- last.points

### Do first or last, whichever has points most spread out.
maxvar.points <- function(d,...){
  myrange <- function(x){
    if(is.factor(x))levels(x)[c(1,nlevels(x))]
    else range(x,na.rm=TRUE)
  }
  vars <- sapply(myrange(d$x),function(v){
    var(d[d$x==v,"y"],na.rm=TRUE)
  })
  FUN <- if(is.na(vars[1]))"last.points"
  else if(is.na(vars[2]))"first.points"
  else if(diff(vars)<0)"first.points" else "last.points"
  apply.method(FUN,d,...)
}

### Label last points, bumping labels up if they collide.
last.bumpup <- list("last.points","bumpup")

### Label first points, bumping labels up if they collide.
first.bumpup <- list("first.points","bumpup")

### Label last points from QP solver that ensures labels do not collide.
last.qp <- vertical.qp("last.points")

### Label first points from QP solver that ensures labels do not collide.
first.qp <- vertical.qp("first.points")

### Draw a speech polygon to the first point.
left.polygons <- polygon.method("left")

### Draw a speech polygon to the last point.
right.polygons <- polygon.method("right")

### Draw a speech polygon to the first point.
first.polygons <- left.polygons

### Draw a speech polygon to the last point.
last.polygons <- right.polygons

### Draw a speech polygon to the top point.
top.polygons <- polygon.method("top")

### Draw a speech polygon to the bottom point.
bottom.polygons <- polygon.method("bottom")

### Label first or last points, whichever are more spread out, and use
### a QP solver to make sure the labels do not collide.
maxvar.qp <- vertical.qp("maxvar.points")

lines2 <- function
### Positioning Method for 2 groups of longitudinal data. One curve
### is on top of the other one (on average), so we label the top one
### at its maximal point, and the bottom one at its minimal
### point. Vertical justification is chosen to minimize collisions
### with the other line. This may not work so well for data with high
### variability, but then again lineplots may not be the best for
### these data either.
(d,
### The data.
 offset=0.3,
### Offset from 0 or 1 for the vjust values.
 ...
### ignored.
 ){
  if(length(unique(d$groups))!=2)
    stop("need 2 groups for lines2")
  top <- 0-offset
  bottom <- 1+offset
  y <- gapply(d,get.means)
  gapply(y,function(D,...){
    bigger.on.average <- D$y==max(y$y)
    f <- if(bigger.on.average)max else min
    compare <- get(if(bigger.on.average)">" else "<")
    is.group <- d$groups==D$groups
    ld    <- d[is.group,]
    other <- d[!is.group,]
    find.closest.y <- function(x){
      closest.x.on.other.line <- which.min(abs(other$x-x))
      other[closest.x.on.other.line,"y"]
    }
    ld$other.yvals <- sapply(ld$x,find.closest.y)
    ld$diff <- abs(ld$y-ld$other.yvals)
    more.extreme <- compare(ld$y,ld$other.yvals)
    ld <- ld[which(more.extreme),] ## which since can have NA
    ld <- ld[ld$y==f(ld$y),]
    which.closest <- which.max(ld$diff)
    pos <- ld[which.closest,]
    transform(pos,vjust=if(bigger.on.average)top else bottom)
  })
}

### Draw a box with the label inside, at the point furthest away from
### the plot border and any other curve.
angled.boxes <-
  list("far.from.others.borders","calc.boxes","enlarge.box","draw.rects")
