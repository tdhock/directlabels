\name{qp.labels}
\alias{qp.labels}
\title{Make a Positioning Method for non-overlapping lineplot labels}
\description{Use a QP solver to find the best places to put the points on a
line, subject to the constraint that they should not overlap.}
\usage{qp.labels(target.var, lower.var, upper.var, order.labels = function(d) order(d[, 
    target.var]), limits = NULL)}
\arguments{
  \item{target.var}{Variable name of the label target.}
  \item{lower.var}{Variable name of the lower limit of each label bounding box.}
  \item{upper.var}{Variable name of the upper limit of each label bounding box.}
  \item{order.labels}{Function that takes the data.frame of labels and returns an
ordering, like from the order function. That ordering will be used
to reorder the rows. This is useful to e.g. break ties when two
groups have exactly the same value at the endpoint near the label.}
  \item{limits}{Function that takes the data.frame of labels an returns a numeric
vector of length 2. If finite, these values will be used to add
constraints to the QP: limits[1] is the lower limit for the first
label's lower.var, and limits[2] is the upper limit for the last
labels's upper.var. Or NULL for no limits.}
}

\value{Positioning Method that adjusts target.var so there is no overlap
of the label bounding boxes, as specified by upper.var and
lower.var.}

\author{Toby Dylan Hocking}




\examples{
  SegCost$error <- factor(SegCost$error,c("FP","FN","E","I"))
  library(ggplot2)
  fp.fn.colors <- c(FP="skyblue",FN="#E41A1C",I="black",E="black")
  fp.fn.sizes <- c(FP=2.5,FN=2.5,I=1,E=1)
  fp.fn.linetypes <- c(FP="solid",FN="solid",I="dashed",E="solid")
  err.df <- subset(SegCost,type!="Signal")
  if(!"theme"\%in\%ls("package:ggplot2")){
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

}
