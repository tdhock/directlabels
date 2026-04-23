library(testthat)
library(ggplot2)
test_that("substitute not required", {
  (count.df <- data.frame(people=1:20, Role=rep(c("aut","ctb"),each=10), release=1:10))
  gg <- ggplot(count.df, aes(
    release, people, color=Role))+
    geom_line(linewidth=1)+
    geom_point(shape=21, fill="white")
  gg
  pp <- function(num)sprintf("%d %s", num, ifelse(num==1, "person", "people"))
  space.cm <- 0.2 # space between polygon point and data point.
  ##substitute
  poly.method <- function(position, direction)substitute(list(
    directlabels::dl.trans(
      cex=0.7, # text size of direct labels.
      y=y+YSPACE),
    directlabels::polygon.method(POSITION, offset.cm=0.5)),
    list(YSPACE=direction*space.cm, POSITION=position))
  ##object 'direction' not found
  poly.method <- function(position, direction)list(
    directlabels::dl.trans(
      cex=0.7, # text size of direct labels.
      y=y+direction*space.cm),
    directlabels::polygon.method(position, offset.cm=0.5))
  ##extra direction column
  poly.method <- function(position, direction)list(
    direction=direction,
    directlabels::dl.trans(
      cex=0.7, # text size of direct labels.
      y=y+direction*space.cm),
    directlabels::polygon.method(position, offset.cm=0.5))
  ##anonymous function
  poly.method <- function(position, direction)list(
    cex=0.7, # text size of direct labels.
    function(d, ...)transform(d,y + direction*space.cm),
    directlabels::polygon.method(position, offset.cm=0.5))
  ##new dl.add
  poly.method <- function(position, direction)list(
    cex=0.7, # text size of direct labels.
    dl.add(y=direction*space.cm),
    directlabels::polygon.method(position, offset.cm=0.5))
  dl <- directlabels::direct.label(
    gg, list(dl.add(x=space.cm), "right.polygons"))+
    scale_y_continuous(limits=c(-5, 25))+
    directlabels::geom_dl(aes(
      label=sprintf("%s\n%s", release, pp(people))),
      data=subset(count.df, Role=="ctb"),
      method=poly.method("top", 1))+
    directlabels::geom_dl(aes(
      label=sprintf("%s\n%s", pp(people), release)),
      data=subset(count.df, Role=="aut"),
      method=poly.method("bottom", -1))
  res <- print(dl)
  expect_is(res, "ggplot")
})

test_that("apply method does not replace label", {
  x <- 1:10
  y <- 1:20
  count.df <- data.frame(y, colour=rep(c("red","blue"),each=10), x, groups=x, label.group=x, label=y)
  exp.label.list <- list(
    top.points=11:20,
    bottom.points=1:10)
  comp.label.list <- list()
  for(mname in names(exp.label.list)){
    lab_df <- directlabels::gapply(count.df, mname)
    comp.label.list[[mname]] <- data.frame(
      lab_df[order(lab_df$x),],
      expected=exp.label.list[[mname]])
  }
  comp.label <- do.call(rbind, comp.label.list)
  with(comp.label, expect_identical(label, expected))
})
