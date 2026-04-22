library(testthat)
library(ggplot2)
library(data.table)
test_that("substitute not required", {
  (count.dt <- data.table(people=1:20, Role=rep(c("aut","ctb"),each=10), release=1:10))
  gg <- ggplot(count.dt, aes(
    release, people, color=Role))+
    geom_line(linewidth=1)+
    geom_point(shape=21, fill="white")
  gg
  pp <- function(num)sprintf("%d %s", num, ifelse(num==1, "person", "people"))
  space.cm <- 0.2 # space between polygon point and data point.
  poly.method <- function(position, direction)substitute(list(
    directlabels::dl.trans(
      cex=0.7, # text size of direct labels.
      y=y+YSPACE),
    directlabels::polygon.method(
      POSITION, offset.cm=0.5)), #space between polygon point and text.
    list(YSPACE=direction*space.cm, POSITION=position))
  poly.method <- function(position, direction)list(
    directlabels::dl.trans(
      cex=0.7, # text size of direct labels.
      y=y+direction*space.cm),
    directlabels::polygon.method(
      position, offset.cm=0.5))
  dl <- directlabels::direct.label(
    gg, list(directlabels::dl.trans(x=x+space.cm), "right.polygons"))+
    scale_y_continuous(limits=c(-5, 25))+
    directlabels::geom_dl(aes(
      label=sprintf("%s\n%s", release, pp(people))),
      data=count.dt[Role=="ctb"],
      method=poly.method("top", 1))+
    directlabels::geom_dl(aes(
      label=sprintf("%s\n%s", pp(people), release)),
      data=count.dt[Role=="aut"],
      method=poly.method("bottom", -1))
  res <- print(dl)
  expect_is(res, "ggplot")
})
