library(directlabels)
library(lattice)
library(testthat)
test_that("panel.superpose.dl works", {
  loci <- data.frame(
    ppp=c(rbeta(8,10,10),rbeta(2,0.15,1),rbeta(2,1,0.15)),
    type=factor(c(rep("NEU",8),rep("POS",2),rep("BAL",2))))
  direct.label(densityplot(~ppp,loci,groups=type,n=500))
  computed <- suppressWarnings(with(loci, panel.superpose.dl(
    x=ppp, groups = type, subscripts=seq_along(ppp),
    method="top.bumptwice", panel.groups="panel.densityplot")))
  expect_identical(computed, NULL)
})
