library(testthat)
context("Nonsyntactic names")
test_that("Nonsyntactic variable names are accepted", {
  library(ggplot2)
  mpg2 <- within(mpg, `car class` <- class)
  gg <- ggplot(mpg2, aes(hwy, cty, colour=`car class`)) +
    geom_point()
  p <- directlabels::direct.label(gg)
  expect_is(p, "ggplot")
})
