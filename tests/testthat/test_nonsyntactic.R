context("Nonsyntactic names")
test_that("Nonsyntactic variable names are accepted", {
  library(ggplot2)
  mpg2 <- within(mpg, `car class` <- class)
  p <- direct.label(ggplot(mpg2, aes(hwy, cty, colour=`car class`)) + geom_point())
  expect_is(p, "ggplot")
})
