library(testthat)
if(require(ggplot2)){
  test_that("Nonsyntactic variable names are accepted", {
    mpg2 <- within(mpg, `car class` <- class)
    gg <- ggplot(mpg2, aes(hwy, cty, colour=`car class`)) +
      geom_point()
    p <- directlabels::direct.label(gg)
    expect_is(p, "ggplot")
  })
}
