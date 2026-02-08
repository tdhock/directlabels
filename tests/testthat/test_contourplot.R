library(testthat)
library(directlabels)

test_that("contourplot methods are deprecated", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  data(volcano)
  volcano3d <- data.frame(
    x = rep(1:nrow(volcano), ncol(volcano)),
    y = rep(1:ncol(volcano), each = nrow(volcano)),
    z = as.vector(volcano)
  )

  p <- ggplot(volcano3d, aes(x, y, z = z)) +
    stat_contour(aes(colour = after_stat(level)))

  # Check for deprecation warning by class
  expect_warning(direct.label(p, "bottom.pieces"), class = "deprecatedWarning")
  expect_warning(direct.label(p, "top.pieces"), class = "deprecatedWarning")
})
