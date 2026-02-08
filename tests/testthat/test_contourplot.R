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

  # Use ..level.. which is the old syntax but likely safer for this test context or the directlabels package versions
  p <- ggplot(volcano3d, aes(x, y, z = z)) +
    stat_contour(aes(colour = ..level..))

  # Check for deprecation warning by class
  # Note: Must print() the ggplot object to trigger the rendering where the method is called
  expect_warning(print(direct.label(p, "bottom.pieces")), class = "deprecatedWarning")
  expect_warning(print(direct.label(p, "top.pieces")), class = "deprecatedWarning")
})
