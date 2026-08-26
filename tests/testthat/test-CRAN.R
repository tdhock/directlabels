library(testthat)
test_that("isotonic dp result increasing", {
  input <- c(4,3,7)
  out <- directlabels::isoreg_dp(input)
  expect_equal(out, c(3.5, 3.5, 7))
})
test_that("isotonic dp error for Inf", {
  input <- c(4,Inf,7)
  expect_error({
    directlabels::isoreg_dp(input)
  }, "data must be finite")
})
test_that("aligned labels no diff", {
  input <- c(1,3,7)
  out <- directlabels::aligned_labels_dp(input, c(1,1,1), -100, 100)
  expect_equal(out, input)
})
test_that("aligned labels bounds", {
  out <- directlabels::aligned_labels_dp(c(-10, -9, 9, 10), c(1,1,1,1), -10, 10)
  expect_equal(out, c(-9, -7, 7, 9))
})
test_that("aligned labels error for no data", {
  expect_error({
    directlabels::aligned_labels_dp(c(), c(), -100, 100)
  }, "no data")
})
test_that("aligned labels error for no data", {
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:3, -100, 100)
  }, "target and half.size should have same length")
})
