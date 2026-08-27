library(testthat)
test_that("isotonic dp result increasing", {
  input <- c(4,3,7)
  out <- directlabels::isoreg_dp(input)
  expect_equal(out, c(3.5, 3.5, 7))
})
test_that("isotonic dp interface result", {
  input <- c(4,3,7)
  computed <- directlabels::isoreg_dp_interface(input)
  expected <- list(
    cluster_size=2:0,
    cluster_mean=c(3.5, 7, Inf))
  expect_equal(computed, expected)
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
  out <- directlabels::aligned_labels_dp(c(-10, -10, 9, 10), c(1,2,1,1), -10, 10)
  expect_equal(out, c(-9, -6, 7, 9))
})
test_that("aligned labels error for no data", {
  expect_error({
    directlabels::aligned_labels_dp(c(), c(), -100, 100)
  }, "no data")
})
test_that("aligned labels error for different sizes", {
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:3, -100, 100)
  }, "target and half.size should have same length")
})
test_that("aligned labels error for decreasing data", {
  expect_error({
    directlabels::aligned_labels_dp(1:0, 1:2, -100, 100)
  }, "target must be non-decreasing")
})
test_that("aligned labels error for bad lower bound", {
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, -Inf, 100)
  }, "B.lo must be finite numeric lower bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, numeric(), 100)
  }, "B.lo must be finite numeric lower bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, NA_real_, 100)
  }, "B.lo must be finite numeric lower bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, 1:2, 100)
  }, "B.lo must be finite numeric lower bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, "1", 100)
  }, "B.lo must be finite numeric lower bound")
})
test_that("aligned labels error for bad upper bound", {
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, -100, Inf)
  }, "B.hi must be finite numeric upper bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, -100, numeric())
  }, "B.hi must be finite numeric upper bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, -100, NA_real_)
  }, "B.hi must be finite numeric upper bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, -100, 1:2)
  }, "B.hi must be finite numeric upper bound")
  expect_error({
    directlabels::aligned_labels_dp(1:2, 1:2, -100, "1")
  }, "B.hi must be finite numeric upper bound")
})
