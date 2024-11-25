library(testthat)

test_that("myncurve returns correct values", {
  expect_equal(myncurve(mu = 10, sigma = 5, a = 6), pnorm(6, mean = 10, sd = 5))
})

test_that("myncurve works with a = mu", {
  expect_equal(myncurve(mu = 10, sigma = 5, a = 10), 0.5)
})

test_that("myncurve works with a greater than mu", {
  expect_gt(myncurve(mu = 10, sigma = 5, a = 12), 0.5)
})
