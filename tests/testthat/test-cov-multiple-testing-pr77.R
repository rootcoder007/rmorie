# R/multiple_testing.R, the lines PR #77 added that no other test reached: the Storey pi0
# estimator refuses a lambda grid of two or three values and lambdas outside [0, 1), as
# qvalue::pi0est does.

test_that("a short lambda grid and an out-of-range lambda are refused", {
  skip_if_not_installed("qvalue")
  set.seed(1)
  p <- c(stats::runif(80), stats::runif(20, 0, 0.01))
  expect_error(storey_q(p, lambda_param = c(0.2, 0.5)),
               "length(lambda_param) = 2; a lambda grid needs at least 4 values.", fixed = TRUE)
  expect_error(qvalue::pi0est(p, lambda = c(0.2, 0.5)))
  expect_error(storey_q(p, lambda_param = c(0.1, 0.4, 0.7, 1)), "lambda_param must lie in [0, 1).", fixed = TRUE)
  expect_error(qvalue::pi0est(p, lambda = c(0.1, 0.4, 0.7, 1)))
  expect_error(storey_q(p, lambda_param = -0.1), "lambda_param must lie in [0, 1).", fixed = TRUE)
  # an accepted single lambda is the cutoff estimator, qvalue's and by hand
  expect_equal(storey_q(p, lambda_param = 0.5)$pi0, qvalue::pi0est(p, lambda = 0.5)$pi0)
  expect_equal(storey_q(p, lambda_param = 0.5)$pi0, min(1, sum(p >= 0.5) / (100 * 0.5)))
})
