# morie_bayoptr(): the acquisition-chosen Bayesian optimiser

test_that("the documented example runs: subnormal EI values no longer stop optim", {
  # EI is non-negative; in a flat region the closed form rounds to a
  # subnormal of either sign, which made optim(L-BFGS-B) raise
  # 'non-finite value supplied by optim' (R CMD check --run-donttest).
  ei <- .expected_improvement(mu = 40, sd = 1, f_best = 0)
  expect_identical(ei, 0)
  expect_identical(.probability_of_improvement(mu = 40, sd = 1, f_best = 0), 0)
  expect_gt(.expected_improvement(mu = 0, sd = 1, f_best = 1), 0)
  f <- function(x) -sum((x - c(0.3, 0.7))^2)
  res <- morie_bayoptr(f, bounds = list(c(0, 1), c(0, 1)), n_iter = 8L, n_init = 5L,
                       seed = 1L)
  expect_true(is.finite(res$best_y))
  expect_true(all(res$best_x >= 0 & res$best_x <= 1))
  expect_equal(res$best_y, min(res$y))
  expect_length(res$y, 13L)
})
