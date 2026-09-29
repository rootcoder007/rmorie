test_that("em_imputation agrees with norm::em.norm", {
  skip_if_not_installed("norm")
  set.seed(31)
  n <- 40
  X <- cbind(rnorm(n), rnorm(n), rnorm(n))
  X[, 2] <- X[, 2] + 0.8 * X[, 1]
  X[, 3] <- X[, 3] - 0.5 * X[, 2]
  X[sample(n, 8), 2] <- NA
  X[sample(n, 6), 3] <- NA
  r <- em_imputation(X, max_iter = 10000, tol = 1e-12)
  s <- norm::prelim.norm(X)
  th <- norm::em.norm(s, criterion = 1e-12, maxits = 10000, showits = FALSE)
  par <- norm::getparam.norm(s, th)
  expect_equal(r$mean, par$mu, tolerance = 1e-7)
  expect_equal(r$cov, par$sigma, tolerance = 1e-7, ignore_attr = TRUE)
})
