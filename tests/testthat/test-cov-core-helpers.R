# Coverage for small core exports (aaa_dist_native.R Dchisq,
# aaa_helpers_srr_validate.R morie_impute_column, aaa_rangayyan_ar.R
# stats_free_interp, aaa_tail1_core.R MASS_ginv, distributions.R
# morie_dist_quantile), each checked against base R.

test_that("Dchisq is the chi-square density", {
  x <- c(0.3, 1, 2.5, 7)
  expect_equal(Dchisq(x, 3), dchisq(x, 3), tolerance = 1e-12)
  expect_equal(Dchisq(x, 4.5, log = TRUE), dchisq(x, 4.5, log = TRUE), tolerance = 1e-12)
})

test_that("morie_impute_column fills missing values by the chosen rule", {
  x <- c(3, NA, 1, 8, NA, 1)
  expect_equal(morie_impute_column(x)$values, replace(x, is.na(x), median(x, na.rm = TRUE)))
  expect_equal(morie_impute_column(x, "mean")$values, replace(x, is.na(x), mean(x, na.rm = TRUE)))
  expect_equal(morie_impute_column(x, "mode")$values, replace(x, is.na(x), 1))
  expect_equal(morie_impute_column(c(NA, 2, NA, 5, NA), "locf")$values, c(NA, 2, 2, 5, 5))
  f <- morie_impute_column(c("a", NA, "b", "b"), "mode")
  expect_equal(f$values, c("a", "b", "b", "b"))
  expect_equal(f$n_imputed, 1L)
  expect_equal(morie_impute_column(1:3)$n_imputed, 0L)
  expect_error(morie_impute_column(c("a", NA), "mean"), "numeric column")
  expect_error(morie_impute_column(c(NA, NA), "mode"), "entirely missing")
  expect_error(morie_impute_column(x, "x"), "should be one of")
})

test_that("stats_free_interp is piecewise-linear with end-segment extrapolation", {
  b <- c(0, 1, 2.5, 4)
  v <- c(1, 3, 2, 5)
  g <- c(0.5, 1, 2, 3.9)
  expect_equal(stats_free_interp(b, v, g), approx(b, v, g)$y, tolerance = 1e-12)
  out <- stats_free_interp(b, v, c(-1, 5))
  expect_equal(out[1], 1 + (-1 - 0) * (3 - 1) / 1, tolerance = 1e-12)
  expect_equal(out[2], 2 + (5 - 2.5) * (5 - 2) / 1.5, tolerance = 1e-12)
})

test_that("MASS_ginv is the Moore-Penrose inverse", {
  X <- cbind(c(1, 2, 3, 4), c(2, 4, 6, 8), c(1, 0, 1, 0))
  G <- MASS_ginv(X)
  expect_equal(X %*% G %*% X, X, tolerance = 1e-10)
  expect_equal(G %*% X %*% G, G, tolerance = 1e-10)
  expect_equal(t(X %*% G), X %*% G, tolerance = 1e-10)
  expect_equal(MASS_ginv(diag(c(2, 4))), diag(c(0.5, 0.25)), tolerance = 1e-12)
  expect_equal(MASS_ginv(matrix(0, 2, 3)), matrix(0, 3, 2))
  skip_if_not_installed("MASS")
  expect_equal(G, MASS::ginv(X), tolerance = 1e-10)
})

test_that("morie_dist_quantile dispatches to the stats quantile function", {
  p <- c(0.05, 0.5, 0.9)
  expect_equal(morie_dist_quantile(morie_distribution("normal", mean = 1, sd = 2), p),
               qnorm(p, 1, 2), tolerance = 1e-12)
  expect_equal(morie_dist_quantile(morie_distribution("poisson", lambda = 3), p), qpois(p, 3))
  expect_error(morie_dist_quantile(list(), 0.5))
})
