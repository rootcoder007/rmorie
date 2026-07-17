# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/semipar_bridge.R

set.seed(2026L)
x_sp <- sort(runif(40, -2, 2))
y_sp <- sin(x_sp) + 0.2 * rnorm(40)
xe_sp <- seq(-1.5, 1.5, length.out = 20)

test_that("kernel_eval supports all kernel types by name and code", {
  for (nm in c("gaussian", "epanechnikov", "uniform", "triangular", "biweight")) {
    v <- kernel_eval(0, nm)
    expect_true(is.numeric(v))
    expect_true(is.finite(v))
  }
  for (cd in 0:4) {
    expect_true(is.finite(kernel_eval(0, cd)))
  }
  # Unknown kernel name
  expect_error(kernel_eval(0, "weirdkernel"), "Unknown kernel")
  expect_error(kernel_eval(0, list()), "character or integer")
  # Vectorised input
  v <- kernel_eval(c(-1, 0, 1, 2), KERNEL_EPANECHNIKOV)
  expect_length(v, 4L)
  expect_equal(v[4], 0)  # outside support
})

test_that("kernel constants are integer codes 0-4", {
  expect_equal(KERNEL_GAUSSIAN, 0L)
  expect_equal(KERNEL_EPANECHNIKOV, 1L)
  expect_equal(KERNEL_UNIFORM, 2L)
  expect_equal(KERNEL_TRIANGULAR, 3L)
  expect_equal(KERNEL_BIWEIGHT, 4L)
})

test_that("nw_regression produces fitted values", {
  fit <- nw_regression(x_sp, y_sp, xe_sp, bandwidth = 0.5)
  expect_length(fit, length(xe_sp))
  expect_true(all(is.finite(fit)))
})

test_that("nw_regression error paths", {
  expect_error(nw_regression(1:5, 1:6, 1:3, 0.5), "equal length")
  expect_error(nw_regression(1:5, 1:5, 1:3, 0), "bandwidth")
  expect_error(nw_regression(1:5, 1:5, 1:3, -1), "bandwidth")
})

test_that("nw_regression handles tiny bandwidth (zero density branch)", {
  # Very tiny bandwidth where no kernel weight is nonzero -- triggers
  # the den < 1e-300 branch when x_eval points are far from data
  fit <- nw_regression(c(0, 1), c(10, 20), c(1000), bandwidth = 0.0001)
  expect_length(fit, 1L)
})

test_that("local_linear with and without slope", {
  fit <- local_linear(x_sp, y_sp, xe_sp, bandwidth = 0.5)
  expect_length(fit, length(xe_sp))
  fit2 <- local_linear(x_sp, y_sp, xe_sp, bandwidth = 0.5, return_slope = TRUE)
  expect_named(fit2, c("y_hat", "beta_hat"))
  expect_length(fit2$y_hat, length(xe_sp))
  expect_length(fit2$beta_hat, length(xe_sp))
})

test_that("local_linear error paths", {
  expect_error(local_linear(1:5, 1:6, 1:3, 0.5), "equal length")
  expect_error(local_linear(1:5, 1:5, 1:3, 0), "bandwidth")
})

test_that("local_linear degenerate (det ~= 0) branch", {
  # Far evaluation point so weights vanish -> det near zero
  fit <- local_linear(c(0, 0.001), c(1, 2), c(100), bandwidth = 0.01,
                      return_slope = TRUE)
  expect_length(fit$y_hat, 1L)
  expect_length(fit$beta_hat, 1L)
})

test_that("kde works for all kernel types", {
  for (nm in c("gaussian", "epanechnikov", "uniform", "triangular", "biweight")) {
    d <- kde(x_sp, xe_sp, bandwidth = 0.4, kernel_type = nm)
    expect_length(d, length(xe_sp))
    expect_true(all(d >= 0))
  }
})

test_that("kde error path on bandwidth", {
  expect_error(kde(x_sp, xe_sp, 0), "bandwidth")
})

test_that("silverman_bandwidth returns positive scalar", {
  h <- silverman_bandwidth(x_sp)
  expect_true(is.numeric(h) && h > 0)
})

test_that("silverman_bandwidth edge cases", {
  expect_equal(silverman_bandwidth(c(5)), 1.0)  # n < 2
  # All-equal -> IQR=0, falls back to sd_hat which is 0 -> 1.0
  expect_equal(silverman_bandwidth(rep(5, 10)), 1.0)
})

test_that("loocv_bandwidth returns finite positive scalar", {
  h <- loocv_bandwidth(x_sp[1:15], y_sp[1:15], n_grid = 5L)
  expect_true(is.numeric(h) && h > 0)
})

test_that("loocv_bandwidth error path on mismatched lengths", {
  expect_error(loocv_bandwidth(1:5, 1:6), "equal length")
})

test_that("loocv_bandwidth handles user bw_min/bw_max", {
  h <- loocv_bandwidth(x_sp[1:10], y_sp[1:10],
                       bw_min = 0.1, bw_max = 1.0, n_grid = 4L)
  expect_true(h >= 0.1 && h <= 1.0)
  # Inverted range gets fixed
  h2 <- loocv_bandwidth(x_sp[1:10], y_sp[1:10],
                        bw_min = 1.0, bw_max = 0.5, n_grid = 3L)
  expect_true(is.numeric(h2))
})

test_that("kernel_cond_moments returns mean and variance", {
  r <- kernel_cond_moments(x_sp, y_sp, xe_sp, bandwidth = 0.5,
                            return_variance = TRUE)
  expect_named(r, c("mean", "variance"))
  expect_length(r$mean, length(xe_sp))
  expect_length(r$variance, length(xe_sp))
  expect_true(all(r$variance >= 0))
  r2 <- kernel_cond_moments(x_sp, y_sp, xe_sp, bandwidth = 0.5,
                             return_variance = FALSE)
  expect_length(r2, length(xe_sp))
})

test_that("kernel_cond_moments error paths", {
  expect_error(kernel_cond_moments(1:5, 1:6, 1:3, 0.5), "equal length")
  expect_error(kernel_cond_moments(1:5, 1:5, 1:3, 0), "bandwidth")
})

test_that("kernel_cond_moments far-eval zero-weight branch", {
  r <- kernel_cond_moments(c(0, 1), c(10, 20), c(1000),
                           bandwidth = 0.001, return_variance = TRUE)
  expect_equal(r$mean[1], 0)
  expect_equal(r$variance[1], 0)
})

test_that("gam_smoother fits and predicts", {
  skip_if_not_installed("mgcv")
  r <- gam_smoother(x_sp, y_sp, x_eval = xe_sp, k = 5)
  expect_named(r, c("fit", "x_eval", "y_hat", "edf", "k"))
  expect_length(r$y_hat, length(xe_sp))
  expect_true(r$edf > 0)
})

test_that("gam_smoother error path on bad lengths", {
  skip_if_not_installed("mgcv")
  expect_error(gam_smoother(1:5, 1:6), "equal length")
})

test_that("gam_smoother default x_eval = x", {
  skip_if_not_installed("mgcv")
  r <- gam_smoother(x_sp, y_sp, k = 5)
  expect_length(r$y_hat, length(x_sp))
})

test_that("SemiparKernels object exposes methods", {
  obj <- SemiparKernels()
  expect_s3_class(obj, "morie_semipar_kernels")
  expect_equal(obj$backend, "r")
  expect_true(obj$available)
  expect_true(is.function(obj$nw_regression))
  fit <- obj$nw_regression(x_sp, y_sp, xe_sp, bandwidth = 0.5)
  expect_length(fit, length(xe_sp))
  ll <- obj$local_linear(x_sp, y_sp, xe_sp, bandwidth = 0.5)
  expect_named(ll, c("y_hat", "beta_hat"))
  d <- obj$kde(x_sp, xe_sp, bandwidth = 0.4)
  expect_length(d, length(xe_sp))
  h <- obj$silverman_bandwidth(x_sp)
  expect_true(h > 0)
  h2 <- obj$loocv_bandwidth(x_sp[1:10], y_sp[1:10], n_grid = 3L)
  expect_true(h2 > 0)
  cm <- obj$kernel_cond_moments(x_sp, y_sp, xe_sp, bandwidth = 0.5)
  expect_named(cm, c("mean", "variance"))
})

test_that("print.morie_semipar_kernels emits header", {
  obj <- SemiparKernels()
  out <- capture.output(print(obj))
  expect_true(any(grepl("morie SemiparKernels", out)))
  expect_true(any(grepl("backend", out)))
})
