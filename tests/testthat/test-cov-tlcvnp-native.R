# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tlcvnp_native.R (CV-TMLE of a smoothed nonpathwise
# parameter, van der Laan & Rose 2018, Ch. 25). The kernel density at
# x0 is (1/nh) sum K((x - x0)/h) with influence curve K/h - psi; the
# Lepski rule picks the smallest bandwidth consistent with every
# larger one; with one bandwidth and equal folds the CV average is the
# full-sample smoothed parameter.

.cv_x <- c(-1.2, -0.4, 0.1, 0.3, 0.35, 0.8, 1.1, 1.9, -0.05, 0.6, 0.2, -0.7)

test_that("kernel_smooth gives the three kernels", {
  expect_equal(kernel_smooth(0.5), 0.75 * 0.75)
  expect_equal(kernel_smooth(1.5), 0)
  expect_equal(kernel_smooth(0.5, "uniform"), 0.5)
  expect_equal(kernel_smooth(-2, "uniform"), 0)
  expect_equal(kernel_smooth(0.7, "gaussian"), dnorm(0.7), tolerance = 1e-12)
  expect_error(kernel_smooth(0, "triweight"), "kernel must be one of")
})

test_that("smoothed_parameter is the kernel density at x0 with its IC", {
  for (k in c("epanechnikov", "gaussian", "uniform")) {
    kv <- vapply((.cv_x - 0.2) / 0.5, kernel_smooth, 0, kernel = k)
    r <- smoothed_parameter(.cv_x, 0.2, 0.5, k)
    expect_equal(r$psi_h, mean(kv) / 0.5, tolerance = 1e-12)
    expect_equal(r$influence_curve, kv / 0.5 - r$psi_h, tolerance = 1e-12)
    expect_equal(r$se, sd(kv / 0.5) / sqrt(12), tolerance = 1e-12)
  }
  g <- smoothed_parameter(.cv_x, 0.2, 0.5, "gaussian")
  expect_equal(g$psi_h, mean(dnorm(.cv_x, 0.2, 0.5)), tolerance = 1e-12)
  expect_error(smoothed_parameter(.cv_x, 0, 0), "bandwidth must be positive")
})

test_that("smoothing_bias is of order h^s", {
  expect_equal(smoothing_bias(dnorm, 0, 0.3, 2)$bias_order, 0.09)
  expect_error(smoothing_bias(dnorm, 0, -1), "must be positive")
})

test_that("select_bandwidth applies Lepski's rule or the smallest se", {
  hs <- c(0.3, 0.6, 1.2)
  fits <- lapply(hs, function(h) smoothed_parameter(.cv_x, 0.2, h))
  ok <- function(i) all(vapply(seq_along(hs)[seq_along(hs) > i], function(j) {
    abs(fits[[i]]$psi_h - fits[[j]]$psi_h) <= fits[[i]]$se + fits[[j]]$se
  }, TRUE))
  first <- which(vapply(seq_along(hs), ok, TRUE))[1]
  s <- select_bandwidth(.cv_x, 0.2, rev(hs))
  expect_equal(s$h, hs[first])
  expect_equal(s$all$psi_h, vapply(fits, function(f) f$psi_h, 0))
  ss <- select_bandwidth(.cv_x, 0.2, hs, criterion = "smallest_se")
  expect_equal(ss$h, hs[which.min(vapply(fits, function(f) f$se, 0))])
  # a tiny C rejects every smaller bandwidth: the largest is chosen
  expect_equal(select_bandwidth(.cv_x, 0.2, hs, C = 1e-9)$h, 1.2)
  expect_equal(select_bandwidth(.cv_x, 0.2, 0.5)$h, 0.5)
  expect_error(select_bandwidth(.cv_x, 0.2, numeric(0)), "no bandwidths")
  expect_error(select_bandwidth(.cv_x, 0.2, hs, criterion = "aic"), "lepski or smallest_se")
})

test_that("cv_tmle_smoothed averages fold estimates", {
  for (fn in list(cv_tmle_smoothed, morie_tlcvnp)) {
    r <- fn(.cv_x, 0.2, 0.5, V = 3, seed = 4)
    expect_equal(r$psi, smoothed_parameter(.cv_x, 0.2, 0.5)$psi_h, tolerance = 1e-12)
    expect_equal(r$psi, mean(r$fold_estimates), tolerance = 1e-12)
    expect_equal(r$ci, r$psi + c(-1.96, 1.96) * r$se, tolerance = 1e-12)
    expect_equal(r$bandwidths, rep(0.5, 3))
  }
  m <- cv_tmle_smoothed(.cv_x, 0.2, c(0.3, 0.9), V = 4)
  expect_true(all(m$bandwidths %in% c(0.3, 0.9)))
  expect_length(m$fold_estimates, 4L)
})
