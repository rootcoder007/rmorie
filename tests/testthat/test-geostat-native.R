# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 19 — structural tests for the native geostatistics engines.
#' @srrstats {G5.6} Variogram ML recovers simulated covariance
#'   parameters; kriging is an exact interpolator at observed sites.

.sim_gp <- function(n = 150, nugget = 0.1, psill = 1, range_ = 0.3,
                    seed = 70) {
  set.seed(seed)
  xy <- cbind(runif(n), runif(n))
  D <- as.matrix(dist(xy))
  C <- psill * exp(-D / range_) + diag(nugget, n)
  # Escalating jitter: older LAPACK builds (oldrel CI) reject this
  # matrix as non-PD at machine precision even though it is PD.
  jit <- 1e-10
  L <- NULL
  while (is.null(L) && jit < 1e-2) {
    L <- tryCatch(chol(C + jit * diag(n)), error = function(e) NULL)
    jit <- jit * 100
  }
  list(xy = xy, y = as.numeric(t(L) %*% rnorm(n)) + 5)
}

test_that("empirical variogram increases with distance for a GP", {
  g <- .sim_gp(n = 200, seed = 71)
  vg <- morie_spatial_variogram(g$xy, g$y)
  expect_s3_class(vg, "data.frame")
  expect_true(all(vg$np > 0))
  # short-range semivariance below long-range
  expect_lt(mean(vg$gamma[1:3]), mean(vg$gamma[(nrow(vg) - 2):nrow(vg)]))
})

test_that("variogram ML recovers exponential parameters", {
  g <- .sim_gp(n = 220, nugget = 0.1, psill = 1, range_ = 0.3, seed = 72)
  fit <- morie_spatial_variogram_fit(g$xy, g$y, model = "exponential")
  # Nugget/range are weakly identified at n = 220; assert the fit is
  # in the right regime rather than pinning noisy point estimates.
  expect_true(is.finite(fit$loglik))
  expect_gt(fit$range, 0.05); expect_lt(fit$range, 1.5)
  expect_lt(fit$nugget, 0.7)
  expect_equal(fit$nugget + fit$psill, 1.1, tolerance = 0.5)
  # spatial model beats a pure-nugget (white noise) Gaussian loglik
  ll_white <- sum(stats::dnorm(g$y, mean(g$y), stats::sd(g$y),
                               log = TRUE))
  expect_gt(fit$loglik, ll_white)
})

test_that("ordinary kriging: near-exact at data sites, sane elsewhere", {
  g <- .sim_gp(n = 150, nugget = 0.01, seed = 73)
  fit <- morie_spatial_variogram_fit(g$xy, g$y)
  kr <- morie_spatial_krige(g$xy, g$y, g$xy[1:10, , drop = FALSE],
                            vgm = fit)
  expect_equal(kr$pred, g$y[1:10], tolerance = 0.15)
  # interior prediction within the data range with positive variance
  kr2 <- morie_spatial_krige(g$xy, g$y, cbind(0.5, 0.5), vgm = fit)
  expect_true(kr2$pred > min(g$y) && kr2$pred < max(g$y))
  expect_gte(kr2$var, 0)
  # kriging variance grows far from the data
  kr3 <- morie_spatial_krige(g$xy, g$y, cbind(5, 5), vgm = fit)
  expect_gt(kr3$var, kr2$var)
})

test_that("native GWR recovers a spatially varying coefficient", {
  set.seed(74)
  n <- 300
  xy <- cbind(runif(n), runif(n))
  beta <- 1 + 2 * xy[, 1]           # coefficient varies west -> east
  x <- rnorm(n)
  y <- beta * x + rnorm(n, 0, 0.3)
  fit <- gwreg(x, y, xy, bandwidth = 0.25)
  b_hat <- fit$estimate
  expect_gt(cor(b_hat, beta), 0.7)
})
