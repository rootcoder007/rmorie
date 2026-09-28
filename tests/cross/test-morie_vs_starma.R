# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: StarModels against the starma package (Cheysson): space-time
# ACF/PACF are deterministic and must agree exactly; starma's Kalman-filter
# STARMA estimates cannot beat our conditional-least-squares minimiser on its
# own objective.

library(testthat)
library(rmorie)

W1 <- matrix(c(0, 0.5, 0.5, 0, 0.5, 0, 0, 0.5, 0.5, 0, 0, 0.5, 0, 0.5, 0.5, 0), 4, byrow = TRUE)
W2 <- matrix(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), 4, byrow = TRUE)
W <- list(W1, W2)
set.seed(3)
X <- matrix(0, 40, 4)
for (t in 2:40) X[t, ] <- 0.4 * X[t - 1, ] + 0.3 * as.numeric(W1 %*% X[t - 1, ]) + rnorm(4)
X <- X + 2

test_that("space-time ACF and PACF equal starma::stacf / stpacf", {
  skip_if_not_installed("starma")
  wl <- list(diag(4), W1, W2)
  d <- starma::stcenter(X)
  ref_acf <- starma::stacf(d, wl, tlag.max = 4, plot = FALSE)
  mine <- StAcf(X, W, max_lag_t = 4)$acf
  expect_equal(unname(as.matrix(ref_acf)), unname(mine[2:5, ]), tolerance = 1e-10)
  ref_pacf <- starma::stpacf(d, wl, tlag.max = 3, plot = FALSE)
  # starma nests its Yule-Walker systems lexicographically in (time lag, spatial order)
  expect_equal(unname(as.matrix(ref_pacf)), unname(StPacf(X, W, max_lag_t = 3, method = "starma")$pacf), tolerance = 1e-10)
  # the published (time-nested) definition agrees with it at the highest spatial order only
  pd <- StPacf(X, W, max_lag_t = 3)$pacf
  expect_equal(unname(as.matrix(ref_pacf))[, 3], unname(pd[, 3]), tolerance = 1e-10)
  expect_gt(max(abs(unname(as.matrix(ref_pacf))[, 1] - pd[, 1])), 1e-3)
})

test_that("starma's STARMA estimates cannot beat the CSS minimiser on the CSS objective", {
  skip_if_not_installed("starma")
  wl <- list(diag(4), W1, W2)
  d <- starma::stcenter(X)
  ar <- matrix(c(1, 1, 0), 1)
  ma <- matrix(c(1, 0, 0), 1)
  ref <- starma::starma(d, wl, ar = ar, ma = ma, iterate = 1)
  mine <- StarmaFit(X, W, list(c(0, 1)), list(0))
  # evaluate our CSS objective at starma's estimates (their sign convention: z = phi z - theta e + e, as ours)
  css <- function(phi, theta) {
    z <- X - mean(X)
    e <- matrix(0, nrow(z), 4)
    for (t in 2:nrow(z)) {
      pred <- phi[1] * z[t - 1, ] + phi[2] * as.numeric(W1 %*% z[t - 1, ])
      if (t >= 3) pred <- pred - theta * e[t - 1, ]
      e[t, ] <- z[t, ] - pred
    }
    sum(e[2:nrow(z), ]^2)
  }
  ref_css <- css(as.numeric(ref$phi[1, 1:2]), as.numeric(ref$theta[1, 1]))
  expect_equal(mine$rss, css(mine$phi[, 3], mine$theta[, 3]), tolerance = 1e-10)
  expect_lte(mine$rss, ref_css + 1e-8)
  expect_lt(abs(mine$phi[1, 3] - ref$phi[1, 1]), 0.1)
})
