# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 21 cross-validation: native Hawkes log-likelihood vs the
# hawkes package (exponential kernel, constant baseline).
library(testthat)
library(rmorie)

test_that("native exp-kernel Hawkes loglik matches hawkes package", {
  skip_if_not_installed("hawkes")
  set.seed(190)
  # simulate a modest exponential Hawkes stream by thinning
  mu <- 0.5
  eta <- 0.4
  beta <- 1.2
  t <- c()
  s <- 0
  Tmax <- 200
  lam_bar <- function(hist, s) mu + eta * beta *
    sum(exp(-beta * (s - hist)))
  while (s < Tmax) {
    lb <- lam_bar(t, s) + eta * beta
    s <- s + rexp(1, lb)
    if (s < Tmax && runif(1) < lam_bar(t, s) / lb) t <- c(t, s)
  }
  skip_if(length(t) < 30, "too few simulated events")
  T_ <- max(t)
  theta <- c(log(mu), eta, beta)
  mine <- rmorie:::.tps_hwka_neg_loglik_general(
    theta, t, T_, kernel_kind = "exponential",
    baseline_kind = "constant")
  ref <- as.numeric(hawkes::likelihoodHawkes(
    lambda0 = mu, alpha = eta * beta, beta = beta, history = t))
  # both are negative log-likelihoods over the horizon max(t)
  expect_equal(mine, ref, tolerance = 1e-6)
})
