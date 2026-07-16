# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 18 — structural tests for the native IRT + psychometrics.
#' @srrstats {G5.6} 2PL/GRM parameter recovery on simulated data with
#'   known item parameters; EAP scores correlate with true abilities.

.sim_2pl <- function(n = 800, a = c(0.8, 1.2, 1.6, 1.0, 1.4),
                     b = c(-1, -0.5, 0, 0.5, 1), seed = 60) {
  set.seed(seed)
  th <- rnorm(n)
  X <- vapply(seq_along(a), function(j)
    rbinom(n, 1, plogis(a[j] * (th - b[j]))), numeric(n))
  list(X = X, theta = th, a = a, b = b)
}

test_that("2PL EM recovers item parameters and abilities", {
  sim <- .sim_2pl(n = 1500, seed = 61)
  fit <- morie_irt_2pl(sim$X)
  expect_s3_class(fit, "morie_irt_2pl")
  expect_true(fit$converged)
  expect_lt(sqrt(mean((fit$difficulty - sim$b)^2)), 0.2)
  expect_lt(sqrt(mean((fit$discrimination - sim$a)^2)), 0.3)
  expect_gt(cor(fit$theta, sim$theta), 0.75)
  expect_true(all(fit$theta_se > 0))
})

test_that("2PL handles NAs and rejects non-binary input", {
  sim <- .sim_2pl(seed = 62)
  X <- sim$X
  X[sample(length(X), 200)] <- NA
  fit <- morie_irt_2pl(X)
  expect_true(all(is.finite(fit$difficulty)))
  expect_error(morie_irt_2pl(matrix(1:6, 2)), "binary")
})

test_that("EAP scoring of new respondents matches training scores", {
  sim <- .sim_2pl(n = 1000, seed = 63)
  fit <- morie_irt_2pl(sim$X)
  sc <- morie_irt_eap(fit, sim$X)
  expect_equal(sc$theta, fit$theta, tolerance = 1e-8)
  # all-correct pattern scores above all-wrong pattern
  hi <- morie_irt_eap(fit, matrix(1, 1, 5))$theta
  lo <- morie_irt_eap(fit, matrix(0, 1, 5))$theta
  expect_gt(hi, lo)
})

test_that("GRM recovers ordered thresholds and abilities", {
  set.seed(64)
  n <- 1200; k <- 4
  a_true <- c(1.2, 1.5, 1.0, 1.8)
  th <- rnorm(n)
  X <- vapply(seq_len(k), function(j) {
    bj <- sort(runif(2, -1, 1))
    p_ge2 <- plogis(a_true[j] * (th - bj[1]))
    p_ge3 <- plogis(a_true[j] * (th - bj[2]))
    u <- runif(n)
    1L + (u < p_ge2) + (u < p_ge3)
  }, integer(n))
  fit <- morie_irt_grm(X)
  expect_s3_class(fit, "morie_irt_grm")
  for (j in seq_len(k)) {
    expect_true(all(diff(fit$thresholds[[j]]) > 0))
  }
  expect_gt(cor(fit$theta, th), 0.7)
})

test_that("native omega/KMO/parallel run and are sane", {
  set.seed(65)
  n <- 500
  f <- rnorm(n)
  X <- sapply(1:6, function(j) 0.7 * f + rnorm(n, 0, 0.6))
  om <- morie_psymet_omega(X)
  expect_gt(om$total, 0.7)
  expect_lte(om$total, 1)
  km <- morie_psymet_kmo(X)
  expect_gt(km$msa, 0.7)
  expect_equal(morie_psymet_parallel(X), 1L)
  # two clean factors detected
  g <- rnorm(n)
  X2 <- cbind(sapply(1:3, function(j) 0.9 * f + rnorm(n, 0, 0.4)),
              sapply(1:3, function(j) 0.9 * g + rnorm(n, 0, 0.4)))
  expect_equal(morie_psymet_parallel(X2), 2L)
})
