# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 1.l tests for the RDD / IRT wrapper-as-extender entry points
# in R/extenders_rdd.R: rddensity / rdlocrand / rdpower / anchors /
# anominate.  Each block guards with skip_if_not_installed() and
# checks the happy-path shape (a 2-element list with $method and
# $raw).

# ---------------------------------------------------------------------------
# rddensity
# ---------------------------------------------------------------------------

test_that("morie_rdd_density_test wraps rddensity::rddensity", {
  skip_if_not_installed("rddensity")
  set.seed(1L)
  x <- c(stats::rnorm(200L, mean = -0.2), stats::rnorm(200L, mean = 0.2))
  out <- morie_rdd_density_test(x, cutoff = 0)
  expect_type(out, "list")
  expect_identical(out$method, "rddensity::rddensity")
  expect_false(is.null(out$raw))
  # rddensity::rddensity returns class "CJMrddensity" (Cattaneo-
  # Jansson-Ma) in current CRAN versions, not bare "rddensity".
  expect_s3_class(out$raw, "CJMrddensity")
})


# ---------------------------------------------------------------------------
# rdlocrand
# ---------------------------------------------------------------------------

test_that("morie_rdd_local_randinf wraps rdlocrand::rdrandinf", {
  skip_if_not_installed("rdlocrand")
  set.seed(2L)
  R <- stats::runif(200L, -1, 1)
  Y <- 0.5 * R + as.numeric(R >= 0) * 0.3 + stats::rnorm(200L, sd = 0.5)
  out <- morie_rdd_local_randinf(
    Y = Y, R = R, wl = -0.2, wr = 0.2,
    reps = 200L, seed = 42L
  )
  expect_type(out, "list")
  expect_identical(out$method, "rdlocrand::rdrandinf")
  expect_false(is.null(out$raw))
})


# ---------------------------------------------------------------------------
# rdpower
# ---------------------------------------------------------------------------

test_that("morie_rdd_power_calc wraps rdpower::rdpower", {
  skip_if_not_installed("rdpower")
  set.seed(3L)
  R <- stats::runif(500L, -1, 1)
  Y <- 0.4 * R + as.numeric(R >= 0) * 0.2 + stats::rnorm(500L, sd = 0.5)
  out <- morie_rdd_power_calc(cbind(Y, R), cutoff = 0, tau = 0.2)
  expect_type(out, "list")
  expect_identical(out$method, "rdpower::rdpower")
  expect_false(is.null(out$raw))
})


# ---------------------------------------------------------------------------
# anominate
# ---------------------------------------------------------------------------

test_that("morie_anominate_ideal_points wraps anominate::anominate", {
  skip_if_not_installed("anominate")
  skip_if_not_installed("pscl")
  set.seed(5L)
  n_leg  <- 20L
  n_vote <- 30L
  ideal <- stats::rnorm(n_leg)
  cut   <- stats::rnorm(n_vote)
  votes <- matrix(0L, nrow = n_leg, ncol = n_vote)
  for (j in seq_len(n_vote)) {
    p <- stats::pnorm(ideal - cut[j])
    votes[, j] <- stats::rbinom(n_leg, 1L, p)
  }
  rc <- pscl::rollcall(
    data = votes,
    yea = 1L, nay = 0L, missing = NA,
    legis.names = paste0("L", seq_len(n_leg)),
    vote.names = paste0("V", seq_len(n_vote))
  )
  out <- morie_anominate_ideal_points(
    rc, dims = 1L, nsamp = 100L, burnin = 50L, thin = 1L,
    polarity = 1L, verbose = FALSE
  )
  expect_type(out, "list")
  expect_identical(out$method, "anominate::anominate")
  expect_false(is.null(out$raw))
})
