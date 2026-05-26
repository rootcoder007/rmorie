# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 1.n tests for the FDR / nonparametric / quantile /
# latent-class wrapper-as-extender entry points in
# R/extenders_nonparam.R: locfdr / fdrtool / quantreg / np /
# dirichletprocess / lcmm.  Each block guards with
# skip_if_not_installed() and checks the happy-path shape (a
# 2-element list with $method and $raw).

# ---------------------------------------------------------------------------
# locfdr
# ---------------------------------------------------------------------------

test_that("morie_locfdr_estimate wraps locfdr::locfdr", {
  skip_if_not_installed("locfdr")
  set.seed(1L)
  zz <- c(stats::rnorm(900L), stats::rnorm(100L, mean = 3))
  out <- morie_locfdr_estimate(zz, plot = 0L)
  expect_type(out, "list")
  expect_identical(out$method, "locfdr::locfdr")
  expect_false(is.null(out$raw))
  expect_true(!is.null(out$raw$fdr))
})


# ---------------------------------------------------------------------------
# fdrtool
# ---------------------------------------------------------------------------

test_that("morie_fdr_qvalues wraps fdrtool::fdrtool", {
  skip_if_not_installed("fdrtool")
  set.seed(2L)
  x <- c(stats::rnorm(900L), stats::rnorm(100L, mean = 3))
  out <- morie_fdr_qvalues(x, statistic = "normal", plot = FALSE,
                           verbose = FALSE)
  expect_type(out, "list")
  expect_identical(out$method, "fdrtool::fdrtool")
  expect_false(is.null(out$raw))
  expect_true(!is.null(out$raw$qval))
  expect_length(out$raw$qval, length(x))
})


# ---------------------------------------------------------------------------
# quantreg
# ---------------------------------------------------------------------------

test_that("morie_quantile_reg wraps quantreg::rq", {
  skip_if_not_installed("quantreg")
  set.seed(3L)
  n  <- 100L
  df <- data.frame(x = stats::rnorm(n))
  df$y <- 1 + 2 * df$x + stats::rnorm(n)
  out <- morie_quantile_reg(y ~ x, tau = c(0.25, 0.5, 0.75), data = df)
  expect_type(out, "list")
  expect_identical(out$method, "quantreg::rq")
  expect_false(is.null(out$raw))
  expect_true(inherits(out$raw, c("rq", "rqs")))
})


# ---------------------------------------------------------------------------
# np
# ---------------------------------------------------------------------------

test_that("morie_np_kernel_reg wraps np::npregbw + np::npreg", {
  skip_if_not_installed("np")
  set.seed(4L)
  n  <- 50L
  df <- data.frame(x = stats::runif(n, -1, 1))
  df$y <- sin(pi * df$x) + stats::rnorm(n, sd = 0.1)
  out <- suppressWarnings(suppressMessages(
    morie_np_kernel_reg(y ~ x, data = df)
  ))
  expect_type(out, "list")
  # The wrapper bypasses np's formula method (which has an NSE bug
  # inside the test scope) and dispatches the default xdat/ydat
  # method via npregbw -> npreg. Method string updated to match.
  expect_identical(out$method, "np::npreg (default method via npregbw)")
  expect_false(is.null(out$raw))
  expect_true(!is.null(out$raw$bws))
  expect_true(!is.null(out$raw$fit))
})


# ---------------------------------------------------------------------------
# dirichletprocess
# ---------------------------------------------------------------------------

test_that("morie_dp_gaussian_mixture wraps DirichletProcessGaussian + Fit", {
  skip_if_not_installed("dirichletprocess")
  set.seed(5L)
  y <- c(stats::rnorm(50L, -2), stats::rnorm(50L, 2))
  out <- suppressWarnings(suppressMessages(
    morie_dp_gaussian_mixture(y, iterations = 50L)
  ))
  expect_type(out, "list")
  expect_identical(
    out$method,
    "dirichletprocess::DirichletProcessGaussian + Fit"
  )
  expect_false(is.null(out$raw))
  expect_true(!is.null(out$raw$clusterParameters) ||
                !is.null(out$raw$clusterLabels))
})


# ---------------------------------------------------------------------------
# lcmm
# ---------------------------------------------------------------------------

test_that("morie_lcmm_latent_class wraps lcmm::lcmm", {
  skip_if_not_installed("lcmm")
  set.seed(6L)
  n_id <- 40L
  reps <- 4L
  id <- rep(seq_len(n_id), each = reps)
  tt <- rep(seq_len(reps), times = n_id)
  cls <- rep(stats::rbinom(n_id, 1L, 0.5), each = reps)
  y <- 1 + 0.3 * tt + cls * (0.5 + 0.4 * tt) + stats::rnorm(n_id * reps)
  df <- data.frame(ID = id, Time = tt, Y = y)
  out <- suppressWarnings(suppressMessages(
    morie_lcmm_latent_class(
      fixed   = Y ~ Time,
      random  = ~ Time,
      subject = "ID",
      data    = df,
      ng      = 1L,
      verbose = FALSE
    )
  ))
  expect_type(out, "list")
  expect_identical(out$method, "lcmm::lcmm")
  expect_false(is.null(out$raw))
})
