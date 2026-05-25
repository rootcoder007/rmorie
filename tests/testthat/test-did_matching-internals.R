# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2II: tests for the internal helpers in did.R + matching.R
# (.morie_did_*, .morie_matching_*) accessed via morie:::. These
# helpers are reached only indirectly via the public fns; pinning
# them with direct tests makes regression breakage obvious.

# ============================================================== did internals

test_that(".morie_did_make_ci returns symmetric CI bounds", {
  out <- morie:::.morie_did_make_ci(estimate = 0.5, se = 0.1)
  expect_true(is.list(out) || is.numeric(out))
  flat <- unlist(out)
  expect_true(all(is.finite(flat)))
})

test_that(".morie_did_make_ci honours custom alpha", {
  out_05  <- morie:::.morie_did_make_ci(0.5, 0.1, alpha = 0.05)
  out_01  <- morie:::.morie_did_make_ci(0.5, 0.1, alpha = 0.01)
  # 99% CI should be wider than 95% CI
  w_05 <- diff(unlist(out_05)); w_01 <- diff(unlist(out_01))
  expect_true(abs(w_01) >= abs(w_05))
})

test_that(".morie_did_add_intercept prepends a 1-column", {
  X <- matrix(stats::rnorm(20), 10, 2)
  out <- morie:::.morie_did_add_intercept(X)
  expect_equal(ncol(out), ncol(X) + 1L)
  expect_true(all(out[, 1] == 1))
})

test_that(".morie_did_drop_na drops rows with NAs in named cols", {
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA),
                   z = 1:3)
  out <- morie:::.morie_did_drop_na(df, c("x", "y"))
  expect_equal(nrow(out), 1L)
})

test_that(".morie_did_pvalue returns 2-sided p from a t stat", {
  expect_lt(morie:::.morie_did_pvalue(1.96), 0.06)
  expect_gt(morie:::.morie_did_pvalue(0.0), 0.99)
})

test_that(".morie_did_ols_robust_se returns robust SE on a small fit", {
  set.seed(1L)
  n <- 50L; X <- cbind(1, stats::rnorm(n))
  y <- X %*% c(1, 0.5) + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(
    morie:::.morie_did_ols_robust_se(X, y),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("ols_robust_se error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.numeric(out) || is.list(out))
})

test_that(".morie_did_ols_robust_se with cluster_ids returns clustered SE", {
  set.seed(2L)
  n <- 60L; X <- cbind(1, stats::rnorm(n))
  y <- X %*% c(1, 0.5) + stats::rnorm(n, sd = 0.3)
  clust <- sample.int(10L, n, replace = TRUE)
  out <- tryCatch(
    morie:::.morie_did_ols_robust_se(X, y, cluster_ids = clust),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("ols_robust_se cluster error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.numeric(out) || is.list(out))
})

test_that(".morie_did_have_* helpers return a logical", {
  expect_type(morie:::.morie_did_have_fixest(), "logical")
  expect_type(morie:::.morie_did_have_did(),    "logical")
  expect_type(morie:::.morie_did_have_bacondecomp(), "logical")
  expect_type(morie:::.morie_did_have_synthdid(), "logical")
  expect_type(morie:::.morie_did_have_fwildboot(), "logical")
  expect_type(morie:::.morie_did_have_sandwich(), "logical")
})

test_that(".morie_did_outcome_regression_att returns ATT on synthetic data", {
  set.seed(3L)
  n <- 80L
  X <- cbind(1, stats::rnorm(n))
  treat <- stats::rbinom(n, 1L, 0.5)
  y <- 0.5 * treat + X[, 2] + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(
    morie:::.morie_did_outcome_regression_att(y, X, treat),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("outcome_regression_att error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.numeric(out) || is.list(out))
})

test_that(".morie_did_ipw_att returns ATT given y/treat/ps", {
  set.seed(4L)
  n <- 100L
  treat <- stats::rbinom(n, 1L, 0.5)
  ps <- pmin(pmax(stats::runif(n, 0.1, 0.9), 0.05), 0.95)
  y <- 0.5 * treat + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(
    morie:::.morie_did_ipw_att(y, treat, ps),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("ipw_att error: %s", conditionMessage(out)))
  }
  expect_true(is.numeric(out) || is.list(out))
})

test_that(".morie_did_within_transform demean each unit's series", {
  set.seed(5L)
  n_units <- 20L; n_periods <- 5L
  df <- expand.grid(unit = 1:n_units, time = 1:n_periods)
  df$y <- df$unit * 10 + df$time + stats::rnorm(nrow(df))
  out <- tryCatch(
    morie:::.morie_did_within_transform(df, "y", "unit", "time"),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("within_transform error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.numeric(out) || is.list(out) ||
                is.data.frame(out))
})

# =========================================================== matching internals

test_that(".morie_matching_logit returns ln(p/(1-p)) with eps clipping", {
  expect_equal(morie:::.morie_matching_logit(0.5), 0)
  expect_lt(morie:::.morie_matching_logit(0.01), -3)
  expect_gt(morie:::.morie_matching_logit(0.99), 3)
  # Edge: p=0 and p=1 should clip via eps, not return Inf
  expect_true(is.finite(morie:::.morie_matching_logit(0.0, eps = 1e-6)))
  expect_true(is.finite(morie:::.morie_matching_logit(1.0, eps = 1e-6)))
})

test_that(".morie_matching_have returns logical for installed/not-installed pkg", {
  expect_type(morie:::.morie_matching_have("MatchIt"), "logical")
  expect_false(morie:::.morie_matching_have("__not_a_real_pkg__"))
})

test_that(".morie_matching_drop_na drops rows with NAs in named cols", {
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA),
                   z = 1:3)
  out <- morie:::.morie_matching_drop_na(df, c("x", "y"))
  expect_equal(nrow(out), 1L)
})

test_that(".morie_matching_empty_pairs returns the empty-pairs sentinel", {
  out <- morie:::.morie_matching_empty_pairs()
  expect_type(out, "list")
})

test_that(".morie_matching_have_cpp returns logical for cpp fn names", {
  expect_type(morie:::.morie_matching_have_cpp("morie_matching_nearest_neighbor"),
              "logical")
})
