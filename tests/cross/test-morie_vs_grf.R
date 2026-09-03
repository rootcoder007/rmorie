# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native R-learner causal forest vs grf (module 11).
# Suggests allowed here ONLY.

test_that("cross: native dr_forest ATE agrees with grf AIPW", {
  skip_if_not_installed("grf")
  set.seed(121)
  n <- 2000L
  X <- matrix(rnorm(n * 4), n, 4)
  w <- rbinom(n, 1, plogis(0.5 * X[, 1] - 0.3 * X[, 2]))
  y <- (1 + 0.5 * X[, 2]) * w + X[, 1] + rnorm(n)
  df <- data.frame(t = w, y = y, X)
  names(df)[3:6] <- paste0("x", 1:4)

  ours <- morie_estimate_dr_forest(df, "t", "y", paste0("x", 1:4))
  cf <- grf::causal_forest(X, y, w, seed = 121)
  est <- grf::average_treatment_effect(cf, method = "AIPW")
  ref <- unname(est[["estimate"]])
  ref_se <- unname(est[["std.err"]])

  # true ATE = 1 + 0.5*E[x2] = 1; both must cover, and agree jointly
  expect_lt(abs(ours$ate - ref), 1.96 * sqrt(ours$se^2 + ref_se^2))
  expect_lt(abs(ours$ate - 1), 4 * ours$se)
})

test_that("cross: native tau(x) correlates with grf predictions", {
  skip_if_not_installed("grf")
  set.seed(122)
  n <- 3000L
  X <- matrix(rnorm(n * 3), n, 3)
  w <- rbinom(n, 1, plogis(0.4 * X[, 1]))
  y <- (1 + X[, 2]) * w + X[, 1] + rnorm(n)

  nf <- rmorie:::.morie_causal_forest_native(X, y, w, n_trees = 300L)
  cf <- grf::causal_forest(X, y, w, seed = 122)
  tau_grf <- as.numeric(stats::predict(cf)$predictions)

  expect_gt(stats::cor(nf$tau, tau_grf), 0.6)
})

test_that("cross: DR-learner CATE agrees with grf's causal forest", {
  skip_if_not_installed("grf")
  set.seed(124)
  n <- 3000L
  X <- matrix(rnorm(n * 3), n, 3)
  w <- rbinom(n, 1, plogis(0.4 * X[, 1]))
  y <- (1 + X[, 2]) * w + X[, 1] + rnorm(n)
  df <- data.frame(y = y, d = w, X)
  names(df)[3:5] <- paste0("x", 1:3)
  tau_dr <- morie_estimate_cate(df, "d", "y", paste0("x", 1:3),
                                meta_learner = "dr_learner")
  cf <- grf::causal_forest(X, y, w, seed = 124)
  tau_grf <- as.numeric(stats::predict(cf)$predictions)
  expect_gt(stats::cor(tau_dr, tau_grf), 0.6)
})
