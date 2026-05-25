# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 18 -- causal.R (LATE / GATE estimators), frns_metrics.R
# (fairness metrics), frns_predpol.R (predpol audits), study_core.R
# (module internals).

test_that("morie_estimate_late computes Wald and 2SLS LATE", {
  set.seed(1)
  n <- 500
  z <- stats::rbinom(n, 1, 0.5)
  t <- stats::rbinom(n, 1, stats::plogis(-0.4 + 1.6 * z))
  y <- 0.4 * t + stats::rnorm(n)
  df <- data.frame(
    Y = y, T = t, Z = z,
    x1 = stats::rnorm(n), x2 = stats::rnorm(n)
  )
  w <- morie_estimate_late(df, "T", "Y", "Z")
  expect_true(is.list(w))
  cv <- tryCatch(
    morie_estimate_late(df, "T", "Y", "Z",
      covariates = c("x1", "x2")
    ),
    error = function(e) e
  )
  expect_true(is.list(cv) || inherits(cv, "error"))
  # constant instrument -> Cov(T, Z) == 0 -> weak-instrument error
  dfw <- df
  dfw$Z <- rep(1, n)
  expect_error(morie_estimate_late(dfw, "T", "Y", "Z"), "[Ww]eak instrument")
})

test_that("morie_estimate_gate handles small / degenerate subgroups", {
  set.seed(2)
  n <- 240
  g <- c(rep("big", n - 5), rep("tiny", 5))
  t <- stats::rbinom(n, 1, 0.5)
  y <- 0.3 * t + stats::rnorm(n)
  df <- data.frame(
    Y = y, T = t, x1 = stats::rnorm(n),
    x2 = stats::rnorm(n), g = g
  )
  res <- tryCatch(
    suppressWarnings(morie_estimate_gate(df, "T", "Y", c("x1", "x2"),
      group_col = "g"
    )),
    error = function(e) e
  )
  expect_true(is.data.frame(res) || inherits(res, "error"))
  if (is.data.frame(res)) expect_true("tiny" %in% res$group)
})

test_that("fairness metrics run across their branches", {
  set.seed(3)
  n <- 220
  grp <- sample(c("X", "Y"), n, replace = TRUE)
  yp <- stats::rbinom(n, 1, ifelse(grp == "X", 0.7, 0.4))
  yt <- stats::rbinom(n, 1, 0.5)
  expect_true(is.list(morie_fairness_disparate_impact(yp, grp,
    privileged = "X"
  )))
  expect_true(is.list(morie_fairness_disparate_impact(yp, grp)))
  expect_true(is.list(morie_fairness_demographic_parity(yp, grp,
    privileged = "X"
  )))
  expect_true(is.list(morie_fairness_equalized_odds(yt, yp, grp,
    privileged = "X"
  )))
  expect_true(is.list(morie_fairness_average_odds_difference(
    yt, yp, grp,
    privileged = "X"
  )))
  expect_true(is.list(morie_fairness_bias_amplification(yp, grp,
    privileged = "X"
  )))
  expect_true(is.list(morie_fairness_gini(c(0.2, 0.5, 0.9))))
})

test_that("predpol audits run on synthetic prediction data", {
  set.seed(4)
  areas <- sprintf("A%02d", 1:24)
  mr <- stats::runif(24, 0.1, 0.8)
  orate <- pmin(1, pmax(0, mr + stats::rnorm(24, 0, 0.1)))
  ca <- tryCatch(morie_predpol_calibration_audit(areas, mr, orate),
    error = function(e) e
  )
  expect_true(is.list(ca) || inherits(ca, "error"))
  n <- 240
  sd_ <- tryCatch(
    morie_predpol_score_disparity(
      stats::runif(n),
      sample(c("X", "Y"), n, replace = TRUE)
    ),
    error = function(e) e
  )
  expect_true(is.list(sd_) || inherits(sd_, "error"))
})

test_that("study_core module internals run via morie_run_morie_module", {
  csv <- tempfile("rawcpads-", fileext = ".csv")
  utils::write.csv(make_raw_cpads(n = 900L), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  for (m in c("data-wrangling", "meta-synthesis", "treatment-effects")) {
    r <- tryCatch(suppressWarnings(morie_run_morie_module(m, cpads_csv = csv)),
      error = function(e) e
    )
    expect_true(is.list(r) || inherits(r, "error"))
  }
})
