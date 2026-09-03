# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 24 — the MRM flagship: load / reconcile / estimate / report,
# plus the phase-17 composition test across the unified class system.

.mk_mrm_df <- function(n = 500, tau = 0.8, seed = 90) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- runif(n)
  t <- rbinom(n, 1, plogis(0.6 * x1 - 0.4 * x2))
  y <- 1 + tau * t + 0.5 * x1 + 0.3 * x2 + rnorm(n)
  data.frame(y = y, t = t, x1 = x1, x2 = x2)
}

test_that("morie_mrm_load_si_dataset attaches provenance", {
  skip_if_not_installed("rmoriedata")
  d <- morie_mrm_load_si_dataset("otis_b01")
  expect_s3_class(d, "morie_mrm_dataset")
  expect_equal(d$provenance$n_rows, nrow(d$data))
  expect_match(d$provenance$sha256, "^[0-9a-f]{64}$")
})

test_that("morie_mrm_reconcile: matches, orphans, conflicts, tolerance", {
  a <- data.frame(id = 1:6, date = c(10, 20, 30, 40, 50, 60),
                  outcome = c("x", "x", "y", "y", "z", "z"))
  b <- data.frame(id = c(1:4, 9, 10), date = c(10, 21, 30, 44, 1, 2),
                  outcome = c("x", "x", "y", "q", "r", "s"))
  r <- morie_mrm_reconcile(a, b, keys = "id",
                           compare = c("date", "outcome"),
                           numeric_tolerance = 1)
  expect_s3_class(r, "morie_mrm_reconciliation")
  expect_equal(nrow(r$matched), 4L)
  expect_equal(r$match_rate, 4 / 6)
  expect_equal(nrow(r$unmatched_primary), 2L)
  expect_equal(nrow(r$unmatched_secondary), 2L)
  # date 20 vs 21 within tolerance; 40 vs 44 conflicts; outcome y vs q
  expect_setequal(r$conflicts$field, c("date", "outcome"))
  expect_equal(sum(r$conflicts$field == "date"), 1L)
  expect_output(print(r), "match rate")
})

test_that("morie_mrm_estimate_causal_effect composes native estimators", {
  df <- .mk_mrm_df(tau = 0.8, seed = 91)
  eff <- morie_mrm_estimate_causal_effect(
    df, "t", "y", c("x1", "x2"),
    methods = c("matching", "ate", "aipw", "dml"))
  expect_s3_class(eff, "morie_mrm_effect")
  expect_gte(nrow(eff$results), 3L)
  expect_true(all(abs(eff$results$estimate - 0.8) < 0.4))
  expect_true(all(eff$results$p_adjusted >= eff$results$p_value - 1e-12))
  expect_equal(eff$consensus$estimate, 0.8, tolerance = 0.25)
  expect_match(eff$citation, "Ruhela")
  expect_match(eff$citation, "rmorie")
})

test_that("morie_mrm_report renders all four formats", {
  df <- .mk_mrm_df(n = 300, seed = 92)
  eff <- morie_mrm_estimate_causal_effect(df, "t", "y", c("x1", "x2"),
                                          methods = c("ate", "aipw"))
  txt <- morie_mrm_report(eff, format = "text")
  expect_true(any(grepl("Consensus", txt)))
  md <- morie_mrm_report(eff, format = "markdown")
  expect_true(any(grepl("^\\|", md)))
  tex <- morie_mrm_report(eff, format = "latex")
  expect_true(any(grepl("begin\\{tabular\\}", tex)))
  expect_false(any(grepl("[^\\\\]%", tex)))   # escaped percent signs
  html <- morie_mrm_report(eff, format = "html")
  expect_true(any(grepl("<table", html)))
  expect_output(print(eff), "Consensus")
})

test_that("full MRM pipeline composes across modules (phase 17)", {
  # One DAG, one estimand, one match, one estimate, one refutation —
  # every step a module of this branch.
  set.seed(93)
  n <- 600
  age <- rnorm(n)
  educ <- rnorm(n)
  re74 <- rnorm(n)
  treat <- rbinom(n, 1, plogis(0.4 * age + 0.3 * educ + 0.3 * re74))
  re78 <- 1 + 0.9 * treat + 0.5 * age + 0.4 * educ + 0.6 * re74 +
    rnorm(n)
  d <- data.frame(age = age, educ = educ, re74 = re74,
                  treat = treat, re78 = re78)
  # 1. the DAG (module 13)
  dag <- morie_dag(
    edges = c("re74 -> treat", "re74 -> re78",
              "age -> treat", "age -> re78",
              "educ -> treat", "educ -> re78",
              "treat -> re78"),
    exposure = "treat", outcome = "re78")
  # 2. identification
  id <- morie_dag_identify(dag)
  expect_true(id$identified)
  expect_setequal(id$adjustment_set, c("re74", "age", "educ"))
  # 3. matching on the identified set (module 1)
  m <- morie_matching_nearest_neighbor(d, "treat", id$adjustment_set)
  expect_gt(nrow(m$match_pairs), 50)
  # 4. estimation through the DAG (module 10 via 13)
  ate <- morie_dag_estimate(dag, d, method = "backdoor.dml")
  est <- if (!is.null(ate$estimate)) ate$estimate else ate$ate
  expect_equal(est, 0.9, tolerance = 0.3)
  # 5. refutation (module 13): each of the three checks passes
  for (mth in c("placebo_treatment", "random_common_cause",
                "data_subset")) {
    rf <- morie_dag_refute(dag, d, mth, n_reps = 10L)
    expect_true(rf$passed)
  }
  # 6. the MRM report renders the composed estimate (module 24)
  eff <- morie_mrm_estimate_causal_effect(d, "treat", "re78",
                                          id$adjustment_set,
                                          methods = c("ate", "dml"))
  out <- morie_mrm_report(eff, format = "markdown")
  expect_true(any(grepl("dml", out)))
})
