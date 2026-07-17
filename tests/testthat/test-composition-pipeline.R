# SPDX-License-Identifier: AGPL-3.0-or-later
# Phase 17 -- THE COMPOSITION TEST. One pipeline, one class system:
# DAG -> identification -> matching -> weighting -> estimation ->
# refutation -> report, on the real LaLonde data. No other package
# passes this because no other package holds every stage natively.

test_that("full MRM pipeline composes across modules on LaLonde", {
  p <- system.file("extdata", "quasiex", "lalonde_matchit.csv",
                   package = "rmorie")
  if (!nzchar(p)) p <- file.path("inst", "extdata", "quasiex",
                                 "lalonde_matchit.csv")
  d <- read.csv(p)
  d$treat <- as.integer(d$treat)

  # 1. Build the DAG (module 13).
  dag <- morie_dag(
    edges = c("re74 -> treat", "re74 -> re78",
              "age -> treat",  "age -> re78",
              "educ -> treat", "educ -> re78",
              "treat -> re78"),
    exposure = "treat", outcome = "re78"
  )

  # 2. Identify the estimand.
  id <- morie_dag_identify(dag)
  expect_true(id$identified)
  expect_setequal(id$adjustment_set, c("re74", "age", "educ"))

  # 3. Match on the identified adjustment set (module 1-7 family).
  m <- morie_matching_nearest_neighbor(d, "treat", id$adjustment_set)
  expect_true(is.list(m))

  # 4. Weight on the same set (module 15).
  w <- morie_weight_ps(d, "treat", id$adjustment_set, estimand = "ATT")
  expect_s3_class(w, "morie_weight")
  bal <- morie_weight_diagnostic(w, d, "treat", id$adjustment_set)
  expect_true(is.data.frame(bal))

  # 5. Estimate through the DAG router (module 13 -> DML engines).
  est <- morie_dag_estimate(dag, d, method = "backdoor.aipw")
  expect_true(is.finite(est$ate))

  # 6. Refute (module 13, DoWhy trio).
  rf_placebo <- morie_dag_refute(dag, d, method = "placebo_treatment",
                                 n_reps = 10L)
  rf_subset <- morie_dag_refute(dag, d, method = "data_subset",
                                n_reps = 10L)
  expect_true(is.list(rf_placebo) && is.list(rf_subset))

  # 7. Report through the MRM flagship (phase 16).
  eff <- morie_mrm_estimate_causal_effect(
    d, treatment = "treat", outcome = "re78",
    covariates = id$adjustment_set
  )
  rpt <- morie_mrm_report(eff)
  expect_true(is.character(rpt) || is.list(rpt))
})
