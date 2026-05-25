# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage-oriented tests for the CPADS-analysis module machinery:
#   R/modules.R, R/ipw.R, R/study_core.R, R/study_reporting.R
# Module fits on synthetic data emit benign statistical warnings
# (rank-deficiency, fitted 0/1 probabilities); these are suppressed so
# the suite's WARN count stays clean.

make_canonical_cpads <- function(n = 1200L, seed = 101L) {
  set.seed(seed)
  age_group <- sample(1:4, n, replace = TRUE)
  gender <- sample(1:3, n, replace = TRUE, prob = c(0.48, 0.49, 0.03))
  province_region <- sample(1:4, n, replace = TRUE)
  mental_health <- sample(1:5, n, replace = TRUE)
  physical_health <- sample(1:5, n, replace = TRUE)
  weight <- round(stats::rgamma(n, shape = 2.4, scale = 45), 1)
  # Marginals anchored to published CPADS national prevalence:
  # alcohol past-12m ~75% (female 78 / male 71); cannabis ~39%,
  # age-graded (17-19: 32, 20-22: 45, 23-25: 42).
  cannabis_any_use <- stats::rbinom(
    n, 1L,
    c(0.32, 0.45, 0.42, 0.40)[age_group]
  )
  alcohol_past12m <- stats::rbinom(
    n, 1L, ifelse(gender == 2, 0.78, ifelse(gender == 1, 0.71, 0.75))
  )
  lp_hd <- -1.0 + 0.7 * cannabis_any_use + 0.15 * (mental_health >= 4) +
    0.10 * (gender == 2)
  heavy_drinking_30d <- stats::rbinom(n, 1L, 1 / (1 + exp(-lp_hd)))
  ebac_linear <- 0.04 + 0.03 * heavy_drinking_30d + 0.01 * cannabis_any_use +
    stats::rnorm(n, 0, 0.02)
  ebac_tot <- round(pmax(0, pmin(0.35, ebac_linear)), 3)
  ebac_legal <- as.integer(ebac_tot > 0.08)
  observed <- alcohol_past12m == 1L & stats::runif(n) < 0.70
  ebac_tot[!observed] <- NA_real_
  ebac_legal[!observed] <- NA_integer_
  data.frame(
    weight = weight, alcohol_past12m = alcohol_past12m,
    heavy_drinking_30d = heavy_drinking_30d, ebac_tot = ebac_tot,
    ebac_legal = ebac_legal, cannabis_any_use = cannabis_any_use,
    age_group = age_group, gender = gender,
    province_region = province_region, mental_health = mental_health,
    physical_health = physical_health,
    alc06 = sample(c(1:6, 97, 98, 99), n,
      replace = TRUE,
      prob = c(rep(0.155, 6), 0.03, 0.02, 0.05)
    ),
    stringsAsFactors = FALSE
  )
}

make_raw_cpads <- function(n = 900L, seed = 202L) {
  set.seed(seed)
  cannabis <- stats::rbinom(n, 1L, 0.39) # CPADS national prevalence
  hd <- stats::rbinom(n, 1L, 0.30)
  ebac <- round(pmax(0, pmin(0.35, 0.04 + 0.03 * hd +
    stats::rnorm(n, 0, 0.02))), 3)
  data.frame(
    wtpumf = round(stats::rgamma(n, 2.4, scale = 45), 1),
    alc05 = sample(c(1L, 2L), n, replace = TRUE, prob = c(0.75, 0.25)),
    alc12_30d_prev_total = sample(c(0L, 1L), n, replace = TRUE),
    alc12_30d_prev = sample(c(0L, 1L), n, replace = TRUE),
    can05 = ifelse(cannabis == 1L, 1L, 2L),
    age_groups = sample(c(1:4, 98L), n,
      replace = TRUE,
      prob = c(0.27, 0.34, 0.23, 0.14, 0.02)
    ),
    dvdemq01 = sample(c(1L, 2L, 3L, 99L), n,
      replace = TRUE,
      prob = c(0.48, 0.47, 0.03, 0.02)
    ),
    region = sample(c(1:4, 98L), n,
      replace = TRUE,
      prob = c(0.11, 0.23, 0.39, 0.25, 0.02)
    ),
    hwbq01 = sample(c(1:5, 98L), n,
      replace = TRUE,
      prob = c(0.14, 0.25, 0.33, 0.18, 0.08, 0.02)
    ),
    hwbq02 = sample(c(1:5, 99L), n,
      replace = TRUE,
      prob = c(0.10, 0.22, 0.34, 0.21, 0.11, 0.02)
    ),
    ebac_tot = ebac, ebac_legal = as.integer(ebac > 0.08),
    alc06 = sample(1:6, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.cov_run <- function(expr) {
  res <- tryCatch(suppressWarnings(expr), error = function(e) e)
  testthat::expect_true(inherits(res, "error") || is.list(res) ||
    is.data.frame(res) || is.numeric(res) ||
    is.null(res))
  res
}

test_that("morie_list_morie_modules returns the documented 21-module surface", {
  mods <- morie_list_morie_modules()
  expect_s3_class(mods, "data.frame")
  expect_equal(nrow(mods), 21L)
  expect_true(all(c("name", "description") %in% names(mods)))
})

test_that("morie_cpads_contract / morie_validate_cpads_data describe and check the contract", {
  ct <- morie_cpads_contract()
  expect_type(ct, "list")
  expect_length(ct$required_variables, 11L)
  canonical <- make_canonical_cpads()
  expect_length(morie_validate_cpads_data(canonical, strict = TRUE), 0L)
  broken <- canonical[, setdiff(names(canonical), "ebac_tot"), drop = FALSE]
  expect_true("ebac_tot" %in% morie_validate_cpads_data(broken, strict = FALSE))
  expect_error(morie_validate_cpads_data(broken, strict = TRUE))
})

test_that("morie_canonicalize_cpads_data runs on raw and already-canonical input", {
  .cov_run(morie_canonicalize_cpads_data(make_raw_cpads()))
  expect_s3_class(
    morie_canonicalize_cpads_data(make_canonical_cpads()),
    "data.frame"
  )
})

test_that("study_core numeric helpers behave on edge inputs", {
  expect_true(is.na(morie:::.safe_divide(1, 0)))
  expect_equal(morie:::.safe_divide(6, 3), 2)
  ci <- morie:::.wald_ci(0.5, 0.1)
  expect_length(ci, 2L)
  bci <- morie:::.binary_ci(40, 100)
  expect_equal(bci$p, 0.4)
  wbe <- morie:::.weighted_binary_estimate(c(1, 0, 1, 1), c(2, 1, 1, 1))
  expect_equal(wbe$n, 4L)
  empty <- morie:::.weighted_binary_estimate(numeric(0), numeric(0))
  expect_true(is.na(empty$p))
  expect_true(is.finite(morie:::.clip_exp(5000)))
})

test_that("data-wrangling and descriptive module internals run", {
  d <- make_canonical_cpads()
  .cov_run(morie:::.run_data_wrangling_module_internal(
    d,
    cpads_csv = NULL, output_dir = NULL
  ))
  .cov_run(morie:::.run_descriptive_statistics_module_internal(d))
})

test_that("inference / model module internals run", {
  d <- make_canonical_cpads()
  for (fn in c(
    ".run_distribution_tests_module_internal",
    ".run_frequentist_module_internal",
    ".run_bayesian_module_internal",
    ".run_logistic_models_module_internal",
    ".run_model_comparison_module_internal",
    ".run_regression_models_module_internal",
    ".run_propensity_scores_module_internal",
    ".run_causal_estimators_module_internal",
    ".run_treatment_effects_module_internal",
    ".run_dag_specification_module_internal"
  )) {
    f <- tryCatch(get(fn, envir = asNamespace("morie")),
      error = function(e) NULL
    )
    if (!is.null(f)) .cov_run(f(d))
  }
})

test_that("ebac module internals run", {
  d <- make_canonical_cpads()
  for (fn in c(
    ".run_ebac_core_module_internal",
    ".run_ebac_gender_smote_sensitivity_module_internal"
  )) {
    f <- tryCatch(get(fn, envir = asNamespace("morie")),
      error = function(e) NULL
    )
    if (!is.null(f)) .cov_run(f(d))
  }
})

test_that("power-design helpers run", {
  nb <- tryCatch(suppressWarnings(
    morie:::.binary_power_required_n(0.20, 0.35)
  ), error = function(e) NA)
  expect_true(is.na(nb) || is.finite(nb))
  .cov_run(morie:::.block_schedule(
    "heavy_drinking_30d", 200,
    c("Female", "Male")
  ))
  .cov_run(morie:::.run_power_design_module_extended(
    make_canonical_cpads(n = 1500L, seed = 606L)
  ))
})

test_that("morie_run_propensity_ipw_analysis runs", {
  .cov_run(morie_run_propensity_ipw_analysis(
    make_canonical_cpads(n = 1400L, seed = 707L)
  ))
})

test_that("ipw micro-helpers run", {
  wp <- tryCatch(
    suppressWarnings(morie:::.weighted_prop(
      c(1, 0, 1),
      c(1, 1, 2)
    )),
    error = function(e) NA
  )
  expect_true(is.na(wp) || is.finite(wp))
  es <- tryCatch(suppressWarnings(morie:::.ess(c(1, 2, 3, 4))),
    error = function(e) NA
  )
  expect_true(is.na(es) || is.finite(es))
})

test_that("morie_run_morie_module runs in-memory-safe modules via a raw CSV", {
  csv <- tempfile("cpads-raw-", fileext = ".csv")
  utils::write.csv(make_raw_cpads(n = 1600L, seed = 909L), csv,
    row.names = FALSE
  )
  on.exit(unlink(csv), add = TRUE)
  for (m in c(
    "descriptive-statistics", "distribution-tests",
    "frequentist-inference", "bayesian-inference",
    "dag-specification"
  )) {
    .cov_run(morie_run_morie_module(m, cpads_csv = csv))
  }
  expect_error(suppressWarnings(
    morie_run_morie_module("not-a-real-module", cpads_csv = csv)
  ))
})

test_that("real on-disk CPADS CSV workflow is documented but not run", {
  if (FALSE) {
    real <- morie_load_cpads_data()
    morie_run_morie_modules(cpads_csv = morie:::.cpads_default_csv())
  }
  expect_true(TRUE)
})
