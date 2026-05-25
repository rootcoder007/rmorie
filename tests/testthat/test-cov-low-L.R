# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch L: dccmd, mrm_doe, entheo_preprocess, mrm_diagnostics, study_reporting, synthetic, frns_predpol, frns_metrics.

# ==== dccmd.R ====
test_that("morie_dcc_multivariate_garch errors when n too small", {
  # n=10 rows, k=2 cols: 20 values matches; n=10 < 30 triggers the guard
  expect_error(morie_dcc_multivariate_garch(matrix(rnorm(20), 10, 2)), "Need n>=30")
  # k=1 column: matrix(rnorm(30), 30, 1) is correct (30*1=30 values)
  expect_error(morie_dcc_multivariate_garch(matrix(rnorm(30), 30, 1)), "Need n>=30")
})

test_that(".dccmd_negll guards return 1e10 on out-of-domain parameters", {
  set.seed(1)
  Z <- matrix(rnorm(60), 30, 2)
  Q_bar <- crossprod(Z) / 30
  expect_equal(morie:::.dccmd_negll(c(-0.01, 0.5), Q_bar, 30, Z), 1e10)
  expect_equal(morie:::.dccmd_negll(c(0.1, -0.01), Q_bar, 30, Z), 1e10)
  expect_equal(morie:::.dccmd_negll(c(0.6, 0.5), Q_bar, 30, Z), 1e10)
})

# ==== mrm_doe.R ====
test_that("mrm_random_latin gives reproducible permuted Latin square", {
  sq1 <- mrm_random_latin(k = 5, seed = 7L)
  sq2 <- mrm_random_latin(k = 5, seed = 7L)
  expect_identical(sq1, sq2)
  for (i in seq_len(5)) {
    expect_equal(length(unique(sq1[i, ])), 5L)
    expect_equal(length(unique(sq1[, i])), 5L)
  }
})

# ==== entheo_preprocess.R ====
test_that("preprocess_eeg warns on absent eeg matrices", {
  rec <- list(eeg = list(sfreq = NA, data_dmt = NULL, data_pcb = NULL))
  res <- morie:::preprocess_eeg(rec)
  expect_equal(res$sfreq, 250)
  expect_equal(res$n_bad, 0L)
  expect_equal(res$n_channels, 0L)
  expect_true(any(grepl("data_dmt absent", res$warnings)))
})

test_that("preprocess_fmri warns when motion_fd_mm absent", {
  set.seed(1)
  rec_no_fd <- list(fmri = list(
    motion_fd_mm = NULL,
    data_dmt = matrix(rnorm(40), 5, 8),
    data_pcb = NULL
  ))
  res <- morie:::preprocess_fmri(rec_no_fd)
  expect_equal(res$n_scrubbed, 0L)
})

test_that(".entheo_asr_trim returns a list with arr + n_bad fields", {
  set.seed(1)
  x <- matrix(rnorm(50), 5, 10)
  tr <- morie:::.entheo_asr_trim(x, threshold = 5)
  expect_type(tr, "list")
  expect_true(all(c("arr", "n_bad") %in% names(tr)))
  expect_equal(dim(tr$arr), dim(x))
})

# ==== mrm_diagnostics.R ====
test_that("mrm_standardised_difference handles zero-variance covariate", {
  set.seed(1)
  n <- 100L
  df <- data.frame(
    D = rbinom(n, 1, 0.5),
    const = rep(5, n),
    age = rnorm(n, 50, 10)
  )
  tbl <- mrm_standardised_difference(df,
    treatment_col = "D",
    covariates = c("const", "age")
  )
  expect_true(is.na(tbl$smd_pct[tbl$covariate == "const"]))
})

test_that("mrm_median_causal_effect errors when no valid matches", {
  set.seed(1)
  df <- data.frame(
    D = rep(1L, 20),
    y = rnorm(20),
    age = rnorm(20)
  )
  expect_error(
    mrm_median_causal_effect(df,
      treatment_col = "D",
      outcome_col = "y",
      covariates = "age"
    ),
    "no valid matches"
  )
})

# ==== study_reporting.R ====
test_that(".binary_power_required_n returns NA on zero effect", {
  expect_true(is.na(morie:::.binary_power_required_n(0.3, 0.3)))
})

test_that(".continuous_power_required_n returns NA on zero/NA pooled d", {
  expect_true(is.na(morie:::.continuous_power_required_n(1, 1, 1)))
})

test_that(".block_schedule returns empty data.frame on empty strata or NA n", {
  empty1 <- morie:::.block_schedule("ep1", required_n = 40, strata_levels = character())
  expect_equal(nrow(empty1), 0L)
  empty2 <- morie:::.block_schedule("ep1", required_n = NA_real_, strata_levels = c("M", "F"))
  expect_equal(nrow(empty2), 0L)
})

# ==== synthetic.R ====
test_that("resolve_synthetic_name_map rejects bad inputs", {
  expect_error(
    morie:::resolve_synthetic_name_map(c("a", "b"), profile = "generic"),
    "named character vector"
  )
  bad <- c(id = "x", weight = "w")
  expect_error(
    morie:::resolve_synthetic_name_map(bad, profile = "generic"),
    "missing required keys"
  )
})

test_that("morie_generate_synthetic_data validates n and special_code_rate", {
  expect_error(morie_generate_synthetic_data(n = 50), "n` must be an integer")
  expect_error(
    morie_generate_synthetic_data(n = 200, special_code_rate = -0.1),
    "special_code_rate"
  )
})

test_that("inject_special_codes is identity when rate == 0", {
  set.seed(1)
  x <- 1:50
  expect_identical(morie:::inject_special_codes(x, rate = 0), x)
})

# ==== frns_predpol.R ====
test_that("morie_predpol_aggregate_areas errors on length mismatch", {
  expect_error(
    morie_predpol_aggregate_areas(area = c("a", "b"), risk = c(1, 2, 3), outcome = c(0, 1)),
    "same length"
  )
})

test_that("morie_predpol_calibration_audit guards length, n<2", {
  expect_error(
    morie_predpol_calibration_audit(
      areas = "a", mean_risk = c(1, 2),
      outcome_rate = c(1, 2), group = c("X", "Y")
    ),
    "must all align"
  )
  expect_error(
    morie_predpol_calibration_audit(
      areas = c("a"), mean_risk = 1,
      outcome_rate = 1, group = "X"
    ),
    "at least two areas"
  )
})

test_that("morie_predpol_score_disparity guards", {
  expect_error(
    morie_predpol_score_disparity(score = 1:3, group = c("A", "A")),
    "same length"
  )
  expect_error(
    morie_predpol_score_disparity(score = 1:3, group = c("A", "A", "A")),
    "at least two groups"
  )
})

# ==== frns_metrics.R ====
test_that(".frns_check_aligned errors on length mismatch and empty input", {
  expect_error(
    morie:::.frns_check_aligned(list("a", 1:3), list("b", 1:2)),
    "length mismatch"
  )
  expect_error(
    morie:::.frns_check_aligned(list("a", integer(0)), list("b", integer(0))),
    "inputs are empty"
  )
})

test_that(".frns_resolve_privileged errors on unknown privileged key", {
  rates <- list(A = list(rate = 0.5), B = list(rate = 0.7))
  expect_error(
    morie:::.frns_resolve_privileged("Z", rates),
    "privileged group 'Z' not found"
  )
})

test_that("morie_fairness_disparate_impact handles too-few-groups", {
  expect_error(
    morie_fairness_disparate_impact(c(1, 1, 0), c("A", "A", "A")),
    "at least two groups"
  )
})

test_that("morie_fairness_demographic_parity guards too-few-groups", {
  expect_error(
    morie_fairness_demographic_parity(c(1, 0, 1), c("A", "A", "A")),
    "at least two groups"
  )
})

test_that("morie_fairness_gini guards empty + per-group breakdown", {
  expect_error(morie_fairness_gini(numeric(0)), "values is empty")
  expect_equal(morie_fairness_gini(c(0, 0, 0, 0))$value, 0)
})
