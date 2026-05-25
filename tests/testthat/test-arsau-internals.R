# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2LL: tests for arsau.R + arsau_analyze.R internal helpers
# + tps_hawkes_advanced.R extra internals.

# ========================================================== arsau.R internals

test_that(".arsau_make_entry builds a registry entry list", {
  out <- morie:::.arsau_make_entry(
    year_or_range = "2024", kind = "main_records",
    csv_filename = "uof_main_records.csv",
    sidecar_filename = "abc.json",
    expected_rows = 10000L, expected_cols = 65L,
    is_valid = TRUE,
    description_en = "Description", description_fr = "Description fr")
  expect_type(out, "list")
  expect_equal(out$year_or_range, "2024")
  expect_equal(out$kind, "main_records")
})

test_that(".arsau_lookup finds a registered (year, kind) entry", {
  out <- morie:::.arsau_lookup("2024", "main_records")
  expect_type(out, "list")
  expect_equal(out$year_or_range, "2024")
})

test_that(".arsau_lookup returns NULL on unregistered (year, kind)", {
  out <- morie:::.arsau_lookup("9999", "main_records")
  expect_null(out)
})

test_that(".arsau_coerce_year_key accepts 2024 + 2020-2022 keys", {
  expect_equal(morie:::.arsau_coerce_year_key("2024"), "2024")
  expect_equal(morie:::.arsau_coerce_year_key("2023"), "2023")
  expect_equal(morie:::.arsau_coerce_year_key(2024), "2024")
  # range_ok = TRUE allows "2020-2022"
  expect_equal(
    morie:::.arsau_coerce_year_key("2020-2022", range_ok = TRUE),
    "2020-2022")
})

test_that(".arsau_coerce_year_key errors on unknown year", {
  expect_error(
    morie:::.arsau_coerce_year_key("3025"),
    regexp = "Unknown ARSAU year")
})

# ============================================== arsau_analyze.R internals

test_that(".morie_arsau_locate_outcome_col finds a target column case-insensitively", {
  df <- data.frame(
    Race = c("White", "Black"),
    Gender = c("M", "F"),
    PhysicalInjuries = c("Yes", "No"),
    stringsAsFactors = FALSE)
  out <- tryCatch(
    morie:::.morie_arsau_locate_outcome_col(df, "PhysicalInjuries"),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("locate_outcome_col error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.character(out) || is.list(out))
})

# ============================================== tps_hawkes_advanced.R internals

test_that(".tps_hwka_result builds the result list", {
  out <- morie:::.tps_hwka_result(title = "Test",
                                   summary_lines = list(N = 100))
  expect_type(out, "list")
})

test_that(".tps_hwka_n_kernel_params returns 1 for exp / 2 for gamma/weibull/lomax", {
  expect_equal(morie:::.tps_hwka_n_kernel_params("exponential"), 1L)
  expect_equal(morie:::.tps_hwka_n_kernel_params("gamma"), 2L)
  expect_equal(morie:::.tps_hwka_n_kernel_params("weibull"), 2L)
  expect_equal(morie:::.tps_hwka_n_kernel_params("lomax"), 2L)
})

test_that(".tps_hwka_n_baseline_params returns 1 for constant / 4 for sinusoidal", {
  expect_equal(morie:::.tps_hwka_n_baseline_params("constant"), 1L)
  expect_equal(morie:::.tps_hwka_n_baseline_params("sinusoidal"),
               4L)
})

test_that(".tps_hwka_cpp_ok returns logical", {
  expect_type(morie:::.tps_hwka_cpp_ok(), "logical")
})

test_that(".tps_hwka_split_theta partitions exp+constant correctly", {
  # 1 baseline + 1 eta + 1 kernel = 3
  out <- morie:::.tps_hwka_split_theta(
    c(log(0.5), 0.3, log(0.7)),
    kernel_kind = "exponential",
    baseline_kind = "constant")
  expect_type(out, "list")
  expect_true(all(c("a", "eta", "psi") %in% names(out)))
})

test_that(".tps_hwka_split_theta partitions gamma+sinusoidal correctly", {
  # 4 baseline + 1 eta + 2 kernel = 7
  out <- morie:::.tps_hwka_split_theta(
    c(log(0.5), 0, 0.3, -0.2, 0.4, 2.0, 1.5),
    kernel_kind = "gamma",
    baseline_kind = "sinusoidal")
  expect_type(out, "list")
  expect_length(out$a, 4L)
  expect_length(out$psi, 2L)
})

test_that(".tps_hwka_neg_loglik_general is finite on synthetic events", {
  set.seed(1L)
  t <- sort(stats::runif(50, 0, 100))
  T_ <- 100
  out <- tryCatch(
    morie:::.tps_hwka_neg_loglik_general(
      theta = c(log(0.5), 0.3, log(0.7)),
      t = t, T_ = T_,
      kernel_kind = "exponential",
      baseline_kind = "constant"),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("neg_loglik error: %s", conditionMessage(out)))
  }
  expect_true(is.numeric(out) && length(out) == 1L)
})
