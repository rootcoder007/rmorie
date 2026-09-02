# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2WW: tests for internal helpers across tps_temporal.R,
# survival.R, psymet.R, mrm_otis.R, modules.R, entheo_preprocess.R,
# entheo_data.R, db_indexes.R, crypto_hybrid.R, cpads.R.

# ================================================================ tps_temporal.R

test_that(".tps_temporal_result builds a morie_tps_temporal_result list", {
  out <- rmorie:::.tps_temporal_result(title = "Test", call = "demo",
                                        summary_lines = list(N = 5))
  expect_type(out, "list")
  expect_s3_class(out, "morie_tps_temporal_result")
  expect_s3_class(out, "morie_rich_result")
})

test_that(".tps_temporal_fmt_round rounds with NA on non-finite", {
  expect_equal(rmorie:::.tps_temporal_fmt_round(3.14159, 2L), 3.14)
  expect_true(is.na(rmorie:::.tps_temporal_fmt_round(NA, 2L)))
  expect_true(is.na(rmorie:::.tps_temporal_fmt_round(Inf, 2L)))
})

test_that(".tps_temporal_monthly aggregates OCC_YEAR/MONTH/DAY into months", {
  df <- data.frame(OCC_YEAR = c(2024, 2024, 2024),
                   OCC_MONTH = c(1, 1, 2),
                   OCC_DAY = c(5, 20, 1))
  out <- rmorie:::.tps_temporal_monthly(df)
  expect_named(out, c("dates", "counts"))
  expect_length(out$dates, 2L)
  expect_equal(sort(out$counts), c(1L, 2L))
})

test_that(".tps_temporal_monthly on empty input returns zero-row list", {
  out <- rmorie:::.tps_temporal_monthly(data.frame(x = integer(0)))
  expect_length(out$counts, 0L)
})

# ==================================================================== survival.R

# ==================================================================== psymet.R

test_that(".has_psych returns logical", {
  expect_type(rmorie:::.has_psych(), "logical")
})

test_that(".psych_or_stop errors with package name in message when absent", {
  if (requireNamespace("psych", quietly = TRUE)) {
    expect_silent(rmorie:::.psych_or_stop("alpha"))
  } else {
    expect_error(rmorie:::.psych_or_stop("alpha"), regexp = "psych")
  }
})

test_that(".as_item_matrix coerces to double + names columns i1..iN", {
  df <- data.frame(a = 1:3, b = 4:6)
  out <- rmorie:::.as_item_matrix(df)
  expect_true(is.matrix(out))
  expect_equal(storage.mode(out), "double")
  expect_equal(colnames(out), c("a", "b"))
  # Unnamed input gets default i1..iN names.
  out2 <- rmorie:::.as_item_matrix(matrix(1:6, 3L, 2L))
  expect_equal(colnames(out2), c("i1", "i2"))
})

# ==================================================================== mrm_otis.R

test_that(".gini_int returns 0 on uniform input", {
  expect_equal(rmorie:::.gini_int(rep(5, 10L)), 0)
})

test_that(".gini_int returns NA on empty or all-zero", {
  expect_true(is.na(rmorie:::.gini_int(numeric(0))))
  expect_true(is.na(rmorie:::.gini_int(rep(0, 5L))))
})

test_that(".gini_int approaches 1 on skewed input", {
  expect_true(rmorie:::.gini_int(c(rep(0, 99L), 1000)) > 0.9)
})

test_that(".hill_mle returns positive alpha on a Pareto-ish sample", {
  set.seed(1L)
  x <- 1L + as.integer(stats::rexp(200L, rate = 0.5))
  alpha <- rmorie:::.hill_mle(x, x_min = 1L)
  expect_true(is.finite(alpha) && alpha > 0)
})

test_that(".hill_mle returns NA when <2 events above x_min", {
  expect_true(is.na(rmorie:::.hill_mle(c(1L), x_min = 1L)))
})

test_that(".cramer_v returns NA on a 1xN / Nx1 table", {
  expect_true(is.na(rmorie:::.cramer_v(matrix(1:4, 1L, 4L))))
})

test_that(".cramer_v returns a positive scalar on a 2x2 association", {
  tbl <- matrix(c(50, 10, 10, 50), 2L, 2L)
  out <- rmorie:::.cramer_v(tbl)
  expect_true(is.numeric(out) && out > 0)
})

# ==================================================================== modules.R

test_that(".cpads_default_csv returns a (possibly nonexistent) path", {
  expect_type(rmorie:::.cpads_default_csv(), "character")
})

test_that(".resolve_cpads_csv returns an absolute path when file exists", {
  tmp <- tempfile("cpads_demo_", fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("a,b\n1,2", tmp)
  out <- rmorie:::.resolve_cpads_csv(tmp)
  expect_match(out, "cpads_demo_")
})

test_that(".resolve_cpads_csv errors when nothing matches", {
  expect_error(
    rmorie:::.resolve_cpads_csv(
      tempfile("nonexistent_cpads_", fileext = ".csv")),
    regexp = "CPADS CSV not found")
})

test_that(".write_module_outputs pass-through when output_dir is NULL", {
  outs <- list(t1 = data.frame(a = 1:3))
  expect_identical(rmorie:::.write_module_outputs(outs, NULL), outs)
})

test_that(".write_module_outputs writes data.frame + character outputs", {
  outs <- list(report.csv = data.frame(a = 1:2),
               notes = "hello")
  dir <- tempfile("modout_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  rmorie:::.write_module_outputs(outs, dir)
  expect_true(file.exists(file.path(dir, "report.csv")))
  expect_true(file.exists(file.path(dir, "notes.txt")))
})

# ============================================================ entheo_preprocess.R

test_that(".entheo_bandpass returns matrix of same shape", {
  set.seed(2L)
  x <- matrix(stats::rnorm(64L * 256L), 64L, 256L)
  out <- rmorie:::.entheo_bandpass(x, sfreq = 250, low = 1, high = 40)
  expect_true(is.matrix(out))
  expect_equal(dim(out), dim(x))
})

test_that(".entheo_notch returns matrix of same shape", {
  set.seed(3L)
  x <- matrix(stats::rnorm(32L * 256L), 32L, 256L)
  out <- rmorie:::.entheo_notch(x, sfreq = 250, freq = 50)
  expect_true(is.matrix(out))
  expect_equal(dim(out), dim(x))
})

test_that(".entheo_asr_trim returns arr + n_bad list", {
  x <- matrix(c(1, 2, 100, 4, 5, 6, 7, 8, 9), 3L, 3L)
  out <- rmorie:::.entheo_asr_trim(x, threshold = 2.0)
  expect_named(out, c("arr", "n_bad"))
  expect_true(is.numeric(out$n_bad))
})

# =============================================================== entheo_data.R

test_that(".entheo_list_subjects returns char(0) when dir is absent", {
  out <- rmorie:::.entheo_list_subjects(tempfile("nodir_"))
  expect_equal(out, character(0))
})

test_that(".entheo_synthetic_record builds a structured record", {
  out <- rmorie:::.entheo_synthetic_record(subject_id = "01",
                                            n_tp = 64L,
                                            n_chan = 16L,
                                            n_parcels = 50L)
  expect_named(out, c("subject_id", "condition_order", "eeg",
                      "fmri", "behavioural", ".synthetic"))
  expect_true(out$.synthetic)
  expect_equal(dim(out$eeg$data_dmt), c(16L, 64L))
})

test_that(".entheo_load_real returns NULL on missing files", {
  out <- rmorie:::.entheo_load_real("99", tempfile("nodir_"))
  expect_null(out)
})

# =============================================================== db_indexes.R

test_that(".morie_db_index_registry has entries for SIU + b01 + uof_main", {
  reg <- rmorie:::.morie_db_index_registry()
  expect_type(reg, "list")
  for (k in c("SIU", "b01", "uof_main_records"))
    expect_true(k %in% names(reg))
})

test_that(".morie_db_indexes_tps_crime returns the canonical TPS spec", {
  out <- rmorie:::.morie_db_indexes_tps_crime()
  expect_type(out, "list")
  # Each entry should have name_suffix + cols.
  for (entry in out) {
    expect_true(all(c("name_suffix", "cols") %in% names(entry)))
  }
})

test_that(".morie_db_indexes_for resolves table to its index spec", {
  out <- rmorie:::.morie_db_indexes_for("b01")
  expect_type(out, "list")
  # 'OTIS_b01' namespace prefix should also resolve.
  out2 <- rmorie:::.morie_db_indexes_for("OTIS_b01")
  expect_type(out2, "list")
})

# ============================================================== crypto_hybrid.R

test_that(".morie_require_sodium silent / errors based on availability", {
  skip_if_not_installed("sodium")
  if (requireNamespace("sodium", quietly = TRUE)) {
    expect_silent(rmorie:::.morie_require_sodium())
  } else {
    expect_error(rmorie:::.morie_require_sodium(),
                 regexp = "sodium")
  }
})

test_that(".morie_wrapping_key returns 32-byte raw via HKDF when openssl present", {
  skip_if_not_installed("openssl")
  if (!requireNamespace("openssl", quietly = TRUE)) {
    skip("openssl not installed")
  }
  kem_ct <- as.raw(rep(0x01, 32L))
  pk     <- as.raw(rep(0x02, 32L))
  out <- rmorie:::.morie_wrapping_key(kem_ct, pk)
  expect_true(is.raw(out))
  expect_length(out, 32L)
})

# =================================================================== cpads.R

test_that(".morie_cpads_to_numeric coerces silently with NA on non-numeric", {
  expect_equal(rmorie:::.morie_cpads_to_numeric(c("1", "2", "x")),
               c(1, 2, NA))
})

test_that(".morie_cpads_recode_yn maps 1->1, 2->0, 98/99->NA", {
  out <- rmorie:::.morie_cpads_recode_yn(c(1, 2, 98, 99, 1, NA))
  expect_equal(out, c(1, 0, NA, NA, 1, NA))
})

test_that(".morie_cpads_strip_dknr replaces 98/99 with NA", {
  out <- rmorie:::.morie_cpads_strip_dknr(c(1, 2, 98, 99, 5))
  expect_equal(out, c(1, 2, NA, NA, 5))
})
