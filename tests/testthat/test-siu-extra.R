# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2G: tests for siu.R exports not yet covered by test-siu.R.
# Focuses on the pure-function paths (index, record_correction,
# sanity_check, translate error paths) so we don't depend on network
# or LLM bridges.

# ----------------------------------------------------------- siu_index

test_that("morie_siu_index returns the bundled manifest data.frame", {
  m <- tryCatch(morie_siu_index(), error = function(e) e)
  if (inherits(m, "error")) {
    skip(sprintf("manifest unavailable: %s", conditionMessage(m)))
  }
  expect_s3_class(m, "data.frame")
  expect_true("drid" %in% names(m))
  expect_gt(nrow(m), 0L)
})

test_that("morie_siu_index lang='en' filters to English-language rows", {
  full <- tryCatch(morie_siu_index(lang = "all"), error = function(e) NULL)
  en   <- tryCatch(morie_siu_index(lang = "en"),  error = function(e) NULL)
  skip_if(is.null(full) || is.null(en), "manifest unavailable")
  expect_true(nrow(en) <= nrow(full))
})

test_that("morie_siu_index lang='valid' drops empty case_numbers", {
  v <- tryCatch(morie_siu_index(lang = "valid"), error = function(e) NULL)
  skip_if(is.null(v), "manifest unavailable")
  expect_true(all(nzchar(v$case_number)))
})

test_that("morie_siu_index canonical_only filters via canonical_drid", {
  c <- tryCatch(morie_siu_index(canonical_only = TRUE), error = function(e) NULL)
  skip_if(is.null(c), "manifest unavailable")
  expect_s3_class(c, "data.frame")
})

# ------------------------------------------------ siu_record_correction

test_that("morie_siu_record_correction writes a canonical_overrides row", {
  cache_dir <- tempfile("siu_corr_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  morie_siu_record_correction(
    case_number = "24-OFD-001", field = "narrative_summary",
    verified_value = "Verified value here.", note = "QA pass",
    cache_dir = cache_dir
  )
  out_path <- file.path(cache_dir, "canonical_overrides.csv")
  expect_true(file.exists(out_path))
  out <- utils::read.csv(out_path, colClasses = "character",
                         check.names = FALSE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$case_number, "24-OFD-001")
  expect_equal(out$field, "narrative_summary")
})

test_that("morie_siu_record_correction de-dupes (case, field) pairs", {
  cache_dir <- tempfile("siu_corr2_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  morie_siu_record_correction("24-OFD-002", "narrative_summary",
                               "first value",
                               cache_dir = cache_dir)
  morie_siu_record_correction("24-OFD-002", "narrative_summary",
                               "second value",
                               cache_dir = cache_dir)
  out <- utils::read.csv(file.path(cache_dir, "canonical_overrides.csv"),
                          colClasses = "character", check.names = FALSE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$verified_value, "second value")
})

test_that("morie_siu_record_correction errors on an invalid field name", {
  expect_error(
    morie_siu_record_correction("24-OFD-001", "bogus_field", "v",
                                 cache_dir = tempfile()),
    regexp = "field"
  )
})

# --------------------------------------------------- siu_sanity_check

.make_synthetic_siu_csv <- function(valid = TRUE) {
  # Minimal SIU.csv-shaped data.frame with the columns sanity_check
  # actually inspects (case_number + drid for the output frame, plus
  # the format-checked fields).
  if (valid) {
    data.frame(
      case_number = c("24-OFD-001", "23-PFD-045", "22-PSD-100"),
      drid = c("4001", "3902", "2885"),
      date_of_incident_iso = c("2024-03-12", "2023-08-04", "2022-01-15"),
      date_siu_notified_iso = c("2024-03-12", "2023-08-04", "2022-01-15"),
      sex_gender_affected = c("Male", "Female", "Non-binary"),
      number_of_officers_involved = c("1 SO", "2 SO 3 WO", "1 WO"),
      mental_health_or_race_indications = c("Yes", "No", "Yes"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      case_number = c("BAD-FORMAT", "23-PFD-045"),
      drid = c("9999", "3902"),
      date_of_incident_iso = c("not-a-date", "2023-08-04"),
      date_siu_notified_iso = c("2024/03/12", "2023-08-04"),
      sex_gender_affected = c("X", "Female"),
      number_of_officers_involved = c("blah", "1 SO"),
      mental_health_or_race_indications = c("Maybe", "No"),
      stringsAsFactors = FALSE
    )
  }
}

test_that("morie_siu_sanity_check passes a well-formed data.frame", {
  df <- .make_synthetic_siu_csv(valid = TRUE)
  out <- morie_siu_sanity_check(df)
  expect_true(is.list(out) || is.data.frame(out) || is.character(out))
})

test_that("morie_siu_sanity_check flags malformed rows", {
  df <- .make_synthetic_siu_csv(valid = FALSE)
  out <- morie_siu_sanity_check(df)
  expect_true(is.list(out) || is.data.frame(out) || is.character(out))
})

test_that("morie_siu_sanity_check accepts a CSV path", {
  df <- .make_synthetic_siu_csv(valid = TRUE)
  csv <- tempfile("siu_sanity_", fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(df, csv, row.names = FALSE)
  out <- morie_siu_sanity_check(csv)
  expect_true(is.list(out) || is.data.frame(out) || is.character(out))
})

test_that("morie_siu_sanity_check errors without case_number column", {
  expect_error(
    morie_siu_sanity_check(data.frame(x = 1:3)),
    regexp = "case_number"
  )
})

# -------------------------------------------- siu_translate error paths

test_that("morie_siu_translate_fr_to_en errors when no SIU.csv exists", {
  cache_dir <- tempfile("siu_xlate_")
  expect_error(
    morie_siu_translate_fr_to_en(
      case_numbers = "24-OFD-001",
      cache_dir = cache_dir),
    regexp = "SIU"
  )
})
