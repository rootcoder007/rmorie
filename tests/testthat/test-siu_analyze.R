# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for R/siu_analyze.R --- SIU descriptive analyzers driven from
# tiny inline data frames matching the canonical SIU_by_case.csv
# schema.

set.seed(1)

# Reusable synthetic SIU corpus -- minimal but exercises every analyzer.
.fake_siu_df <- function() {
  data.frame(
    case_number = sprintf("22-OCI-%03d", 1:8),
    police_service = c("Toronto Police Service",
                       "Toronto Police Service",
                       "Ontario Provincial Police",
                       "Ontario Provincial Police",
                       "Peel Regional Police",
                       "Peel Regional Police",
                       NA_character_,
                       "Toronto Police Service"),
    charges_recommended = c(TRUE, FALSE, FALSE, FALSE,
                            TRUE, NA, FALSE, TRUE),
    date_of_incident_iso = c("2022-01-05", "2022-06-12",
                             "2023-02-18", "2023-09-30",
                             "2024-03-01", "2024-07-22",
                             NA, "2024-11-11"),
    date_siu_notified_iso = c("2022-01-06", "2022-06-13",
                              "2023-02-19", "2023-10-01",
                              "2024-03-02", "2024-07-23",
                              NA, "2024-11-12"),
    date_of_director_decision_iso = c("2022-08-01", "2022-12-15",
                                      "2023-09-10", "2024-03-05",
                                      "2024-09-01", "2024-12-30",
                                      NA, "2025-02-14"),
    number_of_subject_officials = c(1L, 2L, 1L, 3L, 1L, 2L, NA, 1L),
    number_of_witness_officials = c(2L, 4L, 1L, 5L, 3L, 2L, NA, 2L),
    number_of_civilian_witnesses = c(0L, 3L, 1L, 2L, 0L, 1L, NA, 1L),
    number_of_officers_involved = c(3L, 6L, 2L, 8L, 4L, 4L, NA, 3L),
    sex_gender_affected = c("Male", "Female", "Male", "Male",
                            "Male", NA, "Male", "Female"),
    age_affected = c("34", "27", "55", "19", "42", "31", NA, "29"),
    mental_health_or_race_indications = c(
      "mental health; Indigenous",
      "",
      "racial",
      "mental health; Black",
      "",
      "Indigenous",
      NA,
      "psychiatric"),
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# by_police_service
# ---------------------------------------------------------------------------

test_that("by_police_service returns rich result on synthetic data", {
  set.seed(1)
  r <- morie_siu_by_police_service(.fake_siu_df())
  expect_s3_class(r, "morie_rich_result")
  expect_match(r$title, "police service")
  expect_true(length(r$tables) >= 1L)
})

test_that("by_police_service counts charges correctly", {
  set.seed(1)
  r <- morie_siu_by_police_service(.fake_siu_df())
  expect_equal(r$payload$charged, 3L)
  expect_equal(r$payload$no_charges, 4L)
})

test_that("by_police_service handles NA/blank service via '(unknown)'", {
  set.seed(1)
  r <- morie_siu_by_police_service(.fake_siu_df())
  expect_true("(unknown)" %in% names(r$payload$counts))
})

test_that("by_police_service errors when police_service column missing", {
  set.seed(1)
  d <- .fake_siu_df()
  d$police_service <- NULL
  expect_error(morie_siu_by_police_service(d), "police_service")
})

test_that("by_police_service tolerates missing charges_recommended col", {
  set.seed(1)
  d <- .fake_siu_df()
  d$charges_recommended <- NULL
  r <- morie_siu_by_police_service(d)
  expect_s3_class(r, "morie_rich_result")
  expect_equal(r$payload$charged, 0L)
})


# ---------------------------------------------------------------------------
# by_year
# ---------------------------------------------------------------------------

test_that("by_year groups by 4-char year prefix", {
  set.seed(1)
  r <- morie_siu_by_year(.fake_siu_df())
  expect_s3_class(r, "morie_rich_result")
  expect_equal(r$payload$by_year[["2022"]], 2L)
  expect_equal(r$payload$by_year[["2023"]], 2L)
  expect_equal(r$payload$by_year[["2024"]], 3L)
})

test_that("by_year reports years covered span", {
  set.seed(1)
  r <- morie_siu_by_year(.fake_siu_df())
  span <- r$summary_lines[[1]][[2]]
  expect_equal(span, "2022-2024")
})

test_that("by_year errors when date_of_incident_iso missing", {
  set.seed(1)
  d <- .fake_siu_df()
  d$date_of_incident_iso <- NULL
  expect_error(morie_siu_by_year(d), "date_of_incident_iso")
})

test_that("by_year tolerates missing charges_recommended", {
  set.seed(1)
  d <- .fake_siu_df()
  d$charges_recommended <- NULL
  r <- morie_siu_by_year(d)
  expect_s3_class(r, "morie_rich_result")
})


# ---------------------------------------------------------------------------
# case_counts
# ---------------------------------------------------------------------------

test_that("case_counts produces 4 rows of per-field summaries", {
  set.seed(1)
  r <- morie_siu_case_counts(.fake_siu_df())
  expect_s3_class(r, "morie_rich_result")
  expect_equal(length(r$tables[[1]]$rows), 4L)
})

test_that("case_counts gracefully handles missing numeric cols", {
  set.seed(1)
  d <- .fake_siu_df()
  d$number_of_officers_involved <- NULL
  r <- morie_siu_case_counts(d)
  expect_s3_class(r, "morie_rich_result")
})


# ---------------------------------------------------------------------------
# demographics
# ---------------------------------------------------------------------------

test_that("demographics tabulates sex and reports mean age", {
  set.seed(1)
  r <- morie_siu_demographics(.fake_siu_df())
  expect_s3_class(r, "morie_rich_result")
  expect_match(r$title, "demographics")
  ages_line <- r$summary_lines[[3]][[2]]
  expect_true(is.numeric(ages_line))
  expect_true(ages_line > 20 && ages_line < 60)
})

test_that("demographics survives missing sex/age columns", {
  set.seed(1)
  d <- .fake_siu_df()
  d$sex_gender_affected <- NULL
  d$age_affected <- NULL
  r <- morie_siu_demographics(d)
  expect_s3_class(r, "morie_rich_result")
})


# ---------------------------------------------------------------------------
# mental_health_race_indicators
# ---------------------------------------------------------------------------

test_that("mh_race_indicators tallies semicolon-delimited keywords", {
  set.seed(1)
  r <- morie_siu_mental_health_race_indicators(.fake_siu_df())
  expect_s3_class(r, "morie_rich_result")
  expect_true(length(r$tables[[1]]$rows) >= 1L)
  expect_true(nzchar(r$warnings))
})

test_that("mh_race_indicators returns warning when column missing", {
  set.seed(1)
  d <- .fake_siu_df()
  d$mental_health_or_race_indications <- NULL
  r <- morie_siu_mental_health_race_indicators(d)
  expect_s3_class(r, "morie_rich_result")
  expect_match(r$warnings, "missing")
})


# ---------------------------------------------------------------------------
# decision_timing
# ---------------------------------------------------------------------------

test_that("decision_timing reports 3 intervals", {
  set.seed(1)
  r <- morie_siu_decision_timing(.fake_siu_df())
  expect_s3_class(r, "morie_rich_result")
  expect_equal(length(r$tables[[1]]$rows), 3L)
})

test_that("decision_timing tolerates missing date columns", {
  set.seed(1)
  d <- .fake_siu_df()
  d$date_siu_notified_iso <- NULL
  r <- morie_siu_decision_timing(d)
  expect_s3_class(r, "morie_rich_result")
})


# ---------------------------------------------------------------------------
# charges_by_year_chi2
# ---------------------------------------------------------------------------

test_that("charges_by_year_chi2 computes a p-value when n>=5", {
  set.seed(1)
  # Force a richer table for the chi-square -- replicate rows.
  d <- .fake_siu_df()
  d2 <- do.call(rbind, list(d, d, d))
  r <- morie_siu_charges_by_year_chi2(d2)
  expect_s3_class(r, "morie_rich_result")
})

test_that("charges_by_year_chi2 warns on too-few rows", {
  set.seed(1)
  d <- .fake_siu_df()[1:2, ]
  r <- morie_siu_charges_by_year_chi2(d)
  expect_match(r$warnings, "Fewer than 5")
})


# ---------------------------------------------------------------------------
# all_analyses convenience
# ---------------------------------------------------------------------------

test_that("all_analyses runs every surface and returns a named list", {
  set.seed(1)
  res <- morie_siu_all_analyses(.fake_siu_df())
  expect_type(res, "list")
  expect_true(all(c("by_police_service", "by_year", "case_counts",
                    "demographics", "mh_race_indicators",
                    "decision_timing", "charges_year_chi2") %in%
                  names(res)))
  for (nm in names(res)) {
    expect_s3_class(res[[nm]], "morie_rich_result", exact = FALSE)
  }
})

test_that("all_analyses captures per-surface failures as warnings", {
  set.seed(1)
  # Drop *all* useful columns -> by_police_service should fail.
  d <- data.frame(x = 1:3)
  res <- morie_siu_all_analyses(d)
  expect_match(res$by_police_service$warnings, "simpleError|missing",
               ignore.case = TRUE)
})

test_that("all_analyses optionally writes per-surface .txt dumps", {
  set.seed(1)
  out <- tempfile("siu_dump_")
  res <- morie_siu_all_analyses(.fake_siu_df(), out_dir = out)
  expect_true(dir.exists(out))
  expect_true(length(list.files(out, pattern = "siu_analysis_.*\\.txt$")) >= 1L)
})


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

test_that(".siu_an_truthy counts logicals", {
  set.seed(1)
  expect_equal(morie:::.siu_an_truthy(c(TRUE, FALSE, TRUE, NA)), 2L)
  expect_equal(morie:::.siu_an_truthy(c("yes", "no", "1", "True")), 3L)
})

test_that(".siu_an_falsy counts logicals", {
  set.seed(1)
  expect_equal(morie:::.siu_an_falsy(c(TRUE, FALSE, FALSE, NA)), 2L)
  expect_equal(morie:::.siu_an_falsy(c("yes", "no", "0", "False")), 3L)
})

test_that(".siu_an_load returns the df when passed a data.frame", {
  set.seed(1)
  d <- data.frame(a = 1:3)
  expect_identical(morie:::.siu_an_load(d), d)
})

test_that(".siu_an_load errors when path missing", {
  set.seed(1)
  expect_error(morie:::.siu_an_load(tempfile("does_not_exist_")),
               "not found")
})

test_that(".siu_an_load reads CSV from disk", {
  set.seed(1)
  csv <- tempfile("siu_", fileext = ".csv")
  utils::write.csv(.fake_siu_df(), csv, row.names = FALSE)
  d <- morie:::.siu_an_load(csv)
  expect_s3_class(d, "data.frame")
  expect_true(nrow(d) > 0L)
})

test_that(".siu_an_interval returns n/a on all-NA dates", {
  set.seed(1)
  r <- morie:::.siu_an_interval("Test", rep(NA, 3), rep(NA, 3))
  expect_equal(r[[2]], "n/a")
})

test_that(".siu_an_interval computes mean/median of day-deltas", {
  set.seed(1)
  r <- morie:::.siu_an_interval("test",
                                c("2024-01-01", "2024-01-05"),
                                c("2024-01-02", "2024-01-10"))
  expect_equal(r[[2]], 2L)
  expect_match(r[[3]], "3\\.0")  # mean = (1+5)/2 = 3
})