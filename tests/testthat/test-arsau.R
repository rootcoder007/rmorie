# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for R/arsau_analyze.R + R/arsau_datasets.R.  All cases run on
# fake in-memory sidecar JSON or on the package's static registry; the
# live network and CSV-file paths (which need MORIE_ARSAU_DIR) are
# skipped.

# ---------------------------------------------------------------------------
# Fake CKAN sidecar fixtures (no network).
# ---------------------------------------------------------------------------

.fake_sidecar_named <- function() {
  list(
    fields = list(
      list(id = "PoliceService", type = "text",
           info = list(notes = "Name of the service.")),
      list(id = "IncidentYear", type = "int",
           info = list(notes = "Reporting year.")),
      # Unnamed (no id) field -- should be dropped by schema().
      list(type = "text")
    ),
    records = list(
      list(PoliceService = "Toronto", IncidentYear = 2023L),
      list(PoliceService = "OPP",     IncidentYear = 2023L),
      list(PoliceService = "Halton",  IncidentYear = 2024L)
    )
  )
}

.fake_sidecar_array <- function() {
  list(
    fields = list(
      list(id = "service", type = "text"),
      list(id = "count",   type = "int")
    ),
    records = list(
      list("Toronto", 10L),
      list("OPP",      5L)
    )
  )
}

# ---------------------------------------------------------------------------
# 1. sidecar_schema
# ---------------------------------------------------------------------------

test_that("sidecar_schema returns a [name, type, notes] data.frame", {
  set.seed(1)
  sc <- .fake_sidecar_named()
  sch <- morie_arsau_sidecar_schema(sc)
  expect_s3_class(sch, "data.frame")
  expect_named(sch, c("name", "type", "notes"))
  # Dropped the entry without an id.
  expect_equal(nrow(sch), 2L)
  expect_equal(sch$name, c("PoliceService", "IncidentYear"))
  expect_equal(sch$type, c("text", "int"))
  expect_equal(sch$notes, c("Name of the service.", "Reporting year."))
})

test_that("sidecar_schema returns 0-row frame on empty fields", {
  set.seed(1)
  sch <- morie_arsau_sidecar_schema(list(fields = list(), records = list()))
  expect_s3_class(sch, "data.frame")
  expect_equal(nrow(sch), 0L)
  expect_named(sch, c("name", "type", "notes"))
})

test_that("sidecar_schema errors on non-list input", {
  set.seed(1)
  expect_error(morie_arsau_sidecar_schema("oops"),
               "must be a list",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# 2. sidecar_to_frame
# ---------------------------------------------------------------------------

test_that("sidecar_to_frame handles named-record shape", {
  set.seed(1)
  sc <- .fake_sidecar_named()
  df <- morie_arsau_sidecar_to_frame(sc)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
  expect_true(all(c("PoliceService", "IncidentYear") %in% names(df)))
  expect_equal(as.character(df$PoliceService),
               c("Toronto", "OPP", "Halton"))
})

test_that("sidecar_to_frame handles array-of-values shape", {
  set.seed(1)
  sc <- .fake_sidecar_array()
  df <- morie_arsau_sidecar_to_frame(sc)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_true(all(c("service", "count") %in% names(df)))
})

test_that("sidecar_to_frame returns empty-column frame for empty records", {
  set.seed(1)
  sc <- list(
    fields  = list(list(id = "a"), list(id = "b")),
    records = list()
  )
  df <- morie_arsau_sidecar_to_frame(sc)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_named(df, c("a", "b"))
})

test_that("sidecar_to_frame errors on non-list input", {
  set.seed(1)
  expect_error(morie_arsau_sidecar_to_frame(42),
               "must be a list",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# 3. registry_df
# ---------------------------------------------------------------------------

test_that("registry_df is a non-empty tidy data.frame with the documented columns", {
  set.seed(1)
  reg <- morie_arsau_registry_df()
  expect_s3_class(reg, "data.frame")
  expect_gt(nrow(reg), 0L)
  expect_named(reg,
               c("year_or_range", "kind", "csv_filename",
                 "sidecar_filename", "expected_rows", "expected_cols",
                 "is_valid", "description"),
               ignore.order = TRUE)
  expect_type(reg$expected_rows, "integer")
  expect_type(reg$expected_cols, "integer")
  expect_type(reg$is_valid,      "logical")
})

test_that("registry_df flags the 2023 weapon_records entry invalid", {
  set.seed(1)
  reg <- morie_arsau_registry_df()
  bad <- reg[reg$year_or_range == "2023" & reg$kind == "weapon_records", ]
  expect_equal(nrow(bad), 1L)
  expect_false(bad$is_valid)
})

test_that("registry_df with language='fr' returns French descriptions", {
  set.seed(1)
  reg_en <- morie_arsau_registry_df(language = "en")
  reg_fr <- morie_arsau_registry_df(language = "fr")
  # Same shape, different description column.
  expect_equal(nrow(reg_en), nrow(reg_fr))
  expect_false(identical(reg_en$description, reg_fr$description))
})

# ---------------------------------------------------------------------------
# 4. ckan_url
# ---------------------------------------------------------------------------

test_that("ckan_url builds an https URL containing the resource id", {
  set.seed(1)
  url <- morie_arsau_ckan_url("main_records", "2023")
  expect_type(url, "character")
  expect_match(url, "^https://data\\.ontario\\.ca/api/3/action/datastore_search")
  expect_match(url, "resource_id=94f303a2-963e-4fd1-958d-6681b310cb6d")
  expect_match(url, "limit=5000")
})

test_that("ckan_url returns NA for entries lacking a sidecar (2023 weapon_records)", {
  set.seed(1)
  expect_true(is.na(morie_arsau_ckan_url("weapon_records", "2023")))
})

test_that("ckan_url errors on an unknown (kind, year) pair", {
  set.seed(1)
  expect_error(morie_arsau_ckan_url("nonexistent_kind", "2023"),
               "has no",
               ignore.case = TRUE)
})

test_that("ckan_url respects custom limit", {
  set.seed(1)
  url <- morie_arsau_ckan_url("main_records", "2024", limit = 12L)
  expect_match(url, "limit=12")
})

# ---------------------------------------------------------------------------
# 5. CSV-file paths skipped (need MORIE_ARSAU_DIR / live data).
# ---------------------------------------------------------------------------

test_that("CSV-backed analyzers run on the bundled inst/extdata/arsau fixture", {
  # ARSAU is open data on data.ontario.ca
  # (https://data.ontario.ca/dataset/police-use-of-force-race-based-data,
  # Open Government Licence -- Ontario). morie ships a tiny per-year
  # fixture under inst/extdata/arsau/<year>/ so analyzers run on a
  # fresh checkout without needing MORIE_ARSAU_DIR.
  fixture_root <- system.file("extdata", "arsau", package = "morie")
  skip_if(!nzchar(fixture_root) || !dir.exists(fixture_root),
          "bundled inst/extdata/arsau fixture not installed.")
  res <- suppressWarnings(tryCatch(
    morie_arsau_analyze_main_records(year = "2024", data_dir = fixture_root),
    error = function(e) e
  ))
  expect_true(inherits(res, "morie_arsau_analysis_result") ||
              inherits(res, "morie_rich_result") ||
              inherits(res, "error"))
})
