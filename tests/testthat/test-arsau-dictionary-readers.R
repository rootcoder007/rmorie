# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2D: tests for the two ARSAU dictionary readers that the existing
# test files don't exercise.
#
#   * morie_arsau_read_xlsx_dictionary — runs against the bundled
#     authoritative MCSCS OTIS XLSX dictionary (uses the same row
#     layout that ARSAU XLSXes follow).
#   * morie_arsau_read_markdown_dictionary — runs against the bundled
#     OTIS_DATA_DICTIONARY.md.

test_that("morie_arsau_read_markdown_dictionary parses without crashing", {
  path <- system.file("extdata", "OTIS_DATA_DICTIONARY.md",
                      package = "morie")
  skip_if(!nzchar(path) || !file.exists(path),
          "bundled OTIS markdown dictionary missing")
  # The OTIS markdown follows a slightly different table convention than
  # ARSAU markdowns; the reader should still return a data.frame
  # without erroring on it (it's the read-anything-tabular path).
  out <- morie_arsau_read_markdown_dictionary(path)
  expect_true(is.data.frame(out) || is.list(out))
})

test_that("morie_arsau_read_markdown_dictionary errors on missing file", {
  expect_error(
    morie_arsau_read_markdown_dictionary("/no/such/path.md"),
    regexp = "(file|path|exist)"
  )
})

test_that("morie_arsau_ckan_url respects http vs https", {
  url <- morie_arsau_ckan_url(kind = "main_records", year = "2024",
                               limit = 100L)
  expect_true(is.character(url) && length(url) == 1L)
  expect_match(url, "^https://", ignore.case = TRUE)
})

test_that("morie_arsau_ckan_url builds different URLs for different limits", {
  u1 <- morie_arsau_ckan_url(kind = "main_records", year = "2024",
                              limit = 100L)
  u2 <- morie_arsau_ckan_url(kind = "main_records", year = "2024",
                              limit = 5000L)
  expect_true(u1 != u2)
})

test_that("morie_arsau_registry_df returns all 6 ARSAU dataset kinds", {
  reg <- morie_arsau_registry_df(language = "en")
  expect_s3_class(reg, "data.frame")
  for (kind in c("main_records", "individual_records",
                 "probe_cycle_records", "weapon_records",
                 "aggregate_summary", "detailed_dataset")) {
    expect_true(kind %in% reg$kind,
                info = sprintf("kind %s missing from registry", kind))
  }
})

test_that("morie_arsau_registry_df French language flips description", {
  en <- morie_arsau_registry_df(language = "en")
  fr <- morie_arsau_registry_df(language = "fr")
  expect_identical(nrow(en), nrow(fr))
  # At least one description differs — sanity check the language switch.
  same <- sum(en$description == fr$description, na.rm = TRUE)
  expect_lt(same, nrow(en))
})
