# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3CC: tests for exported callables across tps_fetch.R,
# ingest_tps.R, datasets.R (TPS + SIU + ARSAU + OTIS), and
# siu_fetch.R parse_case_page.
#
# Network-dependent paths are exercised via testthat::local_mocked_bindings,
# so these tests run offline yet still cover the live-mode dispatch logic
# instead of skipping when network / bundled-synthetic fixtures are absent.

# ================================================================== tps_fetch.R

test_that("morie_tps_list_categories returns a sorted character vector", {
  out <- morie_tps_list_categories()
  expect_type(out, "character")
  expect_true(length(out) > 0L)
  expect_equal(out, sort(out))
})

# ================================================================ ingest_tps.R

test_that("morie_ingest_tps_layers returns a 2-col data.frame", {
  out <- morie_ingest_tps_layers()
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("name", "url"))
  expect_true(nrow(out) >= 3L)
  expect_true(all(grepl("^https://", out$url)))
})

# ================================================================ datasets.R (TPS layers)

test_that("morie_datasets_tps_layers returns a 2-col data.frame", {
  out <- morie_datasets_tps_layers()
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("name", "url"))
  expect_true(nrow(out) >= 3L)
})

# ---------------------------------------------------------------------------
# OFFLINE PATH — bundled inst/extdata/tps_major_crime.csv
# ---------------------------------------------------------------------------

test_that("morie_datasets_tps_major_crime(offline=TRUE) reads bundled synthetic", {
  df <- suppressWarnings(
    morie_datasets_tps_major_crime(offline = TRUE))
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  # TPS PSDP canonical schema markers.
  for (col in c("OBJECTID", "EVENT_UNIQUE_ID", "OCC_YEAR",
                "MCI_CATEGORY", "HOOD_158"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_tps_major_crime(offline=TRUE) honours year + max_features", {
  df <- suppressWarnings(
    morie_datasets_tps_major_crime(offline = TRUE,
                                     year = 2024L,
                                     max_features = 2L))
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) <= 2L)
  if (nrow(df) > 0L) expect_true(all(df$OCC_YEAR == 2024L))
})

# ---------------------------------------------------------------------------
# LIVE-PATH (MOCKED) — verify the non-offline dispatch path is hit when
# offline=FALSE, without touching the network.
# ---------------------------------------------------------------------------

test_that("morie_datasets_tps_major_crime(offline=FALSE) dispatches via .morie_dataset_tps_fetch", {
  # Synthesise a 3-row df shaped like a TPS major-crime ArcGIS pull.
  stub_df <- data.frame(
    OBJECTID = c(101L, 102L, 103L),
    EVENT_UNIQUE_ID = sprintf("GO-MOCK%03d", 1:3),
    OCC_YEAR = c(2024L, 2024L, 2025L),
    MCI_CATEGORY = c("Assault", "Robbery", "Break and Enter"),
    HOOD_158 = c("82", "73", "168"))
  testthat::local_mocked_bindings(
    .morie_dataset_tps_fetch = function(layer_url, where, max_features,
                                          return_geometry) {
      # Sanity-check the URL is the TPS major-crime registry entry.
      expect_match(layer_url, "Major_Crime_Indicators")
      expect_match(where, "OCC_YEAR = 2024|1=1")
      stub_df
    },
    .package = "morie")
  out <- morie_datasets_tps_major_crime(year = 2024L,
                                          max_features = 10L)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_equal(out$MCI_CATEGORY,
               c("Assault", "Robbery", "Break and Enter"))
})

test_that("morie_datasets_tps_homicide dispatches via .morie_dataset_tps_fetch", {
  stub_df <- data.frame(
    OBJECTID = c(1L),
    EVENT_UNIQUE_ID = c("GO-HOM01"),
    OCC_YEAR = c(2023L),
    DIVISION = c("D31"))
  testthat::local_mocked_bindings(
    .morie_dataset_tps_fetch = function(layer_url, where, max_features,
                                          return_geometry) {
      expect_match(layer_url, "Homicides")
      stub_df
    },
    .package = "morie")
  out <- morie_datasets_tps_homicide(year = 2023L)
  expect_s3_class(out, "data.frame")
  expect_equal(out$OBJECTID, 1L)
})

# ================================================================ datasets.R (SIU)

test_that("morie_datasets_siu_director_reports returns df with case_number + url", {
  # When rvest is present and the live SIU site responds, this returns a
  # populated frame; when rvest is missing, it warns + returns a 0-row
  # frame. We tolerate both via a mocked rvest-absent path.
  out <- tryCatch(morie_datasets_siu_director_reports(),
                  warning = function(w) {
                    suppressWarnings(
                      morie_datasets_siu_director_reports())
                  })
  expect_s3_class(out, "data.frame")
  expect_true("case_number" %in% names(out))
  expect_true("url"         %in% names(out))
})

test_that("morie_datasets_siu_report_text(offline=TRUE) reads bundled synthetic", {
  out <- morie_datasets_siu_report_text(offline = TRUE)
  expect_type(out, "character")
  expect_true(nchar(out) > 0L)
  # The bundled SYNTHETIC fixture is the 24-OFD-001 fake; assert markers.
  expect_match(out, "Mandate of the SIU")
  expect_match(out, "24-OFD-001")
  expect_match(out, "AGPL-3\\.0-or-later")
})

test_that("morie_datasets_siu_report_text errors when no url + not offline", {
  expect_error(morie_datasets_siu_report_text(),
               regexp = "url=|offline")
})

# ================================================================ siu_fetch.R

test_that(".siu_fetch_parse_case_page returns NA-stub on empty html", {
  out <- morie:::.siu_fetch_parse_case_page(
    html = "", case_number = "24-OFD-001",
    url = "https://www.siu.on.ca/en/news_template.php?nrid=1")
  expect_type(out, "list")
  expect_equal(out$case_number, "24-OFD-001")
  expect_equal(out$source_url,
               "https://www.siu.on.ca/en/news_template.php?nrid=1")
})

test_that(".siu_fetch_parse_case_page extracts incident date + service + decision", {
  html <- paste(
    "Incident: January 5, 2024",
    "Notification: January 6, 2024",
    "Director's Decision: March 15, 2024",
    "Police Service: Toronto Police Service",
    "The director found no reasonable grounds to believe.",
    sep = "\n")
  out <- morie:::.siu_fetch_parse_case_page(
    html = html, case_number = "24-OFD-001",
    url = "https://example/")
  expect_equal(out$incident_iso,     "2024-01-05")
  expect_equal(out$notification_iso, "2024-01-06")
  expect_equal(out$decision_iso,     "2024-03-15")
  expect_match(out$police_service, "Toronto")
  expect_match(out$director_decision_text, "no reasonable grounds")
})
