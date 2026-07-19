# SPDX-License-Identifier: AGPL-3.0-or-later
# rmorie <- bricklayer SIU integration: full-schema parse + corpus-first
# loader. Offline; skipped wherever the foundation packages are absent.

test_that("morie_siu_parse_report delegates to the compiled parser", {
  skip_if_not_installed("rmoriebricklayer")
  skip_if(!exists("bricklayer_parse_siu",
                  envir = asNamespace("rmoriebricklayer")),
          "rmoriebricklayer too old (< 0.3.5, no compiled SIU parser)")
  fx <- system.file("extdata", "siu_synthetic_report.html",
                    package = "rmoriebricklayer")
  f <- morie_siu_parse_report(fx)
  expect_equal(unname(f["number_of_subject_officers"]), "2")
  expect_equal(unname(f["directors_name"]), "Joseph Martino")
  expect_length(f, 17L)  # 16 schema fields + _language
})

test_that("morie_siu_reports loads the reviewed corpus offline", {
  skip_if_not_installed("rmoriedata")
  df <- morie_siu_reports()
  expect_gt(nrow(df), 2000L)
  expect_equal(ncol(df), 65L)
  expect_true("panel_reviewed" %in% names(df))
})

test_that("scraper prefill uses the compiled parser when available", {
  skip_if_not_installed("rmoriebricklayer")
  skip_if(!exists("bricklayer_parse_siu",
                  envir = asNamespace("rmoriebricklayer")),
          "rmoriebricklayer too old (< 0.3.5)")
  fx <- system.file("extdata", "siu_synthetic_report.html",
                    package = "rmoriebricklayer")
  html <- paste(readLines(fx, warn = FALSE), collapse = "\n")
  rec <- .siu_fetch_parse_case_page(html, "23-OFD-001", "http://example.org")
  expect_equal(rec$police_service, "Barrie Police Service")
  expect_equal(rec$incident_iso, "2023-01-05")
  expect_equal(rec$notification_iso, "2023-01-06")
  expect_equal(rec$decision_iso, "2023-04-28")
})
