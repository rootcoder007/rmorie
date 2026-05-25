# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for the generic open-data access layer (R/data_access.R):
# morie_fetch, morie_ckan_search, morie_fetch_arcgis, morie_siu_directors_reports.
# Pure helpers are tested offline; the live catchers are network-gated
# and skipped on CRAN / offline machines.

test_that(".morie_url_with_params builds and encodes query strings", {
  f <- morie:::.morie_url_with_params
  expect_equal(f("http://x/a"), "http://x/a")
  expect_equal(f("http://x/a", NULL), "http://x/a")
  expect_equal(f("http://x/a", list(q = "b")), "http://x/a?q=b")
  expect_equal(f("http://x/a?p=1", list(q = "b")), "http://x/a?p=1&q=b")
  expect_true(grepl("q=a%20b%26c", f("http://x/a", list(q = "a b&c"))))
  # NULL-valued params are dropped.
  expect_equal(f("http://x/a", list(q = "b", r = NULL)), "http://x/a?q=b")
})

test_that(".morie_ckan_portal resolves names and passes through URLs", {
  f <- morie:::.morie_ckan_portal
  expect_equal(f("open.canada.ca"), "https://open.canada.ca/data/en")
  expect_equal(f("data.ontario.ca"), "https://data.ontario.ca")
  expect_equal(
    f("https://catalogue.example.org/"),
    "https://catalogue.example.org"
  )
  expect_error(f("not-a-portal"))
})

test_that(".morie_detect_format falls back to the URL extension", {
  f <- morie:::.morie_detect_format
  # file:// URLs carry no Content-Type header -> extension fallback.
  expect_equal(f("file:///tmp/x.csv"), "csv")
  expect_equal(f("file:///tmp/x.json"), "json")
  expect_equal(f("file:///tmp/x.xlsx"), "xlsx")
  expect_equal(f("file:///tmp/x.zip"), "zip")
  expect_equal(f("file:///tmp/x.xml"), "xml")
  expect_equal(f("file:///tmp/x.unknownext"), "csv") # last-resort default
})

test_that("morie_fetch reads csv and json over file://", {
  skip_if_not_installed("jsonlite")
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = letters[1:3]), csv,
    row.names = FALSE
  )
  on.exit(unlink(csv), add = TRUE)
  d <- morie_fetch(paste0("file://", csv)) # auto-detected
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 3L)
  expect_equal(nrow(morie_fetch(paste0("file://", csv), format = "csv")), 3L)

  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  js <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(x = 1:2)), js)
  on.exit(unlink(js), add = TRUE)
  jr <- morie_fetch(paste0("file://", js), format = "json")
  expect_equal(as.integer(jr$x), 1:2)
})

test_that("morie_fetch extracts a member from a zip over file://", {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  csv <- tempfile("dl-", fileext = ".csv")
  utils::write.csv(data.frame(a = 1:4), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  zp <- tempfile("dl-", fileext = ".zip")
  on.exit(unlink(zp), add = TRUE)
  withr::local_dir(dirname(csv))
  utils::zip(zp, basename(csv), flags = "-q")
  z <- morie_fetch(paste0("file://", zp),
    format = "zip",
    zip_member = basename(csv)
  )
  expect_equal(nrow(z), 4L)
  # A zip fetch with no member named is an error.
  expect_error(morie_fetch(paste0("file://", zp), format = "zip"))
})

test_that("TPS catalog entries carry verified ArcGIS layer URLs", {
  cat <- morie_dataset_catalog()
  tps <- cat[cat$source == "tps", ]
  expect_equal(nrow(tps), 3L)
  expect_true(all(nzchar(tps$arcgis_url)))
  expect_true(all(grepl("/FeatureServer/0$", tps$arcgis_url)))
})

test_that("morie_ckan_search returns resource rows (network)", {
  testthat::skip_if_offline("open.canada.ca")
  hits <- tryCatch(morie_ckan_search("cannabis", rows = 3),
    error = function(e) NULL
  )
  skip_if(is.null(hits), "CKAN package_search unreachable")
  expect_s3_class(hits, "data.frame")
  expect_true(all(c("dataset_title", "resource_id", "format") %in%
    names(hits)))
})

test_that("morie_fetch_arcgis paginates a FeatureServer layer (network)", {
  testthat::skip_if_offline("services.arcgis.com")
  layer <- paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/",
    "rest/services/Homicides_Open_Data_ASR_RC_TBL_002/",
    "FeatureServer/0"
  )
  df <- tryCatch(morie_fetch_arcgis(layer, max_records = 30),
    error = function(e) NULL
  )
  skip_if(is.null(df), "ArcGIS layer unreachable")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0 && nrow(df) <= 30)
})
