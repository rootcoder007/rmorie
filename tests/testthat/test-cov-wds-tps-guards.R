# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Coverage for the 2026-07-22 CI-green hardening batch:
#   * .morie_dataset_http_post_json non-JSON (HTML error page) branch
#   * StatCan WDS malformed-response / embargo-message guards
#   * statcan_vectors v-prefix strip + invalid-ID error
#   * .morie_tps_parse_datetime (ISO vs US sample-CSV formats)
#   * .tps_hwka_events_to_days routing through the shared parser

test_that("POST helper reports non-JSON service error pages clearly", {
  # The non-JSON branch lives on the compiled libcurl path; mocking
  # .morie_http_post requires the binding to exist (compiled install).
  skip_if_not(exists(".morie_http_post", where = asNamespace("rmorie"),
                     mode = "function"),
              "compiled .morie_http_post backend not available")
  testthat::local_mocked_bindings(
    .morie_http_post = function(url, body, content_type, timeout_s,
                                 headers) {
      "<html><body><h1>503 Service Unavailable</h1></body></html>"
    },
    .package = "rmorie"
  )
  expect_error(
    rmorie:::.morie_dataset_http_post_json("https://example.org/api",
                                          body = list(a = 1L)),
    "non-JSON.*503 Service Unavailable"
  )
})

test_that("cube_metadata surfaces WDS embargo message envelopes", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, timeout_s = 30L) {
      list(message = "The product is not released yet")
    },
    .package = "rmorie"
  )
  expect_error(
    morie_datasets_statcan_cube_metadata(35100177L),
    "StatCan WDS: The product is not released yet"
  )
})

test_that("cube_metadata rejects empty and non-list malformed responses", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, timeout_s = 30L) {
      list()
    },
    .package = "rmorie"
  )
  expect_error(morie_datasets_statcan_cube_metadata(35100177L),
               "empty or malformed")

  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, timeout_s = 30L) {
      list("<html>not an envelope</html>")
    },
    .package = "rmorie"
  )
  expect_error(morie_datasets_statcan_cube_metadata(35100177L),
               "empty or malformed")
})

test_that("statcan_vectors strips v-prefix and rejects non-numeric IDs", {
  seen_body <- NULL
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, timeout_s = 30L) {
      seen_body <<- body
      list(list(status = "SUCCESS",
                object = list(vectorId = body[[1L]]$vectorId,
                              vectorDataPoint = list())))
    },
    .package = "rmorie"
  )
  morie_datasets_statcan_vectors(c("v41690973", "41690974"), n_periods = 2L)
  expect_equal(vapply(seen_body, function(b) b$vectorId, integer(1L)),
               c(41690973L, 41690974L))
  expect_true(all(vapply(seen_body, function(b) b$latestN, integer(1L)) == 2L))

  expect_error(morie_datasets_statcan_vectors("not-a-vector"),
               "must be numeric or 'v'-prefixed")
})

test_that("full_csv_url rejects non-list responses (HTML outage pages)", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, ...) "<html>outage</html>",
    .package = "rmorie"
  )
  expect_error(morie_datasets_statcan_full_csv_url(35100177L),
               "getFullTableDownloadCSV status=NULL")
})

test_that(".morie_tps_parse_datetime handles ISO, US, POSIXct, garbage", {
  parse <- rmorie:::.morie_tps_parse_datetime

  # POSIXct passthrough — returned unchanged
  now <- as.POSIXct("2026-07-22 10:00:00", tz = "UTC")
  expect_identical(parse(now), now)

  # ISO 8601 (live PSDP ArcGIS export shape, incl. T + millis + Z)
  iso <- parse("2017-05-20 19:05:58")
  expect_s3_class(iso, "POSIXct")
  expect_false(is.na(iso))
  live <- parse("2017-05-20T19:05:58.000Z")
  expect_equal(format(live, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
               "2017-05-20 19:05:58")
  expect_equal(format(parse("2017-05-20"), "%Y-%m-%d", tz = "UTC"),
               "2017-05-20")

  # US m/d/Y h:m:s AM/PM (bundled sample CSVs)
  us <- parse("4/15/2014 4:00:00 AM")
  expect_false(is.na(us))
  expect_equal(format(us, "%Y-%m-%d %H:%M", tz = "UTC"), "2014-04-15 04:00")
  pm <- parse("12/31/1999 11:59:59 PM")
  expect_equal(format(pm, "%Y-%m-%d %H:%M", tz = "UTC"), "1999-12-31 23:59")

  # US date-only fallback
  d <- parse("4/15/2014")
  expect_false(is.na(d))
  expect_equal(format(d, "%Y-%m-%d", tz = "UTC"), "2014-04-15")

  # Garbage returns NA, never throws (as.POSIXct alone would error)
  expect_true(is.na(parse("not a date")))

  # Mixed vector: each element parsed by its own format
  mix <- parse(c("2017-05-20 19:05:58", "4/15/2014 4:00:00 AM", "junk"))
  expect_equal(is.na(mix), c(FALSE, FALSE, TRUE))
})

test_that(".tps_hwka_events_to_days parses US-format OCC_DATE samples", {
  df <- data.frame(
    OCC_DATE = c("4/15/2014 4:00:00 AM", "4/16/2014 5:30:00 PM",
                 "4/20/2014 12:00:00 PM", "not a date"),
    stringsAsFactors = FALSE
  )
  out <- rmorie:::.tps_hwka_events_to_days(df, max_n = 10L)
  expect_type(out$t, "double")
  expect_length(out$t, 3L)  # NA row dropped
  expect_false(is.unsorted(out$t))
  expect_equal(out$T_, out$t[length(out$t)])

  expect_error(
    rmorie:::.tps_hwka_events_to_days(data.frame(x = 1), max_n = 10L),
    "no OCC_DATE or REPORT_DATE"
  )
})
