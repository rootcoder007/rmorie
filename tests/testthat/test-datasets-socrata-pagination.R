# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3OO: SODA2 $offset pagination opt-in for
# .morie_dataset_socrata_fetch. Covers two layers:
#
#   (1) Core pagination logic in the helper itself, exercised by
#       mocking .morie_dataset_http_json (the layer below) so we can
#       observe each $offset request and stub the chunked responses.
#   (2) Wrapper plumbing -- chicago_crime + nyc_nypd_* + nyc_sqf each
#       forward `paginate` / `page_size` / `max_pages` verbatim to the
#       helper.

# =============================================================== (1) core

test_that("paginate=FALSE (default) issues a single non-paginated request", {
  calls <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      # 3-row stub frame.
      list(list(a = 1L), list(a = 2L), list(a = 3L))
    },
    .package = "morie")
  out <- morie:::.morie_dataset_socrata_fetch(
    "https://example.test/r.json",
    where = "year=2024", max_features = 50L)
  expect_equal(length(calls), 1L)
  q <- calls[[1L]]$query
  expect_equal(q$`$where`, "year=2024")
  expect_equal(q$`$limit`, 50L)
  expect_null(q$`$offset`)  # single-shot mode never sends $offset
  expect_equal(nrow(out), 3L)
})

test_that("paginate=TRUE walks $offset and stops on empty page", {
  calls <- list()
  responses <- list(
    list(list(id = 1L), list(id = 2L), list(id = 3L)),  # page 0
    list(list(id = 4L), list(id = 5L)),                  # page 1 (short -> exhausted)
    list())                                              # never reached
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      responses[[length(calls)]]
    },
    .package = "morie")
  out <- morie:::.morie_dataset_socrata_fetch(
    "https://example.test/r.json",
    paginate = TRUE, page_size = 3L)
  # 2 requests -- short 2nd page is the stop signal.
  expect_equal(length(calls), 2L)
  expect_equal(calls[[1L]]$query$`$offset`, 0L)
  expect_equal(calls[[1L]]$query$`$limit`, 3L)
  expect_equal(calls[[2L]]$query$`$offset`, 3L)
  expect_equal(calls[[2L]]$query$`$limit`, 3L)
  expect_equal(nrow(out), 5L)
  expect_equal(out$id, 1:5)
})

test_that("paginate=TRUE stops cleanly when server returns the first empty page", {
  calls <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      list()  # always empty
    },
    .package = "morie")
  out <- morie:::.morie_dataset_socrata_fetch(
    "https://example.test/r.json",
    paginate = TRUE, page_size = 100L)
  expect_equal(length(calls), 1L)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("paginate=TRUE honours max_features as a total cap across pages", {
  calls <- list()
  # Each page returns exactly page_size rows so the walk would
  # otherwise continue forever; max_features = 7 should stop the
  # walk after page 2 (4 + 3 rows) and the final $limit = 3.
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      n <- as.integer(query$`$limit`)
      lapply(seq_len(n), function(i) list(rownum = i))
    },
    .package = "morie")
  out <- morie:::.morie_dataset_socrata_fetch(
    "https://example.test/r.json",
    paginate = TRUE, page_size = 4L, max_features = 7L)
  expect_equal(length(calls), 2L)
  expect_equal(calls[[1L]]$query$`$limit`, 4L)
  expect_equal(calls[[1L]]$query$`$offset`, 0L)
  # Second page is clamped to remaining cap (7 - 4 = 3).
  expect_equal(calls[[2L]]$query$`$limit`, 3L)
  expect_equal(calls[[2L]]$query$`$offset`, 4L)
  expect_equal(nrow(out), 7L)
})

test_that("paginate=TRUE respects max_pages safety net", {
  calls <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      n <- as.integer(query$`$limit`)
      lapply(seq_len(n), function(i) list(rownum = i))  # always full page
    },
    .package = "morie")
  out <- morie:::.morie_dataset_socrata_fetch(
    "https://example.test/r.json",
    paginate = TRUE, page_size = 10L, max_pages = 3L)
  expect_equal(length(calls), 3L)
  expect_equal(nrow(out), 30L)
  expect_equal(calls[[3L]]$query$`$offset`, 20L)
})

test_that("paginate=TRUE threads $where + $$app_token through every page", {
  calls <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      list()  # empty -> single request
    },
    .package = "morie")
  out <- morie:::.morie_dataset_socrata_fetch(
    "https://example.test/r.json",
    where = "primary_type='THEFT'",
    app_token = "fake-token-abc",
    paginate = TRUE, page_size = 100L)
  expect_equal(length(calls), 1L)
  expect_equal(calls[[1L]]$query$`$where`, "primary_type='THEFT'")
  expect_equal(calls[[1L]]$query$`$$app_token`, "fake-token-abc")
  expect_equal(calls[[1L]]$query$`$offset`, 0L)
})

test_that("paginate=TRUE rejects page_size <= 0", {
  expect_error(
    morie:::.morie_dataset_socrata_fetch(
      "https://example.test/r.json",
      paginate = TRUE, page_size = 0L),
    regexp = "page_size >= 1")
})

# =============================================================== (2) wrappers

test_that("morie_datasets_chicago_crime forwards paginate/page_size/max_pages", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size,
                    max_pages = max_pages)
      data.frame(id = 1L)
    },
    .package = "morie")
  out <- morie_datasets_chicago_crime(offline = FALSE,
                                        paginate = TRUE,
                                        page_size = 500L,
                                        max_pages = 42L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 500L)
  expect_equal(seen$max_pages, 42L)
  expect_s3_class(out, "data.frame")
})

test_that("morie_datasets_chicago_crime defaults paginate=FALSE", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate)
      data.frame(id = 1L)
    },
    .package = "morie")
  morie_datasets_chicago_crime(offline = FALSE)
  expect_false(isTRUE(seen$paginate))
})

test_that("morie_datasets_nyc_nypd_arrests_ytd forwards paginate + page_size", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size,
                    max_pages = max_pages, url = url)
      data.frame(stub = 1L)
    },
    .package = "morie")
  morie_datasets_nyc_nypd_arrests_ytd(offline = FALSE,
                                        paginate = TRUE,
                                        page_size = 50000L,
                                        max_pages = 10L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 50000L)
  expect_equal(seen$max_pages, 10L)
  expect_match(seen$url, "uip8-fykc\\.json$")
})

test_that("morie_datasets_nyc_nypd_by_key forwards paginate", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size)
      data.frame(stub = 1L)
    },
    .package = "morie")
  morie_datasets_nyc_nypd_by_key("nypd_uof_incidents",
                                   offline = FALSE,
                                   paginate = TRUE,
                                   page_size = 2000L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 2000L)
})

test_that("morie_datasets_nyc_stop_and_frisk forwards paginate to the helper", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size, url = url)
      data.frame(stub = 1L)
    },
    .package = "morie")
  morie_datasets_nyc_stop_and_frisk(year = 2024L, offline = FALSE,
                                      paginate = TRUE,
                                      page_size = 1000L)
  expect_true(isTRUE(seen$paginate))
  expect_match(seen$url, "7v9w-k82r")
})

test_that("morie_datasets_chicago_neighborhoods forwards paginate", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size)
      data.frame(pri_neigh = "STUB")
    },
    .package = "morie")
  morie_datasets_chicago_neighborhoods(offline = FALSE,
                                         paginate = TRUE,
                                         page_size = 5000L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 5000L)
})

# =============================================================== (3) end-to-end via wrapper

test_that("chicago_crime(paginate=TRUE) walks $offset end-to-end against mocked HTTP", {
  calls <- list()
  responses <- list(
    list(list(id = 1L, case_number = "P0-1"),
         list(id = 2L, case_number = "P0-2")),
    list(list(id = 3L, case_number = "P1-1")))
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      responses[[length(calls)]]
    },
    .package = "morie")
  out <- morie_datasets_chicago_crime(offline = FALSE,
                                        paginate = TRUE,
                                        page_size = 2L)
  expect_equal(length(calls), 2L)
  expect_equal(calls[[1L]]$query$`$offset`, 0L)
  expect_equal(calls[[2L]]$query$`$offset`, 2L)
  expect_equal(nrow(out), 3L)
  expect_equal(out$case_number, c("P0-1", "P0-2", "P1-1"))
})
