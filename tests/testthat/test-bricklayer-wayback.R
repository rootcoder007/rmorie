# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_wayback_url delegates to rmoriebricklayer", {
  skip_if_not_installed("rmoriebricklayer")
  testthat::local_mocked_bindings(
    wbsnpurl = function(url, ts = NULL) {
      "https://web.archive.org/web/20260623060856/https://data.ontario.ca/dataset/data-on-inmates-in-ontario"
    },
    .package = "rmoriebricklayer"
  )
  expect_identical(
    morie_wayback_url("https://data.ontario.ca/dataset/data-on-inmates-in-ontario"),
    "https://web.archive.org/web/20260623060856/https://data.ontario.ca/dataset/data-on-inmates-in-ontario"
  )
})

test_that("morie_download delegates to rmoriebricklayer friendly_download", {
  skip_if_not_installed("rmoriebricklayer")
  tp <- tempfile(fileext = ".html")
  testthat::local_mocked_bindings(
    fdl = function(url, tpath, attwb = NULL, allowfile = FALSE) TRUE,
    .package = "rmoriebricklayer"
  )
  expect_identical(morie_download("https://data.ontario.ca/x", tp), tp)
})

test_that("morie_download errors when rmoriebricklayer::fdl() fails, and TRUE asks for a snapshot", {
  skip_if_not_installed("rmoriebricklayer")
  seen <- list()
  testthat::local_mocked_bindings(
    fdl = function(url, tpath, attwb = NULL, allowfile = FALSE) {
      seen[[length(seen) + 1L]] <<- list(attwb = attwb)
      FALSE
    },
    .package = "rmoriebricklayer"
  )
  testthat::local_mocked_bindings(.morie_dl = function(...) stop("offline"), .package = "rmorie")
  expect_error(morie_download("https://data.ontario.ca/x", tempfile()), "could not download")
  expect_error(morie_download("https://data.ontario.ca/x", tempfile(), attempt_wayback = TRUE),
               "could not download")
  expect_error(morie_download("https://data.ontario.ca/x", tempfile(), attempt_wayback = FALSE),
               "could not download")
  expect_null(seen[[1L]]$attwb)
  expect_null(seen[[2L]]$attwb)
  expect_false(seen[[3L]]$attwb)
  # the package transfer succeeding never reaches fdl()
  testthat::local_mocked_bindings(.morie_dl = function(...) invisible(NULL), .package = "rmorie")
  tp <- tempfile()
  expect_identical(morie_download("https://data.ontario.ca/x", tp), tp)
  expect_length(seen, 3L)
})

test_that("the wayback bridges error clearly without rmoriebricklayer", {
  skip_if(requireNamespace("rmoriebricklayer", quietly = TRUE),
          "rmoriebricklayer is installed")
  expect_error(morie_wayback_url("https://x"), "rmoriebricklayer")
  expect_error(morie_download("https://x", tempfile()), "rmoriebricklayer")
})
