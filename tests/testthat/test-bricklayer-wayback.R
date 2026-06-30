# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_wayback_url delegates to rmoriebricklayer", {
  skip_if_not_installed("rmoriebricklayer")
  testthat::local_mocked_bindings(
    wayback_snapshot_url = function(url, timestamp = NULL) {
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
    friendly_download = function(url, target_path, attempt_wayback = NULL) target_path,
    .package = "rmoriebricklayer"
  )
  expect_identical(morie_download("https://data.ontario.ca/x", tp), tp)
})

test_that("the wayback bridges error clearly without rmoriebricklayer", {
  skip_if(requireNamespace("rmoriebricklayer", quietly = TRUE),
          "rmoriebricklayer is installed")
  expect_error(morie_wayback_url("https://x"), "rmoriebricklayer")
  expect_error(morie_download("https://x", tempfile()), "rmoriebricklayer")
})
