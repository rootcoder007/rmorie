# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 22 -- surgical pass on R/data_access.R: the exact
# branches left uncovered by waves 2 and 17 (content-type detection,
# missing-package stops, html multi-table, zip member matching, the
# ArcGIS pagination loop, CKAN-search edge cases).

test_that(".morie_url_with_params returns url when every param is NULL", {
  expect_equal(
    rmorie:::.morie_url_with_params("http://x", list(a = NULL, b = NULL)),
    "http://x"
  )
})

test_that(".morie_detect_format covers every content-type + extension", {
  f <- rmorie:::.morie_detect_format
  cts <- list(
    c("application/zip", "zip"),
    c("text/tab-separated-values", "tsv"),
    c("application/vnd.ms-excel", "xlsx"),
    c("text/xml", "xml"), c("text/html", "html")
  )
  for (ct in cts) {
    testthat::local_mocked_bindings(
      curlGetHeaders = function(url, ...) {
        c(
          "HTTP/1.1 200 OK",
          paste0("content-type: ", ct[1])
        )
      },
      .package = "base"
    )
    expect_equal(f("http://example.org/resource"), ct[2])
  }
  for (e in c("zip", "json", "tsv", "txt", "xls", "xml", "htm")) {
    expect_true(nzchar(f(paste0("file:///tmp/x.", e))))
  }
})

test_that(".morie_parse_file errors when a reader package is absent", {
  p <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1), p, row.names = FALSE)
  on.exit(unlink(p), add = TRUE)
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package %in% c("readxl", "jsonlite", "xml2")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(rmorie:::.morie_parse_file(p, "xlsx", TRUE), "readxl")
  # Module 23: json/xml/html now fall back to the NATIVE parsers when
  # the accelerator packages are absent — they parse, not stop.
  pj <- tempfile(fileext = ".json")
  writeLines('{"a": 1}', pj)
  on.exit(unlink(pj), add = TRUE)
  expect_silent(out_j <- rmorie:::.morie_parse_file(pj, "json", TRUE))
  px <- tempfile(fileext = ".xml")
  writeLines("<r><a>1</a></r>", px)
  on.exit(unlink(px), add = TRUE)
  out_x <- rmorie:::.morie_parse_file(px, "xml", TRUE)
  expect_true(is.list(out_x))
  ph <- tempfile(fileext = ".html")
  writeLines("<html><body><p>hi</p></body></html>", ph)
  on.exit(unlink(ph), add = TRUE)
  out_h <- rmorie:::.morie_parse_file(ph, "html", TRUE)
  expect_true(is.list(out_h))
})

test_that(".morie_parse_file html returns multiple tables / the document", {
  skip_if_not_installed("rvest")
  h <- tempfile(fileext = ".html")
  writeLines(paste0(
    "<html><body>",
    "<table><tr><th>a</th></tr><tr><td>1</td></tr></table>",
    "<table><tr><th>b</th></tr><tr><td>2</td></tr></table>",
    "</body></html>"
  ), h)
  on.exit(unlink(h), add = TRUE)
  if (requireNamespace("rvest", quietly = TRUE)) {
    multi <- rmorie:::.morie_parse_file(h, "html", TRUE)
    expect_true(is.list(multi))
  }
  doc <- rmorie:::.morie_parse_file(h, "html", FALSE)
  expect_true(inherits(doc, "xml_document") || is.list(doc))
})

test_that("morie_fetch zip member matches by substring and errors if absent", {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  csv <- tempfile("zm-", fileext = ".csv")
  utils::write.csv(data.frame(v = 1:3), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  zp <- tempfile("zm-", fileext = ".zip")
  on.exit(unlink(zp), add = TRUE)
  withr::local_dir(dirname(csv))
  utils::zip(zp, basename(csv), flags = "-q")
  base <- basename(csv)
  partial <- substr(base, 1, nchar(base) - 4) # drop ".csv"
  z <- morie_fetch(paste0("file://", zp),
    format = "zip",
    zip_member = partial
  )
  expect_equal(nrow(z), 3L)
  expect_error(morie_fetch(paste0("file://", zp),
    format = "zip",
    zip_member = "no-such-member"
  ), "not found")
})

test_that("morie_ckan_search jsonlite-missing + failed + empty-resources", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "jsonlite")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(morie_ckan_search("x"), "jsonlite")
})

test_that("morie_ckan_search handles failure and empty-resource datasets", {
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) '{"success":false}',
    .package = "rmorie"
  )
  expect_error(morie_ckan_search("x"), "failed")
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      '{"success":true,"result":{"results":[{"title":"D","id":"d","resources":[]}]}}'
    },
    .package = "rmorie"
  )
  # every dataset has empty resources -> all skipped -> rbind(list()) is NULL
  expect_null(morie_ckan_search("x"))
})

test_that(".nz returns empty string when every argument is blank", {
  expect_equal(rmorie:::.nz(), "")
  expect_equal(rmorie:::.nz(NULL, NA, ""), "")
})

test_that("morie_fetch_arcgis: jsonlite stop, empty, and multi-page", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "jsonlite")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(morie_fetch_arcgis("http://x/FeatureServer/0"), "jsonlite")
})

test_that("morie_fetch_arcgis paginates and handles empty layers", {
  # max_records = 0 -> the loop breaks immediately, empty frame
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      '{"features":[{"attributes":{"A":1}}],"exceededTransferLimit":false}'
    },
    .package = "rmorie"
  )
  e0 <- morie_fetch_arcgis("http://x/FeatureServer/0", max_records = 0)
  expect_equal(nrow(e0), 0L)
  # an empty features array -> break, empty frame
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      '{"features":[],"exceededTransferLimit":false}'
    },
    .package = "rmorie"
  )
  expect_equal(nrow(morie_fetch_arcgis("http://x/FeatureServer/0")), 0L)
  # two pages: first exceeds the transfer limit, second does not
  calls <- 0L
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      calls <<- calls + 1L
      if (calls == 1L) {
        '{"features":[{"attributes":{"A":1}}],"exceededTransferLimit":true}'
      } else {
        '{"features":[{"attributes":{"A":2}}],"exceededTransferLimit":false}'
      }
    }, .package = "rmorie"
  )
  two <- morie_fetch_arcgis("http://x/FeatureServer/0", page_size = 1)
  expect_equal(nrow(two), 2L)
})
