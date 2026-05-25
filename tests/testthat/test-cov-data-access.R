# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 2 -- R/data_access.R: the generic open-data layer
# (morie_fetch, morie_ckan_search, morie_fetch_arcgis). Format readers
# run offline over file:// URLs; the CKAN/ArcGIS HTTP is mocked at the
# internal .morie_read_text() helper so no network is touched.

test_that("morie_fetch reads tsv, xml and html over file://", {
  tsv <- tempfile(fileext = ".tsv")
  writeLines(c("a\tb", "1\tx", "2\ty"), tsv)
  on.exit(unlink(tsv), add = TRUE)
  d <- morie_fetch(paste0("file://", tsv), format = "tsv")
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 2L)

  xml <- tempfile(fileext = ".xml")
  writeLines("<root><item>1</item><item>2</item></root>", xml)
  on.exit(unlink(xml), add = TRUE)
  xr <- morie_fetch(paste0("file://", xml), format = "xml")
  expect_true(is.list(xr) || inherits(xr, "xml_document"))

  html <- tempfile(fileext = ".html")
  writeLines(paste0(
    "<html><body><table><tr><th>k</th></tr>",
    "<tr><td>v</td></tr></table></body></html>"
  ), html)
  on.exit(unlink(html), add = TRUE)
  hr <- morie_fetch(paste0("file://", html), format = "html")
  expect_true(is.data.frame(hr) || is.list(hr) ||
    inherits(hr, "xml_document"))
})

test_that("morie_fetch extracts a zip member over file:// (covr-visible)", {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  csv <- tempfile("z-", fileext = ".csv")
  utils::write.csv(data.frame(a = 1:6), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  zp <- tempfile("z-", fileext = ".zip")
  on.exit(unlink(zp), add = TRUE)
  withr::local_dir(dirname(csv))
  utils::zip(zp, basename(csv), flags = "-q")
  z <- morie_fetch(paste0("file://", zp),
    format = "zip",
    zip_member = basename(csv)
  )
  expect_equal(nrow(z), 6L)
})

test_that(".morie_detect_format uses the Content-Type header when present", {
  f <- morie:::.morie_detect_format
  testthat::local_mocked_bindings(
    curlGetHeaders = function(url, ...) {
      c("HTTP/1.1 200 OK", "content-type: application/json")
    },
    .package = "base"
  )
  expect_equal(f("http://example.org/resource"), "json")
  testthat::local_mocked_bindings(
    curlGetHeaders = function(url, ...) {
      c("HTTP/1.1 200 OK", "Content-Type: text/csv; charset=utf-8")
    },
    .package = "base"
  )
  expect_equal(f("http://example.org/resource"), "csv")
})

test_that(".morie_parse_file rejects an unsupported format", {
  p <- tempfile(fileext = ".dat")
  file.create(p)
  on.exit(unlink(p), add = TRUE)
  expect_error(
    morie:::.morie_parse_file(p, "bogus", TRUE),
    "Unsupported"
  )
})

test_that("morie_ckan_search parses a mocked package_search response", {
  fake <- paste0(
    '{"success":true,"result":{"results":[',
    '{"title":"Cannabis Survey","id":"ds-1","resources":[',
    '{"id":"res-1","name":"PUMF","format":"CSV",',
    '"datastore_active":true,"url":"http://x/p.csv"}]}]}}'
  )
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) fake, .package = "morie"
  )
  hits <- morie_ckan_search("cannabis", portal = "open.canada.ca")
  expect_s3_class(hits, "data.frame")
  expect_equal(nrow(hits), 1L)
  expect_equal(hits$resource_id, "res-1")
  expect_equal(hits$format, "CSV")
  expect_true(hits$datastore_active)
})

test_that("morie_ckan_search returns an empty frame on no results", {
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      '{"success":true,"result":{"results":[]}}'
    },
    .package = "morie"
  )
  hits <- morie_ckan_search("nothing-matches-this", portal = "data.ontario.ca")
  expect_s3_class(hits, "data.frame")
  expect_equal(nrow(hits), 0L)
})

test_that("morie_fetch_arcgis parses a mocked FeatureServer response", {
  fake <- paste0(
    '{"features":[{"attributes":{"OBJECTID":1,"NAME":"a"}},',
    '{"attributes":{"OBJECTID":2,"NAME":"b"}}],',
    '"exceededTransferLimit":false}'
  )
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) fake, .package = "morie"
  )
  df <- morie_fetch_arcgis(
    "https://services.arcgis.com/X/arcgis/rest/services/L/FeatureServer/0"
  )
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_true("OBJECTID" %in% names(df))
})

test_that("morie_fetch_arcgis surfaces an ArcGIS error payload", {
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      '{"error":{"code":400,"message":"Invalid query"}}'
    },
    .package = "morie"
  )
  expect_error(
    morie_fetch_arcgis("https://services.arcgis.com/X/Y/FeatureServer/0"),
    "ArcGIS"
  )
})

test_that("morie_fetch dispatches format='arcgis' to morie_fetch_arcgis", {
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) {
      '{"features":[{"attributes":{"OBJECTID":7}}],"exceededTransferLimit":false}'
    },
    .package = "morie"
  )
  df <- morie_fetch("https://services.arcgis.com/X/Y/FeatureServer/0",
    format = "arcgis"
  )
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
})
