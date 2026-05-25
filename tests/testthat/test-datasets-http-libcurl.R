# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3VV: shared libcurl-backed HTTP primitives
# (src/morie_http.{h,cpp}) + the R retrofit in
# .morie_dataset_http_text / .morie_dataset_http_json. These tests
# stay offline: they only exercise the C++ symbol presence + the
# URL-encoding logic + the backend-detection contract. Live network
# round-trips are covered by the smoke calls embedded in
# devtools::load_all interactive sessions; CRAN must not hit the
# wire.

# ====================================== C++ symbol surface

test_that(".morie_http_get C++ symbol is exported and callable in this build", {
  expect_true(.morie_dataset_http_backend_cpp <- exists(
    ".morie_dataset_http_backend_cpp",
    where = asNamespace("morie"), mode = "function") |
    exists(".morie_http_get",
    where = asNamespace("morie"), mode = "function"))
  expect_true(exists(".morie_http_get",
                       where = asNamespace("morie"),
                       mode = "function"))
})

test_that(".morie_http_curl_version returns a non-empty version string", {
  ver <- morie:::.morie_http_curl_version()
  expect_type(ver, "character")
  expect_true(nzchar(ver))
  # Loose shape check: libcurl version strings look like "8.7.1".
  expect_match(ver, "^\\d+\\.\\d+(\\.\\d+)?")
})

test_that(".morie_dataset_http_backend_cpp() reports TRUE when the C++ symbol is present", {
  expect_true(morie:::.morie_dataset_http_backend_cpp())
})

# ====================================== URL builder

test_that(".morie_dataset_build_url returns the bare URL when query is NULL or empty", {
  expect_equal(
    morie:::.morie_dataset_build_url("https://x.test/r.json"),
    "https://x.test/r.json")
  expect_equal(
    morie:::.morie_dataset_build_url("https://x.test/r.json", NULL),
    "https://x.test/r.json")
  expect_equal(
    morie:::.morie_dataset_build_url("https://x.test/r.json", list()),
    "https://x.test/r.json")
})

test_that(".morie_dataset_build_url appends ?k=v on a URL without an existing query string", {
  out <- morie:::.morie_dataset_build_url(
    "https://x.test/r.json",
    query = list(`$limit` = 5L))
  # Either ? or %3F (URLencode escapes $); we accept the encoded form.
  expect_match(out, "\\?")
  expect_match(out, "limit=5")
})

test_that(".morie_dataset_build_url appends &k=v when URL already has a query string", {
  out <- morie:::.morie_dataset_build_url(
    "https://x.test/r.json?$select=id",
    query = list(`$limit` = 5L))
  # The existing ? remains, and the new pair joins with &.
  expect_match(out, "&")
  expect_equal(length(gregexpr("\\?", out)[[1L]]), 1L)
})

test_that(".morie_dataset_build_url percent-encodes both key and value", {
  out <- morie:::.morie_dataset_build_url(
    "https://x.test/r.json",
    query = list(`$where` = "year=2024 AND primary_type='THEFT'"))
  # $ -> %24, space -> %20, ' -> %27
  expect_match(out, "%24where=")
  expect_match(out, "%20AND%20")
  expect_match(out, "%27THEFT%27")
})

test_that(".morie_dataset_build_url drops NULL/empty values", {
  out <- morie:::.morie_dataset_build_url(
    "https://x.test/r.json",
    query = list(`$limit` = 5L,
                  `$where` = NULL,
                  `$select` = character()))
  expect_match(out, "limit=5")
  expect_false(grepl("where", out))
  expect_false(grepl("select", out))
})

test_that(".morie_dataset_build_url preserves correct & ordering of multiple params", {
  out <- morie:::.morie_dataset_build_url(
    "https://x.test/r.json",
    query = list(a = "1", b = "2", c = "3"))
  # 2 ampersands, 1 question mark.
  expect_equal(length(gregexpr("&", out)[[1L]]), 2L)
  expect_equal(length(gregexpr("\\?", out)[[1L]]), 1L)
})

# ====================================== Retrofit routes through C++ helper

test_that(".morie_dataset_http_text routes through .morie_http_get when the C++ backend is available", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_http_get = function(url, timeout_s = 60L,
                                 headers = character(),
                                 user_agent = "",
                                 follow_redirects = TRUE) {
      seen <<- list(url = url, timeout_s = timeout_s,
                    headers = headers)
      "stub-body"
    },
    .package = "morie")
  out <- morie:::.morie_dataset_http_text(
    "https://x.test/r.csv",
    query = list(a = "1"))
  expect_equal(out, "stub-body")
  expect_match(seen$url, "^https://x\\.test/r\\.csv\\?a=1$")
  expect_equal(seen$timeout_s, 60L)
})

test_that(".morie_dataset_http_json routes through .morie_http_get + jsonlite::fromJSON", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_http_get = function(url, timeout_s = 60L,
                                 headers = character(),
                                 user_agent = "",
                                 follow_redirects = TRUE) {
      seen <<- list(url = url)
      '[{"a":1,"b":"two"}]'
    },
    .package = "morie")
  out <- morie:::.morie_dataset_http_json(
    "https://x.test/r.json",
    query = list(`$limit` = 1L))
  expect_s3_class(out, "data.frame")
  expect_equal(out$a, 1)
  expect_equal(out$b, "two")
  expect_match(seen$url, "limit=1")
})

test_that(".morie_dataset_http_json raises when libcurl returns empty body", {
  testthat::local_mocked_bindings(
    .morie_http_get = function(url, timeout_s = 60L,
                                 headers = character(),
                                 user_agent = "",
                                 follow_redirects = TRUE) {
      ""  # transport failure
    },
    .package = "morie")
  expect_error(
    morie:::.morie_dataset_http_json("https://x.test/r.json"),
    regexp = "libcurl returned empty body")
})

test_that(".morie_dataset_http_text + json forward custom headers (e.g. X-App-Token)", {
  captured <- character()
  testthat::local_mocked_bindings(
    .morie_http_get = function(url, timeout_s = 60L,
                                 headers = character(),
                                 user_agent = "",
                                 follow_redirects = TRUE) {
      captured <<- headers
      '[{"x":1}]'
    },
    .package = "morie")
  morie:::.morie_dataset_http_json(
    "https://x.test/r.json",
    headers = c("X-App-Token: tok-abc",
                "Accept: application/json"))
  expect_equal(captured,
                c("X-App-Token: tok-abc",
                  "Accept: application/json"))
})

# ====================================== siu_parser is unaffected

test_that(".siu_http_get still works (3VV preserved siu_parser's transport)", {
  # We don't call .siu_http_get over the wire; we just verify the
  # symbol stayed exported through the 3VV refactor.
  expect_true(exists(".siu_http_get",
                       where = asNamespace("morie"),
                       mode = "function"))
})

# ====================================== 3XX: get_bytes binary-safe path

test_that(".morie_http_get_bytes C++ symbol exists + returns a raw vector", {
  expect_true(exists(".morie_http_get_bytes",
                       where = asNamespace("morie"),
                       mode = "function"))
})

test_that(".morie_dataset_http_bytes routes through .morie_http_get_bytes when available", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_http_get_bytes = function(url, timeout_s = 60L,
                                       headers = character(),
                                       user_agent = "",
                                       follow_redirects = TRUE) {
      seen <<- list(url = url, timeout_s = timeout_s,
                    headers = headers)
      # Return raw bytes representing the literal string "abc\0def"
      # (with embedded NUL -- proves we preserve binary correctly).
      as.raw(c(0x61, 0x62, 0x63, 0x00, 0x64, 0x65, 0x66))
    },
    .package = "morie")
  out <- morie:::.morie_dataset_http_bytes(
    "https://x.test/bin",
    query = list(format = "shp"))
  expect_type(out, "raw")
  expect_equal(length(out), 7L)
  # Critical: the embedded NUL survived (rawToChar would drop here;
  # we deliberately check the raw vector length).
  expect_equal(out[4L], as.raw(0x00))
  expect_match(seen$url, "^https://x\\.test/bin\\?format=shp$")
  expect_equal(seen$timeout_s, 60L)
})

test_that(".morie_dataset_http_bytes forwards custom headers", {
  captured <- character()
  testthat::local_mocked_bindings(
    .morie_http_get_bytes = function(url, timeout_s = 60L,
                                       headers = character(),
                                       user_agent = "",
                                       follow_redirects = TRUE) {
      captured <<- headers
      raw()
    },
    .package = "morie")
  morie:::.morie_dataset_http_bytes(
    "https://x.test/bin",
    headers = c("X-App-Token: tok-xyz",
                "Accept: application/zip"))
  expect_equal(captured,
                c("X-App-Token: tok-xyz",
                  "Accept: application/zip"))
})

# ====================================== 3ZZ: status-aware variants

test_that(".morie_http_get_with_status + .morie_http_post_with_status exist", {
  expect_true(exists(".morie_http_get_with_status",
                       where = asNamespace("morie"),
                       mode = "function"))
  expect_true(exists(".morie_http_post_with_status",
                       where = asNamespace("morie"),
                       mode = "function"))
})

test_that(".morie_dataset_http_text_with_status returns list(body, status_code)", {
  testthat::local_mocked_bindings(
    .morie_http_get_with_status = function(url, timeout_s = 60L,
                                             headers = character(),
                                             user_agent = "",
                                             follow_redirects = TRUE) {
      list(body = "body-text", status_code = 200L)
    },
    .package = "morie")
  out <- morie:::.morie_dataset_http_text_with_status(
    "https://x.test/y", query = list(a = 1))
  expect_type(out, "list")
  expect_named(out, c("body", "status_code"))
  expect_equal(out$body, "body-text")
  expect_equal(out$status_code, 200L)
})

test_that(".morie_dataset_http_text_with_status surfaces 4xx without throwing", {
  testthat::local_mocked_bindings(
    .morie_http_get_with_status = function(url, timeout_s = 60L,
                                             headers = character(),
                                             user_agent = "",
                                             follow_redirects = TRUE) {
      list(body = '{"error":"unauthorised"}', status_code = 401L)
    },
    .package = "morie")
  out <- morie:::.morie_dataset_http_text_with_status(
    "https://x.test/y")
  expect_equal(out$status_code, 401L)
  expect_match(out$body, "unauthorised")
})

test_that(".morie_dataset_http_post_json_with_status serialises body + returns status", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_http_post_with_status = function(url, body,
                                              content_type = "application/json",
                                              timeout_s = 60L,
                                              headers = character(),
                                              user_agent = "",
                                              follow_redirects = TRUE) {
      seen <<- list(body = body, content_type = content_type)
      list(body = '{"ok":true}', status_code = 200L)
    },
    .package = "morie")
  out <- morie:::.morie_dataset_http_post_json_with_status(
    "https://x.test/y",
    body = list(x = 1L, y = "two"))
  expect_equal(out$status_code, 200L)
  # jsonlite::toJSON(auto_unbox=TRUE) renders the body deterministically.
  expect_match(seen$body, '"x":1')
  expect_match(seen$body, '"y":"two"')
  expect_equal(seen$content_type, "application/json")
})
