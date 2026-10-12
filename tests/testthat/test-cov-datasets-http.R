# R/datasets.R: the HTTP helpers on both backends (the libcurl bindings and httr2), the URL
# builder, the synthetic-frame reader, and the ArcGIS record shaping. No network: every
# transport primitive is replaced by a recorder.

.dh_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.dh <- function(name) get(name, envir = environment(morie_datasets_tps_major_crime))

test_that("the URL builder percent-encodes names and values and drops empty parameters", {
  build <- .dh(".morie_dataset_build_url")
  expect_equal(build("https://h/x"), "https://h/x")
  expect_equal(build("https://h/x", list()), "https://h/x")
  expect_equal(build("https://h/x", list(where = "OCC_YEAR = 2020", f = "json", nothing = NULL)),
               "https://h/x?where=OCC_YEAR%20%3D%202020&f=json")
  expect_equal(build("https://h/x?a=1", list(b = "c d")), "https://h/x?a=1&b=c%20d")
  expect_equal(build("https://h/x", list(a = NULL)), "https://h/x")
  expect_equal(.dh(".morie_dataset_year_where")(NULL), "1=1")
  expect_equal(.dh(".morie_dataset_year_where")(2021), "OCC_YEAR = 2021")
})

test_that("the synthetic frame reader says it is a toy, and gives an empty frame when asked for columns", {
  testthat::local_mocked_bindings(.morie_dataset_pkg_csv = function(name) NA_character_, .package = .dh_pkg)
  rs <- .dh(".morie_dataset_read_synthetic")
  expect_error(rs("nope", "tps_major_crime"), "no bundled synthetic frame at inst/extdata/nope.csv")
  empty <- rs("nope", "tps_major_crime", columns = c("A", "B"))
  expect_equal(names(empty), c("A", "B"))
  expect_equal(nrow(empty), 0L)
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(A = 1:2, B = c("x", "y")), csv, row.names = FALSE)
  testthat::local_mocked_bindings(.morie_dataset_pkg_csv = function(name) csv, .package = .dh_pkg)
  expect_message(got <- rs("toy", "tps_major_crime"), "using the bundled synthetic tps_major_crime frame")
  expect_equal(got$B, c("x", "y"))
})

test_that("a frame rmorie does not bundle is looked for in rmoriedata, then reported missing", {
  testthat::local_mocked_bindings(.rmorie_extdata = function(...) "", .package = .dh_pkg)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_equal(.dh(".morie_dataset_pkg_csv")("nothing_here"), NA_character_)
})

test_that("a request that got no HTTP response names the host and curl's reason", {
  reached <- .dh(".morie_http_reached")
  expect_error(reached("https://data.example.org/a/b", list(status_code = 0L, error = "timed out")),
               "could not reach data.example.org \\(timed out\\)")
  expect_error(reached("https://data.example.org/a/b", list()), "could not reach data.example.org \\(no response\\)")
  expect_identical(reached("https://h/x", list(status_code = 200L)), list(status_code = 200L))
  testthat::local_mocked_bindings(.morie_dataset_http_backend_cpp = function() FALSE, .package = .dh_pkg)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(.dh(".morie_http_require")("the NIBRS feed"), "the NIBRS feed needs an HTTP client")
  testthat::local_mocked_bindings(.morie_dataset_http_backend_cpp = function() TRUE, .package = .dh_pkg)
  expect_true(.dh(".morie_http_require")("x"))
})

test_that("the libcurl-backed helpers pass the built URL, headers and timeout through and read the status", {
  rec <- new.env()
  testthat::local_mocked_bindings(
    .morie_dataset_http_backend_cpp = function() TRUE,
    .morie_http_get_with_status = function(url, timeout_s = 60L, headers = character(), ...) {
      rec$get <- list(url = url, timeout = timeout_s, headers = headers)
      rec$reply
    },
    .morie_http_post_with_status = function(url, body, content_type = "application/json", timeout_s = 60L,
                                            headers = character(), ...) {
      rec$post <- list(url = url, body = body, content_type = content_type, timeout = timeout_s, headers = headers)
      rec$reply
    },
    .morie_http_post = function(url, body, content_type = "application/json", timeout_s = 60L, headers = character(), ...) {
      rec$post <- list(url = url, body = body)
      rec$reply$body
    },
    .morie_http_get_bytes = function(url, timeout_s = 60L, headers = character(), ...) {
      rec$get <- list(url = url, timeout = timeout_s, headers = headers)
      rec$bytes
    },
    .package = .dh_pkg)
  rec$reply <- list(status_code = 200L, body = "hello")
  txt <- .dh(".morie_dataset_http_text")("https://h/x", query = list(a = "1"), headers = "X-Key: k", timeout_s = 9L)
  expect_equal(txt, "hello")
  expect_equal(rec$get, list(url = "https://h/x?a=1", timeout = 9L, headers = "X-Key: k"))
  rec$reply <- list(status_code = 0L, error = "refused")
  expect_error(.dh(".morie_dataset_http_text")("https://h/x"), "could not reach h \\(refused\\)")
  rec$reply <- list(status_code = 404L, body = "missing")
  expect_equal(.dh(".morie_dataset_http_text_with_status")("https://h/x", query = list(q = "z")),
               list(status_code = 404L, body = "missing"))
  expect_equal(rec$get$url, "https://h/x?q=z")
  # POST with a JSON body, with and without the status
  rec$reply <- list(status_code = 201L, body = "{\"ok\":true}")
  r <- .dh(".morie_dataset_http_post_json_with_status")("https://h/p", list(a = 1, b = "two"), headers = "T: 1",
                                                        timeout_s = 3L)
  expect_equal(r$status_code, 201L)
  expect_equal(rec$post$body, "{\"a\":1,\"b\":\"two\"}")
  expect_equal(rec$post$content_type, "application/json")
  expect_equal(rec$post$timeout, 3L)
  expect_equal(rec$post$headers, "T: 1")
  parsed <- .dh(".morie_dataset_http_post_json")("https://h/p", list(a = 1))
  expect_equal(parsed, list(ok = TRUE))
  rec$reply <- list(body = "")
  expect_error(.dh(".morie_dataset_http_post_json")("https://h/p", list(a = 1)), "libcurl returned empty body")
  rec$reply <- list(body = "<html>Bad gateway</html>")
  expect_error(.dh(".morie_dataset_http_post_json")("https://h/p", list(a = 1)),
               "returned non-JSON \\(service error page\\?\\) from https://h/p: <html>Bad gateway</html>")
  # bytes
  rec$bytes <- as.raw(c(0x50, 0x4b, 0x00, 0x01))
  expect_equal(.dh(".morie_dataset_http_bytes")("https://h/z.zip", timeout_s = 4L), rec$bytes)
  expect_equal(rec$get$timeout, 4L)
  rec$bytes <- raw(0)
  expect_error(.dh(".morie_dataset_http_bytes")("https://h/z.zip"), "could not reach h \\(no data came back\\)")
  # JSON: one retry on a dropped connection or a 5xx, then every failure in words
  testthat::local_mocked_bindings(Sys.sleep = function(time) invisible(NULL), .package = "base")
  calls <- 0L
  replies <- list()
  testthat::local_mocked_bindings(
    .morie_http_get_with_status = function(url, timeout_s = 60L, headers = character(), ...) {
      calls <<- calls + 1L
      replies[[min(calls, length(replies))]]
    },
    .package = .dh_pkg)
  json <- .dh(".morie_dataset_http_json")
  replies <- list(list(status_code = 503L, body = "busy"), list(status_code = 200L, body = "{\"features\":[]}"))
  expect_equal(json("https://h/j"), list(features = list()))
  expect_equal(calls, 2L)
  calls <- 0L
  replies <- list(list(status_code = 0L, error = "reset"), list(status_code = 0L, error = "reset"))
  expect_error(json("https://h/j"), "could not reach h \\(reset\\)")
  expect_equal(calls, 2L)
  calls <- 0L
  replies <- list(list(status_code = 404L, body = "{\"error\":{\"message\":\"no layer\"}}"))
  expect_error(json("https://h/j"), "^HTTP 404 from h: no layer$")
  replies <- list(list(status_code = 404L, body = "{\"message\":\"gone\"}"))
  expect_error(json("https://h/j"), "^HTTP 404 from h: gone$")
  replies <- list(list(status_code = 404L, body = "{\"error\":\"plain\"}"))
  expect_error(json("https://h/j"), "^HTTP 404 from h: plain$")
  replies <- list(list(status_code = 400L, body = "not json"))
  expect_error(json("https://h/j"), "^HTTP 400 from h$")
  replies <- list(list(status_code = 200L, body = ""))
  expect_error(json("https://h/j"), "h answered with an empty body")
  replies <- list(list(status_code = 200L, body = "<html>proxy</html>"))
  expect_error(json("https://h/j"), "returned non-JSON body \\(likely an upstream proxy error\\)")
  replies <- list(list(status_code = 200L, body = "{\"errorCode\":\"query.soql.no-such-column\",\"message\":\"No such column\"}"))
  expect_error(json("https://h/j"), "^h: No such column \\(query.soql.no-such-column\\)$")
  replies <- list(list(status_code = 200L, body = "[{\"a\":1},{\"a\":2}]"))
  expect_equal(json("https://h/j")$a, c(1L, 2L))
})

test_that("without the libcurl backend the helpers say so when httr2 is missing too", {
  testthat::local_mocked_bindings(.morie_dataset_http_backend_cpp = function() FALSE, .package = .dh_pkg)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(.dh(".morie_dataset_http_text")("https://h/x"), "needs either the libcurl-backed")
  expect_error(.dh(".morie_dataset_http_json")("https://h/x"), "needs either the libcurl-backed")
})

test_that("without the libcurl backend the helpers go through httr2", {
  skip_if_not_installed("httr2")
  testthat::local_mocked_bindings(.morie_dataset_http_backend_cpp = function() FALSE, .package = .dh_pkg)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) package == "httr2", .package = "base")
  seen <- new.env()
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      seen$req <- req
      structure(list(method = "GET", url = req$url, status_code = 200L, body = charToRaw("{\"n\":3}"),
                     headers = structure(list(`content-type` = "application/json"), class = "httr2_headers"),
                     cache = new.env()),
                class = "httr2_response")
    },
    .package = "httr2")
  expect_equal(.dh(".morie_dataset_http_text")("https://h/x", query = list(a = "b"), headers = c("X-A: 1", "junk")),
               "{\"n\":3}")
  expect_equal(seen$req$url, "https://h/x?a=b")
  expect_equal(seen$req$headers[["X-A"]], "1", ignore_attr = TRUE)
  expect_equal(.dh(".morie_dataset_http_json")("https://h/x"), list(n = 3L))
})

test_that("ArcGIS features and record lists become one clean frame", {
  fl <- .dh(".morie_arcgis_feature_list")
  expect_equal(fl(NULL), list())
  expect_equal(fl(list()), list())
  recs <- list(list(attributes = list(a = 1)), list(attributes = list(a = 2)))
  expect_identical(fl(recs), recs)
  df <- data.frame(attributes = I(data.frame(a = 1:2, b = c("x", "y"))), geometry = I(data.frame(x = c(1.5, 2.5), y = c(3, 4))))
  df$attributes <- data.frame(a = 1:2, b = c("x", "y"))
  df$geometry <- data.frame(x = c(1.5, 2.5), y = c(3, 4))
  got <- fl(df)
  expect_length(got, 2)
  expect_equal(got[[2]]$attributes, list(a = 2L, b = "y"))
  expect_equal(got[[1]]$geometry, list(x = 1.5, y = 3))
  flat <- data.frame(a = 1:2)
  expect_equal(fl(flat)[[1]]$attributes, list(a = 1L))
  expect_null(fl(flat)[[1]]$geometry)
  r2d <- .dh(".morie_dataset_records_to_df")
  expect_equal(r2d(NULL), data.frame())
  expect_equal(r2d(list()), data.frame())
  expect_equal(r2d(flat), flat)
  out <- r2d(list(list(id = 1, tags = c("a", "b"), res = list(list(k = 1)), gone = NULL),
                  list(id = 2, extra = "e")))
  expect_equal(names(out), c("id", "tags", "res", "gone", "extra"))
  expect_equal(out$tags, c("a; b", NA))
  expect_equal(out$res, c("[{\"k\":1}]", NA))
  expect_true(all(is.na(out$gone)))
  expect_equal(out$extra, c(NA, "e"))
  # the TPS fetch: attributes to rows, geometry to coordinates
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL, ...) {
      list(features = list(list(attributes = list(OCC_YEAR = 2020, HOOD = "1"), geometry = list(x = 1, y = 2)),
                           list(attributes = list(OCC_YEAR = 2021, HOOD = "2"), geometry = list())))
    },
    .package = .dh_pkg)
  tf <- .dh(".morie_dataset_tps_fetch")
  plain <- tf("https://h/layer", where = "OCC_YEAR = 2020", max_features = 5L)
  expect_equal(plain$HOOD, c("1", "2"))
  expect_null(plain$geom_x)
  geo <- tf("https://h/layer", return_geometry = TRUE)
  expect_equal(geo$geom_x, c(1, NA))
  expect_equal(geo$geom_y, c(2, NA))
  testthat::local_mocked_bindings(.morie_dataset_http_json = function(url, query = NULL, ...) list(features = list()),
                                  .package = .dh_pkg)
  expect_equal(tf("https://h/layer"), data.frame())
})
