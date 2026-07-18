# SPDX-License-Identifier: AGPL-3.0-or-later
# morie_ingest_statcan_vectors(): keyless StatCan WDS vector fetch.
# The native HTTP POST is mocked so the parse + validation logic is
# exercised deterministically, with no live network in CI.

test_that("morie_ingest_statcan_vectors parses the WDS response", {
  sent <- NULL
  fake <- paste0(
    '[',
    '{"status":"SUCCESS","object":{"vectorId":41690973,"vectorDataPoint":[',
    '{"refPer":"2026-04-01","value":168.0,"decimals":1,"scalarFactorCode":0,',
    '"symbolCode":0,"releaseTime":"2026-05-19T08:30"},',
    '{"refPer":"2026-05-01","value":169.6,"decimals":1,"scalarFactorCode":0,',
    '"symbolCode":0,"releaseTime":"2026-06-22T08:30"}]}},',
    '{"status":"SUCCESS","object":{"vectorId":41691045,"vectorDataPoint":[',
    '{"refPer":"2026-04-01","value":182.7,"decimals":1,"scalarFactorCode":0,',
    '"symbolCode":0,"releaseTime":"2026-05-19T08:30"},',
    '{"refPer":"2026-05-01","value":184.5,"decimals":1,"scalarFactorCode":0,',
    '"symbolCode":0,"releaseTime":"2026-06-22T08:30"}]}}',
    ']'
  )
  testthat::local_mocked_bindings(
    .morie_http_post_with_status = function(url, body,
                                            content_type = "application/json",
                                            timeout_s = 60L,
                                            headers = character(),
                                            user_agent = "",
                                            follow_redirects = TRUE) {
      sent <<- list(url = url, body = body)
      list(body = fake, status_code = 200L)
    },
    .package = "rmorie")

  out <- morie_ingest_statcan_vectors(c("v41690973", "41691045"), periods = 2)

  expect_s3_class(out, "data.frame")
  expect_setequal(names(out),
    c("vector", "ref_date", "value", "decimals",
      "scalar_factor", "symbol_code", "release_time"))
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$vector), c(41690973, 41691045))
  expect_equal(out$value[out$vector == 41690973 &
                         out$ref_date == "2026-05-01"], 169.6)

  # request hit the WDS vectors endpoint, and the "v" prefix was stripped
  expect_match(sent$url, "getDataFromVectorsAndLatestNPeriods$")
  body <- jsonlite::fromJSON(sent$body)
  expect_setequal(body$vectorId, c(41690973, 41691045))
  expect_true(all(body$latestN == 2L))
})

test_that("morie_ingest_statcan_vectors validates inputs (no network)", {
  expect_error(morie_ingest_statcan_vectors(character(0)), "non-empty")
  expect_error(morie_ingest_statcan_vectors("abc"), "vector IDs")
  expect_error(morie_ingest_statcan_vectors("v1", periods = 0), "positive")
})

test_that("morie_ingest_statcan_vectors errors on a non-200 status", {
  testthat::local_mocked_bindings(
    .morie_http_post_with_status = function(url, body,
                                            content_type = "application/json",
                                            timeout_s = 60L,
                                            headers = character(),
                                            user_agent = "",
                                            follow_redirects = TRUE) {
      list(body = "", status_code = 503L)
    },
    .package = "rmorie")
  expect_error(morie_ingest_statcan_vectors("v41690973"), "503")
})
