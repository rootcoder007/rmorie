# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3RR: Socrata OData v4 integration. Helper +
# chicago_crime_odata wrapper. @odata.nextLink server-driven
# pagination, OData metadata-col stripping, X-App-Token header.
#
# Phase 3WW: rewritten to mock .morie_dataset_http_json (the
# libcurl-or-httr2 transport boundary) instead of httr2 internals
# directly.

# ===================================================== (1) OData helper core

test_that(".morie_dataset_odata_fetch(paginate=FALSE) hits /api/odata/v4/<id> with $top + $select", {
  capture <- new.env()
  capture$urls <- character()
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL,
                                          headers = character(),
                                          timeout_s = 60L) {
      capture$urls <- c(capture$urls,
                         morie:::.morie_dataset_build_url(url, query))
      list(value = list(list(id = "1", primary_type = "THEFT"),
                          list(id = "2", primary_type = "BATTERY")),
           `@odata.context` = "https://x/api/odata/v4/$metadata")
    },
    .package = "morie")
  out <- morie:::.morie_dataset_odata_fetch(
    "ijzp-q8t2",
    select = "id,primary_type",
    top = 2L)
  expect_equal(length(capture$urls), 1L)
  u <- URLdecode(capture$urls[1L])
  expect_match(u, "data\\.cityofchicago\\.org/api/odata/v4/ijzp-q8t2")
  expect_match(u, "\\$select=id,primary_type")
  expect_match(u, "\\$top=2")
})

test_that(".morie_dataset_odata_fetch strips @odata.* metadata cols from rows", {
  testthat::local_mocked_bindings(
    .morie_dataset_records_to_df = function(records) {
      data.frame(
        `@odata.id` = "https://x/api/odata/v4/ijzp-q8t2('row-aaaa.bbbb')",
        id = 1L, primary_type = "THEFT",
        check.names = FALSE, stringsAsFactors = FALSE)
    },
    .morie_dataset_http_json = function(url, query = NULL,
                                          headers = character(),
                                          timeout_s = 60L) {
      list(value = list(list(id = 1L)))
    },
    .package = "morie")
  out <- morie:::.morie_dataset_odata_fetch("ijzp-q8t2")
  expect_false("@odata.id" %in% names(out))
  expect_true("id" %in% names(out))
  expect_true("primary_type" %in% names(out))
})

test_that(".morie_dataset_odata_fetch(paginate=TRUE) follows @odata.nextLink until absent", {
  capture <- new.env()
  capture$urls <- character()
  call_count <- 0L
  responses <- list(
    # page 0: 3 rows + nextLink
    list(value = list(list(id = "1"), list(id = "2"), list(id = "3")),
         `@odata.nextLink` = "https://x/api/odata/v4/ijzp-q8t2?%24skip=3"),
    # page 1: 2 rows + nextLink
    list(value = list(list(id = "4"), list(id = "5")),
         `@odata.nextLink` = "https://x/api/odata/v4/ijzp-q8t2?%24skip=5"),
    # page 2: 0 rows -> stop
    list(value = list()))
  testthat::local_mocked_bindings(
    .morie_dataset_records_to_df = function(records) {
      if (length(records) == 0L) return(data.frame())
      as.data.frame(do.call(rbind, lapply(records, as.data.frame,
                                            stringsAsFactors = FALSE)))
    },
    .morie_dataset_http_json = function(url, query = NULL,
                                          headers = character(),
                                          timeout_s = 60L) {
      capture$urls <- c(capture$urls,
                         morie:::.morie_dataset_build_url(url, query))
      call_count <<- call_count + 1L
      responses[[call_count]]
    },
    .package = "morie")
  out <- morie:::.morie_dataset_odata_fetch("ijzp-q8t2",
                                              paginate = TRUE)
  # 3 requests: initial + 2 nextLinks. The 3rd returns empty -> stop.
  expect_equal(length(capture$urls), 3L)
  expect_match(capture$urls[2L], "%24skip=3")
  expect_match(capture$urls[3L], "%24skip=5")
  expect_equal(nrow(out), 5L)
})

test_that(".morie_dataset_odata_fetch(paginate=TRUE) stops when @odata.nextLink is absent", {
  capture <- new.env()
  capture$urls <- character()
  testthat::local_mocked_bindings(
    .morie_dataset_records_to_df = function(records) {
      as.data.frame(do.call(rbind, lapply(records, as.data.frame,
                                            stringsAsFactors = FALSE)))
    },
    .morie_dataset_http_json = function(url, query = NULL,
                                          headers = character(),
                                          timeout_s = 60L) {
      capture$urls <- c(capture$urls,
                         morie:::.morie_dataset_build_url(url, query))
      # Single page, no nextLink -> stop after 1 request.
      list(value = list(list(id = "1"), list(id = "2")))
    },
    .package = "morie")
  out <- morie:::.morie_dataset_odata_fetch("ijzp-q8t2",
                                              paginate = TRUE)
  expect_equal(length(capture$urls), 1L)
  expect_equal(nrow(out), 2L)
})

test_that(".morie_dataset_odata_fetch(paginate=TRUE) honours max_features by truncating the last page", {
  testthat::local_mocked_bindings(
    .morie_dataset_records_to_df = function(records) {
      as.data.frame(do.call(rbind, lapply(records, as.data.frame,
                                            stringsAsFactors = FALSE)))
    },
    .morie_dataset_http_json = local({
      count <- 0L
      responses <- list(
        list(value = list(list(id = "1"), list(id = "2"), list(id = "3")),
             `@odata.nextLink` = "https://x/p2"),
        list(value = list(list(id = "4"), list(id = "5"), list(id = "6")),
             `@odata.nextLink` = "https://x/p3"))
      function(url, query = NULL, headers = character(),
                timeout_s = 60L) {
        count <<- count + 1L
        responses[[count]]
      }
    }),
    .package = "morie")
  out <- morie:::.morie_dataset_odata_fetch("ijzp-q8t2",
                                              paginate = TRUE,
                                              max_features = 4L)
  expect_equal(nrow(out), 4L)
})

test_that(".morie_dataset_odata_fetch sends app_token as X-App-Token header", {
  capture <- new.env()
  capture$headers <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_records_to_df = function(records) data.frame(),
    .morie_dataset_http_json = function(url, query = NULL,
                                          headers = character(),
                                          timeout_s = 60L) {
      capture$headers[[length(capture$headers) + 1L]] <- headers
      list(value = list())
    },
    .package = "morie")
  morie:::.morie_dataset_odata_fetch("ijzp-q8t2",
                                       app_token = "tok-odata-xyz")
  expect_equal(capture$headers[[1L]], "X-App-Token: tok-odata-xyz")
})

test_that(".morie_dataset_odata_fetch passes $filter verbatim (caller-supplied)", {
  capture <- new.env()
  capture$urls <- character()
  testthat::local_mocked_bindings(
    .morie_dataset_records_to_df = function(records) data.frame(),
    .morie_dataset_http_json = function(url, query = NULL,
                                          headers = character(),
                                          timeout_s = 60L) {
      capture$urls <- c(capture$urls,
                         morie:::.morie_dataset_build_url(url, query))
      list(value = list())
    },
    .package = "morie")
  morie:::.morie_dataset_odata_fetch(
    "ijzp-q8t2",
    filter = "year eq 2024",
    orderby = "year desc")
  u <- URLdecode(capture$urls[1L])
  expect_match(u, "\\$filter=year eq 2024")
  expect_match(u, "\\$orderby=year desc")
})

# ============================== (2) chicago_crime_odata wrapper

test_that("morie_datasets_chicago_crime_odata(offline=TRUE) returns the 22-col chicago_crime synthetic frame", {
  df <- suppressWarnings(morie_datasets_chicago_crime_odata(offline = TRUE))
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 22L)
  expect_true("location" %in% names(df))
})

test_that("morie_datasets_chicago_crime_odata(offline=TRUE) honours max_features", {
  df <- suppressWarnings(
    morie_datasets_chicago_crime_odata(offline = TRUE,
                                          max_features = 2L))
  expect_equal(nrow(df), 2L)
})

test_that("morie_datasets_chicago_crime_odata(offline=FALSE) routes to .morie_dataset_odata_fetch for ijzp-q8t2 by default", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_odata_fetch = function(view_id, filter = NULL,
                                            select = NULL,
                                            orderby = NULL,
                                            top = NULL, skip = NULL,
                                            app_token = NULL,
                                            paginate = FALSE,
                                            max_pages = 200L,
                                            max_features = NULL,
                                            base_url = "https://data.cityofchicago.org") {
      seen <<- list(view_id = view_id, filter = filter,
                    select = select, orderby = orderby,
                    top = top, paginate = paginate,
                    max_features = max_features)
      data.frame(id = 1L)
    },
    .package = "morie")
  morie_datasets_chicago_crime_odata(
    select = "id,primary_type,year",
    orderby = "year desc",
    top = 100L,
    offline = FALSE)
  expect_equal(seen$view_id, "ijzp-q8t2")
  expect_equal(seen$select, "id,primary_type,year")
  expect_equal(seen$orderby, "year desc")
  expect_equal(seen$top, 100L)
  expect_false(isTRUE(seen$paginate))
})

test_that("morie_datasets_chicago_crime_odata(resource_id='crimes') routes via publisher alias", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_odata_fetch = function(view_id, filter = NULL,
                                            select = NULL,
                                            orderby = NULL,
                                            top = NULL, skip = NULL,
                                            app_token = NULL,
                                            paginate = FALSE,
                                            max_pages = 200L,
                                            max_features = NULL,
                                            base_url = "https://data.cityofchicago.org") {
      seen <<- list(view_id = view_id)
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_crime_odata(offline = FALSE,
                                       resource_id = "crimes")
  expect_equal(seen$view_id, "crimes")
})

test_that("morie_datasets_chicago_crime_odata forwards paginate + max_pages + app_token", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_odata_fetch = function(view_id, filter = NULL,
                                            select = NULL,
                                            orderby = NULL,
                                            top = NULL, skip = NULL,
                                            app_token = NULL,
                                            paginate = FALSE,
                                            max_pages = 200L,
                                            max_features = NULL,
                                            base_url = "https://data.cityofchicago.org") {
      seen <<- list(paginate = paginate, max_pages = max_pages,
                    app_token = app_token, max_features = max_features)
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_crime_odata(offline = FALSE,
                                       paginate = TRUE,
                                       max_pages = 50L,
                                       max_features = 5000L,
                                       app_token = "odata-tok")
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$max_pages, 50L)
  expect_equal(seen$app_token, "odata-tok")
  expect_equal(seen$max_features, 5000L)
})

test_that("morie_datasets_chicago_crime_odata defaults to offline = TRUE", {
  expect_s3_class(suppressWarnings(morie_datasets_chicago_crime_odata()),
                  "data.frame")
})
