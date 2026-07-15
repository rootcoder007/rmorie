# SPDX-License-Identifier: AGPL-3.0-or-later
# Contract tests for the central mock registry (helper-mocks.R): if a mock
# drifts from the real package's return shape, these fail first, pointing
# at the mock rather than at production code.

test_that("matchit mock matches the real matchit object shape", {
  f <- get_mock("matchit")()
  d <- data.frame(d = rep(c(1L, 0L), each = 10),
                  x1 = rnorm(20), x2 = rnorm(20))
  m <- f(d ~ x1 + x2, data = d)
  expect_s3_class(m, "matchit")
  expect_true(all(c("match.matrix", "weights", "treat", "distance") %in%
                    names(m)))
  expect_length(m$weights, nrow(d))
  expect_true(is.matrix(m$match.matrix))
})

test_that("matchit mock agrees with real MatchIt shape when installed", {
  skip_if_not_installed("MatchIt")
  set.seed(1)
  d <- data.frame(d = rep(c(1L, 0L), each = 15),
                  x1 = rnorm(30), x2 = rnorm(30))
  real <- MatchIt::matchit(d ~ x1 + x2, data = d, method = "nearest")
  mock <- get_mock("matchit")()(d ~ x1 + x2, data = d)
  for (field in c("match.matrix", "weights", "treat", "distance")) {
    expect_true(field %in% names(real), info = paste("real has", field))
    expect_true(field %in% names(mock), info = paste("mock has", field))
  }
  expect_identical(class(mock), class(real))
})

test_that("svyglm mock exposes the coef/vcov surface the pipeline reads", {
  f <- get_mock("svyglm")()
  fit <- f(y ~ x1, design = NULL)
  expect_s3_class(fit, "svyglm")
  expect_named(stats::coef(fit), c("(Intercept)", "x1"))
  expect_equal(dim(fit$vcov), c(2L, 2L))
})

test_that("canned CKAN body parses through the real parser", {
  skip_if_not_installed("jsonlite")
  local_canned_read_text("ckan_package_search")
  rows <- morie_ckan_search("anything", portal = "open.canada.ca")
  expect_s3_class(rows, "data.frame")
  expect_equal(nrow(rows), 2L)
  expect_true(all(c("dataset_title", "resource_id", "format", "url") %in%
                    names(rows)))
  expect_identical(rows$format, c("CSV", "TXT"))
  expect_identical(rows$dataset_title[1], "Test Package")
})

test_that("canned ArcGIS body parses through the real pagination loop", {
  skip_if_not_installed("jsonlite")
  local_canned_read_text("arcgis_features")
  df <- morie_fetch_arcgis("https://example.com/FeatureServer/0")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_true(all(c("id", "name") %in% names(df)))
  expect_identical(df$name, c("Alpha", "Beta"))
})
