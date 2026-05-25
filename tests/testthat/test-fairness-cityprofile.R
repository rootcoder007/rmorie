# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

test_that("MORIE_FAIRNESS_CANONICAL_FIELDS holds five names", {
  expect_equal(MORIE_FAIRNESS_CANONICAL_FIELDS,
               c("area","risk","outcome","population","group"))
})

test_that("morie_fairness_city_profile constructs", {
  p <- morie_fairness_city_profile("chicago", area_col = "ca",
                                   risk_col = "rti",
                                   outcome_col = "shoots",
                                   group_col = "race")
  expect_s3_class(p, "morie_city_profile")
  expect_equal(p$name, "chicago")
  expect_equal(p$area_col, "ca")
  expect_equal(p$risk_col, "rti")
})

test_that("morie_fairness_city_profile errors on empty name", {
  expect_error(morie_fairness_city_profile("", area_col = "a"))
})

test_that("morie_fairness_column_map drops NULL cols", {
  p <- morie_fairness_city_profile("x", area_col = "a", risk_col = "r")
  m <- morie_fairness_column_map(p)
  expect_true("a" %in% names(m))
  expect_true("r" %in% names(m))
  expect_false(any(is.na(m)))
})

test_that("morie_fairness_column_map needs profile class", {
  expect_error(morie_fairness_column_map(list(name = "x")))
})

test_that("morie_fairness_register_city + get_city round-trips", {
  p <- morie_fairness_city_profile(
    paste0("test_city_", as.integer(Sys.time()) %% 100000L),
    area_col = "a"
  )
  morie_fairness_register_city(p)
  got <- morie_fairness_get_city(p$name)
  expect_equal(got$name, p$name)
})

test_that("morie_fairness_register_city rejects duplicate without overwrite", {
  nm <- paste0("dup_", as.integer(Sys.time()) %% 100000L)
  p <- morie_fairness_city_profile(nm, area_col = "a")
  morie_fairness_register_city(p)
  expect_error(morie_fairness_register_city(p), "already registered")
  expect_silent(morie_fairness_register_city(p, overwrite = TRUE))
})

test_that("morie_fairness_get_city errors when missing", {
  expect_error(morie_fairness_get_city("__definitely_missing__"),
               "no city profile")
})

test_that("morie_fairness_list_cities includes generic", {
  v <- morie_fairness_list_cities()
  expect_true("generic" %in% v)
})

test_that("morie_fairness_apply_profile renames columns", {
  df <- data.frame(beat = c("A","B"), score = c(0.1, 0.9))
  p <- morie_fairness_city_profile("demo", area_col = "beat",
                                   risk_col = "score")
  out <- morie_fairness_apply_profile(df, p)
  expect_equal(sort(names(out)), c("area","risk"))
  expect_equal(out$area, c("A","B"))
})

test_that("morie_fairness_apply_profile accepts character profile name", {
  df <- data.frame(area = c("A","B"), risk = c(0.1, 0.2),
                   outcome = c(1,0), population = c(100, 200),
                   group = c("X","Y"))
  out <- morie_fairness_apply_profile(df, "generic")
  expect_true(all(c("area","risk","outcome") %in% names(out)))
})

test_that("morie_fairness_apply_profile errors on missing column", {
  df <- data.frame(area = "A")
  p <- morie_fairness_city_profile("z", area_col = "area",
                                   risk_col = "r_nope")
  expect_error(morie_fairness_apply_profile(df, p), "not in")
})

test_that("print.morie_city_profile runs", {
  p <- morie_fairness_city_profile("z", area_col = "a", notes = "hi")
  expect_output(print(p))
})