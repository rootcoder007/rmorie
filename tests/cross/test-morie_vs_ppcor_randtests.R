# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 26 cross-validation: native stats primitives vs ppcor /
# randtests / EValue.
library(testthat)
library(rmorie)

set.seed(260)
df <- as.data.frame(matrix(rnorm(200 * 5), 200, 5))

test_that("native partial correlation matches ppcor::pcor", {
  skip_if_not_installed("ppcor")
  ref <- ppcor::pcor(df)
  mine <- morie_partial_cor(df)
  expect_equal(unname(mine$estimate), unname(as.matrix(ref$estimate)),
               tolerance = 1e-8)
  expect_equal(unname(mine$p.value), unname(as.matrix(ref$p.value)),
               tolerance = 1e-8)
  expect_equal(mine$gp, ref$gp)
})

test_that("native partial correlation test matches ppcor::pcor.test", {
  skip_if_not_installed("ppcor")
  ref <- ppcor::pcor.test(df$V1, df$V2, df[, 3:5])
  mine <- morie_partial_cor_test(df$V1, df$V2, df[, 3:5])
  expect_equal(mine$estimate, ref$estimate, tolerance = 1e-8)
  expect_equal(mine$p.value, ref$p.value, tolerance = 1e-8)
  expect_equal(mine$statistic, ref$statistic, tolerance = 1e-8)
})

test_that("native semi-partial correlation matches ppcor::spcor", {
  skip_if_not_installed("ppcor")
  ref <- ppcor::spcor(df)
  mine <- morie_semipartial_cor(df)
  expect_equal(unname(mine$estimate), unname(as.matrix(ref$estimate)),
               tolerance = 1e-8)
})

test_that("native runs test matches randtests::runs.test", {
  skip_if_not_installed("randtests")
  set.seed(261); x <- rnorm(80)
  ref <- randtests::runs.test(x)
  mine <- morie_runs_test(x)
  expect_equal(unname(mine$statistic), unname(ref$statistic),
               tolerance = 1e-8)
  expect_equal(mine$p.value, ref$p.value, tolerance = 1e-8)
})

test_that("native turning-point test matches randtests", {
  skip_if_not_installed("randtests")
  set.seed(262); x <- rnorm(100)
  ref <- randtests::turning.point.test(x)
  mine <- morie_turning_point_test(x)
  expect_equal(unname(mine$statistic), unname(ref$statistic),
               tolerance = 1e-8)
  expect_equal(mine$p.value, ref$p.value, tolerance = 1e-8)
})

test_that("native Bartels + difference-sign match randtests", {
  skip_if_not_installed("randtests")
  set.seed(263); x <- rnorm(120)
  rb <- randtests::bartels.rank.test(x)
  mb <- morie_bartels_rank_test(x)
  expect_equal(unname(mb$statistic), unname(rb$statistic),
               tolerance = 1e-6)
  rd <- randtests::difference.sign.test(x)
  md <- morie_difference_sign_test(x)
  expect_equal(unname(md$statistic), unname(rd$statistic),
               tolerance = 1e-6)
})

test_that("native E-value matches EValue::evalue", {
  skip_if_not_installed("EValue")
  for (rr in c(1.5, 2.0, 3.9, 0.4)) {
    ref <- EValue::evalue(EValue::RR(rr))
    mine <- morie_e_value(rr)$morie_e_value
    expect_equal(mine, as.numeric(ref["E-values", "point"]),
                 tolerance = 1e-8)
  }
  ci <- EValue::evalue(EValue::RR(3.9), lo = 2.4)
  expect_equal(morie_e_value(3.9, 2.4)$e_value_ci,
               as.numeric(ci["E-values", "lower"]), tolerance = 1e-8)
})
