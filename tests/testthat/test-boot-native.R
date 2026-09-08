# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for the native bootstrap (module 28). No reference
# package needed; boot/simpleboot parity lives in tests/cross/.

test_that("morie_boot: t0 is the observed statistic and t has R rows", {
  set.seed(1)
  x <- rnorm(50)
  b <- morie_boot(x, function(d, i) mean(d[i]), R = 200)
  expect_s3_class(b, "morie_boot")
  expect_equal(b$t0, mean(x), tolerance = 1e-12)
  expect_equal(nrow(b$t), 200L)
  expect_equal(dim(b$index), c(200L, 50L))
})

test_that("morie_boot is seed-reproducible", {
  x <- rnorm(40)
  set.seed(7)
  a <- morie_boot(x, function(d, i) median(d[i]), R = 100)
  set.seed(7)
  c <- morie_boot(x, function(d, i) median(d[i]), R = 100)
  expect_identical(a$t, c$t)
})

test_that("morie_boot_ci returns ordered intervals for all four types", {
  set.seed(2)
  x <- rgamma(60, 2, 1)
  b <- morie_boot(x, function(d, i) mean(d[i]), R = 1000)
  ci <- morie_boot_ci(b, type = c("norm", "basic", "perc", "bca"))
  expect_named(ci, c("norm", "basic", "perc", "bca"))
  for (nm in names(ci)) {
    expect_length(ci[[nm]], 2L)
    expect_lte(ci[[nm]][1], ci[[nm]][2])
  }
  # the point estimate lies inside the percentile interval
  expect_gte(mean(x), ci$perc[1])
  expect_lte(mean(x), ci$perc[2])
})

test_that("morie_tsboot runs for moving-block and stationary schemes", {
  set.seed(3)
  y <- as.numeric(arima.sim(list(ar = 0.4), 100))
  for (sm in c("fixed", "geom")) {
    tb <- morie_tsboot(y, function(s) mean(s), R = 100, l = 8, sim = sm)
    expect_s3_class(tb, "morie_boot")
    expect_equal(tb$t0, mean(y), tolerance = 1e-12)
    expect_equal(nrow(tb$t), 100L)
    expect_true(all(is.finite(tb$t[, 1])))
  }
})

test_that("morie_two_boot bootstraps the mean difference", {
  set.seed(4)
  a <- rnorm(30, 5)
  c <- rnorm(25, 4)
  tb <- morie_two_boot(a, c, statistic = mean, R = 400)
  expect_s3_class(tb, "morie_boot")
  expect_equal(tb$t0, mean(a) - mean(c), tolerance = 1e-12)
  expect_equal(nrow(tb$t), 400L)
})

test_that("norm.inter interpolation is monotone in alpha", {
  set.seed(5)
  t <- rnorm(500)
  q <- rmorie:::.morie_norm_inter(t, c(0.025, 0.5, 0.975))
  expect_true(q[1] < q[2] && q[2] < q[3])
})
