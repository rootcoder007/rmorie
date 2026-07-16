# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native permutation tests (module 29) vs coin.
# The Strasser-Weber conditional moments give the asymptotic normal /
# chi-square reference in closed form, matching coin's statistic() and
# pvalue() to machine precision; the exact two-sample distribution is
# checked against coin's exact Wilcoxon.

test_that("native independence test matches coin::independence_test (asymptotic)", {
  skip_if_not_installed("coin")
  set.seed(1)
  df <- data.frame(x = rnorm(80), y = rnorm(80))
  m <- morie_indep_test(y ~ x, df, distribution = "asymptotic")
  ci <- coin::independence_test(y ~ x, data = df, distribution = "asymptotic")
  expect_equal(m$statistic, as.numeric(coin::statistic(ci)), tolerance = 1e-8)
  expect_equal(m$p.value, as.numeric(coin::pvalue(ci)), tolerance = 1e-8)
})

test_that("native Wilcoxon matches coin::wilcox_test (asymptotic)", {
  skip_if_not_installed("coin")
  set.seed(2)
  df <- data.frame(y = c(rnorm(30), rnorm(35, 0.6)),
                   g = factor(rep(c("a", "b"), c(30, 35))))
  for (alt in c("two.sided", "greater", "less")) {
    m <- morie_wilcox_test(y ~ g, df, alternative = alt,
                           distribution = "asymptotic")
    ci <- coin::wilcox_test(y ~ g, data = df, alternative = alt,
                            distribution = "asymptotic")
    expect_equal(m$statistic, as.numeric(coin::statistic(ci)),
                 tolerance = 1e-8, info = alt)
    expect_equal(m$p.value, as.numeric(coin::pvalue(ci)),
                 tolerance = 1e-8, info = alt)
  }
})

test_that("native Wilcoxon matches coin::wilcox_test (exact, with ties)", {
  skip_if_not_installed("coin")
  set.seed(3)
  df <- data.frame(y = c(sample(1:10, 14, TRUE), sample(3:12, 12, TRUE)),
                   g = factor(rep(c("a", "b"), c(14, 12))))
  m <- morie_wilcox_test(y ~ g, df, distribution = "exact")
  ci <- coin::wilcox_test(y ~ g, data = df, distribution = "exact")
  expect_equal(m$p.value, as.numeric(coin::pvalue(ci)), tolerance = 1e-8)
})

test_that("native one-way (k>2) matches coin::oneway_test quadratic (asymptotic)", {
  skip_if_not_installed("coin")
  set.seed(4)
  df <- data.frame(y = rnorm(120),
                   g = factor(rep(letters[1:4], each = 30)))
  m <- morie_oneway_test(y ~ g, df, distribution = "asymptotic")
  ci <- coin::oneway_test(y ~ g, data = df, distribution = "asymptotic",
                          teststat = "quadratic")
  expect_equal(m$statistic, as.numeric(coin::statistic(ci)), tolerance = 1e-8)
  expect_equal(m$df, as.integer(round(ci@statistic@df)), tolerance = 1e-8)
  expect_equal(m$p.value, as.numeric(coin::pvalue(ci)), tolerance = 1e-8)
})

test_that("native one-way (k=2) matches coin::oneway_test scalar", {
  skip_if_not_installed("coin")
  set.seed(5)
  df <- data.frame(y = c(rnorm(25), rnorm(25, 0.4)),
                   g = factor(rep(c("a", "b"), each = 25)))
  m <- morie_oneway_test(y ~ g, df)
  ci <- coin::oneway_test(y ~ g, data = df, distribution = "asymptotic",
                          teststat = "scalar")
  expect_equal(abs(m$statistic), abs(as.numeric(coin::statistic(ci))),
               tolerance = 1e-8)
  expect_equal(m$p.value, as.numeric(coin::pvalue(ci)), tolerance = 1e-8)
})
