# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for the native permutation tests (module 29). No
# reference package needed -- correctness is checked against base R and
# internal consistency; coin parity lives in tests/cross/.

test_that("morie_indep_test returns a finite scalar Z and valid p", {
  set.seed(1)
  d <- data.frame(x = rnorm(60), y = rnorm(60))
  r <- morie_indep_test(y ~ x, d)
  expect_true(is.finite(r$statistic))
  expect_true(r$p.value >= 0 && r$p.value <= 1)
  # one-sided p-values partition around the two-sided
  g <- morie_indep_test(y ~ x, d, alternative = "greater")
  l <- morie_indep_test(y ~ x, d, alternative = "less")
  expect_equal(g$p.value + l$p.value, 1, tolerance = 1e-8)
})

test_that("morie_wilcox_test asymptotic + exact agree in sign and bounds", {
  set.seed(2)
  d <- data.frame(y = c(rnorm(20), rnorm(22, 1)),
                  g = factor(rep(c("a", "b"), c(20, 22))))
  a <- morie_wilcox_test(y ~ g, d, distribution = "asymptotic")
  e <- morie_wilcox_test(y ~ g, d, distribution = "exact")
  expect_true(is.finite(a$statistic))
  expect_true(a$p.value >= 0 && a$p.value <= 1)
  expect_true(e$p.value >= 0 && e$p.value <= 1)
  # exact and asymptotic two-sided p are close for n ~ 40
  expect_equal(a$p.value, e$p.value, tolerance = 0.05)
  expect_error(morie_wilcox_test(y ~ g, rbind(d, data.frame(y = 1, g = "c"))),
               "two-level")
})

test_that("morie_wilcox_test exact matches full enumeration on a tiny sample", {
  y <- c(4, 9, 2, 7, 1, 8)          # 6 obs
  g <- factor(c("a", "a", "a", "b", "b", "b"))
  d <- data.frame(y = y, g = g)
  r <- morie_wilcox_test(y ~ g, d, distribution = "exact")
  # brute force the two-sided permutation p for the rank-sum of group a
  ranks <- rank(y)
  idx <- utils::combn(6, 3)
  sums <- apply(idx, 2, function(i) sum(ranks[i]))
  obs <- sum(ranks[g == "a"])
  mu <- mean(sums)
  p <- mean(abs(sums - mu) >= abs(obs - mu) - 1e-9)
  expect_equal(r$p.value, p, tolerance = 1e-8)
})

test_that("morie_oneway_test handles k=2 (scalar) and k>2 (quadratic)", {
  set.seed(3)
  d2 <- data.frame(y = c(rnorm(20), rnorm(20, 1)),
                   g = factor(rep(c("a", "b"), each = 20)))
  r2 <- morie_oneway_test(y ~ g, d2)
  expect_equal(r2$df, 1L)
  expect_true(r2$p.value >= 0 && r2$p.value <= 1)

  d4 <- data.frame(y = rnorm(80), g = factor(rep(letters[1:4], each = 20)))
  r4 <- morie_oneway_test(y ~ g, d4)
  expect_equal(r4$df, 3L)                      # k-1
  expect_true(r4$statistic >= 0)               # chi-square
  expect_true(r4$p.value >= 0 && r4$p.value <= 1)
})

test_that("Strasser-Weber moments: covariance is symmetric PSD", {
  set.seed(4)
  g <- rmorie:::.morie_f_trafo(factor(rep(c("a", "b", "c"), each = 15)))
  h <- matrix(rnorm(45), ncol = 1)
  m <- rmorie:::.morie_sw_moments(g, h)
  expect_equal(m$Sigma, t(m$Sigma), tolerance = 1e-10)
  expect_true(min(eigen(m$Sigma, symmetric = TRUE, only.values = TRUE)$values) >= -1e-8)
})
