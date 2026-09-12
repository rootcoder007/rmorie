# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Sec. 6.6 and 8.2: the Mann-Whitney U statistic, the Wilcoxon rank sum,
# their shared exact null distribution, the normal approximation with
# the tie correction, the shift confidence interval and the sample-size
# formula.
#
# Anchors outside the module: base R's exact Wilcoxon rank-sum
# distribution (dwilcox, pwilcox), stats::wilcox.test for the statistic
# and its shift interval, and the identity W = U + m(m+1)/2 that ties
# the two parameterisations together.

test_that("the exact null distribution of U is stats::dwilcox", {
  for (m in 2:7) {
    for (n in 2:7) {
      total <- m * n
      counts <- rmorie:::.gbRankCounts(m, n)
      expect_length(counts, total + 1)
      pmf <- counts / choose(m + n, m)
      # the subset count against base R's own exact distribution
      expect_equal(pmf, stats::dwilcox(0:total, m, n))
      expect_equal(sum(pmf), 1)
      expect_equal(cumsum(pmf), stats::pwilcox(0:total, m, n))
      # the counts are the number of rank orderings, so they sum to the
      # number of ways to choose which observations are the x sample
      expect_equal(sum(counts), choose(m + n, m))
      # symmetric about mn/2
      expect_equal(pmf, rev(pmf))
    }
  }
  # the extremes are the single separated orderings
  expect_equal(rmorie:::.gbRankCounts(3, 4)[1], 1)
  expect_equal(utils::tail(rmorie:::.gbRankCounts(3, 4), 1), 1)
})

test_that("U matches stats::wilcox.test and its own moments", {
  set.seed(21)
  for (i in 1:6) {
    x <- stats::rnorm(9, mean = i / 3)
    y <- stats::rnorm(11)
    got <- rmorie:::Mwu(x, y)
    want <- suppressWarnings(stats::wilcox.test(x, y))
    expect_equal(got$statistic, as.numeric(want$statistic))
    # the exact two-sided p-value, doubled smaller tail
    expect_equal(got$p_value, want$p.value, tolerance = 1e-8)
  }

  x <- c(1, 3, 5, 7)
  y <- c(2, 4, 6)
  u <- rmorie:::Mwu(x, y)
  # U counts the pairs where x beats y, halving exact ties
  expect_equal(u$statistic, sum(outer(x, y, ">")))
  expect_equal(u$mean, 4 * 3 / 2)
  expect_equal(u$var, 4 * 3 * 8 / 12)
  expect_equal(u$z, (u$statistic - u$mean) / sqrt(u$var))
  expect_equal(u$p_normal, 2 * (1 - stats::pnorm(abs(u$z))))
  expect_equal(u$m, 4L)
  expect_equal(u$n, 3L)

  # completely separated samples give the extreme statistic
  expect_equal(rmorie:::Mwu(c(10, 11), c(1, 2))$statistic, 4)
  expect_equal(rmorie:::Mwu(c(1, 2), c(10, 11))$statistic, 0)
  # ties count a half each, which is what keeps U symmetric
  expect_equal(rmorie:::Mwu(c(1, 2), c(2, 3))$statistic, 0.5)
  expect_equal(rmorie:::Mwu(c(1, 1), c(1, 1))$statistic, 2)
  # and U for (x, y) mirrors U for (y, x) about mn
  expect_equal(rmorie:::Mwu(x, y)$statistic +
                 rmorie:::Mwu(y, x)$statistic, 12)
  expect_error(rmorie:::Mwu(numeric(0), 1), "non-empty")
})

test_that("the rank sum is U shifted by m(m+1)/2", {
  set.seed(22)
  x <- stats::rnorm(8)
  y <- stats::rnorm(10)
  w <- rmorie:::Wrs(x, y)
  # the statistic is the sum of the x sample's ranks in the pooled data
  expect_equal(w$statistic, sum(rank(c(x, y))[1:8]))
  # THE IDENTITY: W = U + m(m+1)/2, so the two are the same test
  expect_equal(w$statistic, rmorie:::Mwu(x, y)$statistic + 8 * 9 / 2)
  # and the two exact p-values therefore agree
  expect_equal(w$p_value, rmorie:::Mwu(x, y)$p_value)

  # the attainable range
  expect_equal(w$wmin, 8 * 9 / 2)
  expect_equal(w$wmax, 8 * (2 * 18 - 8 + 1) / 2)
  expect_true(w$statistic >= w$wmin && w$statistic <= w$wmax)
  # the null moments
  expect_equal(w$mean, 8 * 19 / 2)
  expect_equal(w$var, 8 * 10 * 19 / 12)
  expect_equal(w$z, (w$statistic - w$mean) / sqrt(w$var))
  # the mean sits midway between the extremes
  expect_equal(w$mean, (w$wmin + w$wmax) / 2)

  # a fully separated sample attains an endpoint
  expect_equal(rmorie:::Wrs(c(1, 2), c(10, 20))$statistic, 3)
  expect_equal(rmorie:::Wrs(c(10, 20), c(1, 2))$statistic, 7)
  # tied observations get midranks
  expect_equal(rmorie:::Wrs(c(1, 1), c(1, 1))$statistic, 5)
  expect_error(rmorie:::Wrs(1, numeric(0)), "non-empty")
})

test_that("the rank-sum normal approximation carries the tie correction", {
  m <- 10L
  n <- 12L
  nn <- 22
  mean <- m * (nn + 1) / 2
  v0 <- m * n * (nn + 1) / 12
  for (w in c(80, mean, 150)) {
    z <- rmorie:::Wrsz(w, m, n)
    expect_equal(z$mean, mean)
    expect_equal(z$var, v0)
    expect_equal(z$var_uncorrected, v0)
    expect_equal(z$z, (w - mean) / sqrt(v0))
    expect_equal(z$p_value, min(1, 2 * (1 - stats::pnorm(abs(z$z)))))
    expect_equal(rmorie:::Wrsz(w, m, n, "greater")$p_value,
                 1 - stats::pnorm(z$z))
    expect_equal(rmorie:::Wrsz(w, m, n, "less")$p_value,
                 stats::pnorm(z$z))
  }
  # at the null mean there is nothing to detect
  expect_equal(rmorie:::Wrsz(mean, m, n)$z, 0)
  expect_equal(rmorie:::Wrsz(mean, m, n)$p_value, 1)
  # the continuity correction moves the statistic toward the null
  expect_lte(abs(rmorie:::Wrsz(150, m, n, correct = TRUE)$z),
             abs(rmorie:::Wrsz(150, m, n)$z))

  # ties reduce the variance by the documented amount
  tv <- c(3, 2)
  tied <- rmorie:::Wrsz(150, m, n, ties = tv)
  expect_equal(tied$var,
               m * n / 12 * ((nn + 1) - sum(tv * (tv^2 - 1)) /
                               (nn * (nn - 1))))
  expect_lt(tied$var, v0)
  expect_equal(tied$var_uncorrected, v0)
  # a tie group of size 1 is not a tie, so it changes nothing
  expect_equal(rmorie:::Wrsz(150, m, n, ties = c(1, 1))$var, v0)
  # and the approximation tracks the exact tail
  expect_equal(rmorie:::Wrsz(m * (m + 1) / 2 + 95, m, n, "greater")$p_value,
               1 - stats::pwilcox(94, m, n), tolerance = 0.15)
  expect_error(rmorie:::Wrsz(1, 0L, 1L), "at least 1")
  expect_error(rmorie:::Wrsz(1, 2L, 2L, "sideways"),
               "two-sided, greater or less")
})

test_that("the shift interval is built from the pairwise differences", {
  set.seed(23)
  x <- stats::rnorm(7, mean = 1)
  y <- stats::rnorm(9)
  d <- sort(as.numeric(outer(x, y, "-")))

  ci <- rmorie:::Mwuci(x, y, k = 10L)
  expect_equal(ci$ndiff, 63L)
  expect_equal(ci$lower, d[11L])
  expect_equal(ci$upper, d[63L - 10L])
  # the point estimate is the median of the differences, which is the
  # Hodges-Lehmann shift estimate wilcox.test reports
  expect_equal(ci$estimate, stats::median(d))
  expect_equal(ci$estimate,
               as.numeric(suppressWarnings(
                 stats::wilcox.test(x, y, conf.int = TRUE)$estimate)),
               tolerance = 1e-8)
  # the interval brackets its own estimate
  expect_true(ci$lower <= ci$estimate && ci$estimate <= ci$upper)
  # k = 0 is the full range of the differences
  full <- rmorie:::Mwuci(x, y, k = 0L)
  expect_equal(full$lower, min(d))
  expect_equal(full$upper, max(d))
  # a larger k is a narrower interval
  expect_gte(rmorie:::Mwuci(x, y, k = 20L)$lower, ci$lower)
  expect_lte(rmorie:::Mwuci(x, y, k = 20L)$upper, ci$upper)

  # the rank-sum parameterisation indexes the same differences, offset
  # by the minimum attainable rank sum
  wci <- rmorie:::Wrsci(x, y, wcrit = 7 * 8 / 2 + 10)
  expect_equal(wci$k, 10L)
  expect_equal(wci$lower, ci$lower)
  expect_equal(wci$upper, ci$upper)
  expect_equal(wci$estimate, ci$estimate)

  expect_error(rmorie:::Mwuci(x, y, k = 63L), "0\\.\\.mn-1")
  expect_error(rmorie:::Mwuci(x, y, k = -1L), "0\\.\\.mn-1")
  expect_error(rmorie:::Mwuci(numeric(0), y, 0L), "non-empty")
  expect_error(rmorie:::Wrsci(x, y, wcrit = 0), "outside 0\\.\\.mn-1")
})

test_that("the sample-size formula splits the total by the allocation", {
  s <- rmorie:::Mwun(0.7, c = 0.5, alpha = 0.05, beta = 0.10)
  raw <- (stats::qnorm(0.95) + stats::qnorm(0.90))^2 /
    (12 * 0.5 * 0.5 * (0.7 - 0.5)^2)
  expect_equal(s$n_raw, raw)
  expect_equal(s$n, as.integer(ceiling(raw)))
  # the total is split in the requested proportion
  expect_equal(s$m + s$n_y, s$n)
  expect_equal(s$m, as.integer(round(0.5 * s$n)))
  # an unbalanced allocation needs a larger total, since c(1-c) is
  # maximised at one half
  expect_gt(rmorie:::Mwun(0.7, c = 0.2)$n, s$n)
  expect_equal(rmorie:::Mwun(0.7, c = 0.25)$m,
               as.integer(round(0.25 * rmorie:::Mwun(0.7, c = 0.25)$n)))
  # a smaller effect, or a smaller beta, needs more
  ns <- vapply(c(0.9, 0.8, 0.7, 0.6, 0.55), function(p) rmorie:::Mwun(p)$n, 0L)
  expect_true(all(diff(ns) > 0))
  expect_gt(rmorie:::Mwun(0.7, beta = 0.01)$n, s$n)
  expect_gte(rmorie:::Mwun(0.7, twosided = TRUE)$n, s$n)
  # symmetric about one half
  expect_equal(rmorie:::Mwun(0.7)$n, rmorie:::Mwun(0.3)$n)
  expect_equal(s$z_alpha, stats::qnorm(0.95))
  expect_equal(s$z_beta, stats::qnorm(0.90))

  expect_error(rmorie:::Mwun(0.5), "must differ from 0\\.5")
  expect_error(rmorie:::Mwun(0.7, c = 0), "strictly inside")
  expect_error(rmorie:::Mwun(0.7, c = 1), "strictly inside")
})
