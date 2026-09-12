# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 4: the Kolmogorov-Smirnov goodness-of-fit statistic, its exact
# distribution, one-sided tail, critical values, confidence band and
# sample size, the Cramer-von Mises criterion, and the Lilliefors tests
# for normality and exponentiality.
#
# Anchors outside the module: stats::ks.test for the statistic and its
# exact p-values (one- and two-sided), the Kolmogorov limiting series
# written out by hand, the Birnbaum-Tingey closed form, and for the
# Lilliefors tests the published critical-value tables the module
# carries (checked against their source rows rather than against the
# module's own lookup).

test_that("the KS statistic matches stats::ks.test", {
  set.seed(31)
  for (i in 1:6) {
    x <- stats::runif(25)
    got <- rmorie:::Ksdistfree(x, stats::punif)
    want <- suppressWarnings(stats::ks.test(x, "punif"))
    expect_equal(got$statistic, as.numeric(want$statistic))
    # the one-sided pieces, against ks.test's own. R's "greater" is the
    # alternative that the sample's EDF lies ABOVE the null CDF, whose
    # statistic is D+ = sup(F_n - F); "less" is the mirror, D-.
    expect_equal(got$dplus,
                 as.numeric(suppressWarnings(
                   stats::ks.test(x, "punif",
                                  alternative = "greater")$statistic)))
    expect_equal(got$dminus,
                 as.numeric(suppressWarnings(
                   stats::ks.test(x, "punif",
                                  alternative = "less")$statistic)))
  }
  # D is the larger of the two one-sided deviations
  x <- c(0.1, 0.4, 0.7)
  k <- rmorie:::Ksdistfree(x, stats::punif)
  expect_equal(k$statistic, max(k$dplus, k$dminus))
  # computed by hand: the EDF steps at 1/3, 2/3, 1
  expect_equal(k$dplus, max(c(1, 2, 3) / 3 - c(0.1, 0.4, 0.7)))
  expect_equal(k$dminus, max(c(0.1, 0.4, 0.7) - c(0, 1, 2) / 3))
  expect_equal(k$z, c(0.1, 0.4, 0.7))
  expect_equal(k$n, 3L)
  # a sample exactly on the diagonal of its own CDF is the minimum
  expect_equal(rmorie:::Ksdistfree(c(0.5), stats::punif)$statistic, 0.5)
  # against a different hypothesised distribution
  nx <- stats::qnorm(c(0.25, 0.5, 0.75))
  expect_equal(rmorie:::Ksdistfree(nx, stats::pnorm)$statistic,
               as.numeric(suppressWarnings(
                 stats::ks.test(nx, "pnorm")$statistic)))
  expect_error(rmorie:::Ksdistfree(numeric(0), stats::punif), "non-empty")
})

test_that("the exact distribution of D agrees with ks.test's exact tail", {
  set.seed(32)
  for (n in c(5L, 12L, 25L)) {
    x <- stats::runif(n)
    d <- rmorie:::Ksdistfree(x, stats::punif)$statistic
    got <- rmorie:::Ksexact(d, n)
    want <- suppressWarnings(stats::ks.test(x, "punif", exact = TRUE))
    # sf is P(D_n >= d), which is exactly the two-sided exact p-value
    expect_equal(got$sf, want$p.value, tolerance = 1e-8)
    expect_equal(got$cdf, 1 - want$p.value, tolerance = 1e-8)
  }
  # the degenerate ends
  expect_equal(rmorie:::Ksexact(0, 10L)$cdf, 0)
  expect_equal(rmorie:::Ksexact(-1, 10L)$cdf, 0)
  expect_equal(rmorie:::Ksexact(1, 10L)$cdf, 1)
  expect_equal(rmorie:::Ksexact(2, 10L)$cdf, 1)
  # it is a distribution function: non-decreasing, inside [0, 1]
  ds <- seq(0.05, 0.95, by = 0.05)
  cdfs <- vapply(ds, function(d) rmorie:::Ksexact(d, 15L)$cdf, 0)
  expect_true(all(diff(cdfs) >= -1e-12))
  expect_true(all(cdfs >= 0 & cdfs <= 1))
  expect_equal(cdfs, 1 - vapply(ds, function(d) rmorie:::Ksexact(d, 15L)$sf, 0))
  # for n = 1 the distribution is exact and elementary: D_1 is uniform on
  # (1/2, 1), so P(D_1 <= d) = 2d - 1
  expect_equal(rmorie:::Ksexact(0.75, 1L)$cdf, 0.5, tolerance = 1e-9)
  expect_error(rmorie:::Ksexact(0.5, 0L), "at least 1")
})

test_that("the one-sided tail is the Birnbaum-Tingey formula", {
  for (n in c(5L, 10L, 20L)) {
    for (c in c(0.1, 0.2, 0.35, 0.5)) {
      got <- rmorie:::Ksplusdist(c, n)
      jmax <- floor(n * (1 - c))
      j <- 0:jmax
      want <- c * sum(choose(n, j) * (c + j / n)^(j - 1) *
                        (1 - c - j / n)^(n - j))
      expect_equal(got$sf, min(1, max(0, want)))
      expect_equal(got$cdf, 1 - got$sf)
      expect_equal(got$terms, as.integer(jmax + 1L))
      expect_true(got$sf >= 0 && got$sf <= 1)
    }
  }
  # and against ks.test's exact one-sided p-value
  set.seed(33)
  x <- stats::runif(15)
  dp <- rmorie:::Ksdistfree(x, stats::punif)$dplus
  expect_equal(rmorie:::Ksplusdist(dp, 15L)$sf,
               suppressWarnings(stats::ks.test(x, "punif",
                                               alternative = "greater",
                                               exact = TRUE))$p.value,
               tolerance = 1e-8)
  # the tail decreases as the threshold rises
  sfs <- vapply(seq(0.05, 0.9, by = 0.05),
                function(c) rmorie:::Ksplusdist(c, 20L)$sf, 0)
  expect_true(all(diff(sfs) < 0))
  # the degenerate ends
  expect_equal(rmorie:::Ksplusdist(0, 10L)$sf, 1)
  expect_equal(rmorie:::Ksplusdist(1, 10L)$sf, 0)
  # the one-sided tail is no larger than the two-sided one
  expect_lte(rmorie:::Ksplusdist(0.3, 20L)$sf,
             rmorie:::Ksexact(0.3, 20L)$sf + 1e-12)
  expect_error(rmorie:::Ksplusdist(0.5, 0L), "at least 1")
})

test_that("the Kolmogorov limit is its alternating series", {
  q <- rmorie:::.gbKsQ
  for (k in c(0.4, 0.8, 1.0, 1.36, 2.0)) {
    want <- 2 * sum((-1)^(seq_len(100) - 1) *
                      exp(-2 * seq_len(100)^2 * k^2))
    expect_equal(q(k), want)
  }
  # it is a survival function: falling to zero, inside [0, 1]
  ks <- seq(0.3, 3, by = 0.1)
  vals <- vapply(ks, q, 0)
  expect_true(all(diff(vals) < 0))
  expect_true(all(vals >= 0 & vals <= 1 + 1e-12))
  expect_lt(q(3), 1e-7)
  # the classic 5% point of the Kolmogorov distribution is about 1.3581
  expect_equal(q(1.3581), 0.05, tolerance = 1e-3)
  # and it is the asymptotic p-value ks.test reports for a large sample
  set.seed(34)
  x <- stats::runif(400)
  d <- rmorie:::Ksdistfree(x, stats::punif)$statistic
  expect_equal(q(sqrt(400) * d),
               suppressWarnings(stats::ks.test(x, "punif",
                                               exact = FALSE))$p.value,
               tolerance = 1e-6)
})

test_that("the critical value inverts the exact distribution", {
  for (n in c(8L, 20L)) {
    for (alpha in c(0.10, 0.05, 0.01)) {
      k <- rmorie:::Kscrit(n, alpha)
      # the exact critical value is where the exact tail equals alpha
      expect_equal(rmorie:::Ksexact(k$dcrit, n)$sf, alpha, tolerance = 1e-6)
      # the asymptotic one is k_alpha / sqrt(n)
      expect_equal(k$dcrit_asymp, k$k_alpha / sqrt(n))
      # and k_alpha solves Q(k) = alpha
      expect_equal(rmorie:::.gbKsQ(k$k_alpha), alpha, tolerance = 1e-6)
      expect_true(k$dcrit > 0 && k$dcrit < 1)
    }
  }
  # a smaller alpha needs a larger deviation
  cs <- vapply(c(0.20, 0.10, 0.05, 0.01),
               function(a) rmorie:::Kscrit(15L, a)$dcrit, 0)
  expect_true(all(diff(cs) > 0))
  # and a larger sample needs a smaller one
  ns <- vapply(c(5L, 10L, 20L, 40L),
               function(n) rmorie:::Kscrit(n, 0.05)$dcrit, 0)
  expect_true(all(diff(ns) < 0))
  # the asymptotic value approaches the exact one as n grows
  big <- rmorie:::Kscrit(80L, 0.05)
  expect_equal(big$dcrit, big$dcrit_asymp, tolerance = 0.02)
  # the exact branch can be skipped
  expect_true(is.nan(rmorie:::Kscrit(20L, 0.05, exact = FALSE)$dcrit))
  expect_error(rmorie:::Kscrit(0L), "at least 1")
  expect_error(rmorie:::Kscrit(10L, 0), "strictly inside")
})

test_that("the confidence band is the EDF displaced by the critical value", {
  x <- c(0.1, 0.3, 0.6, 0.9)
  b <- rmorie:::Ksband(x, dcrit = 0.2)
  expect_equal(b$at, sort(x))
  expect_equal(b$edf, c(0.25, 0.5, 0.75, 1))
  expect_equal(b$lower, pmax(b$edf - 0.2, 0))
  expect_equal(b$upper, pmin(b$edf + 0.2, 1))
  expect_equal(b$width, b$upper - b$lower)
  # the band is clipped to the unit interval, since it bounds a CDF
  expect_true(all(b$lower >= 0 & b$upper <= 1))
  expect_equal(b$upper[4], 1)
  # evaluated at arbitrary points rather than the data
  at <- c(0, 0.5, 1)
  a <- rmorie:::Ksband(x, 0.2, at = at)
  expect_equal(a$at, at)
  expect_equal(a$edf, c(0, 0.5, 1))
  # a wider critical value is a wider band
  expect_true(all(rmorie:::Ksband(x, 0.4)$width >= b$width))
  expect_error(rmorie:::Ksband(x, 0), "strictly inside")
  expect_error(rmorie:::Ksband(x, 1), "strictly inside")
  expect_error(rmorie:::Ksband(numeric(0), 0.2), "non-empty")
})

test_that("the sample size attains the requested uniform accuracy", {
  for (c in c(0.2, 0.3)) {
    s <- rmorie:::Ksn(c, alpha = 0.05)
    # the asymptotic figure is (k_alpha / c)^2
    expect_equal(s$n_asymp, as.integer(ceiling((s$k_alpha / c)^2)))
    expect_equal(rmorie:::.gbKsQ(s$k_alpha), 0.05, tolerance = 1e-6)
    # the returned n really does attain the coverage, which is the
    # property the search is for
    expect_gte(s$coverage, 0.95)
    expect_equal(s$coverage, rmorie:::Ksexact(c, s$n)$cdf)
    # and one fewer does not
    expect_lt(rmorie:::Ksexact(c, s$n - 1L)$cdf, 0.95)
  }
  # a tighter accuracy or a smaller alpha needs more observations
  expect_gt(rmorie:::Ksn(0.1)$n, rmorie:::Ksn(0.3)$n)
  expect_gte(rmorie:::Ksn(0.2, alpha = 0.01)$n, rmorie:::Ksn(0.2, 0.05)$n)
  expect_error(rmorie:::Ksn(0), "strictly inside")
  expect_error(rmorie:::Ksn(0.2, alpha = 1), "strictly inside")
})

test_that("the Cramer-von Mises criterion is its closed form", {
  set.seed(35)
  x <- stats::runif(20)
  z <- sort(stats::punif(sort(x)))
  j <- seq_len(20)
  w <- rmorie:::Cvmw2(x, stats::punif)
  expect_equal(w$statistic, 1 / (12 * 20) + sum((z - (2 * j - 1) / 40)^2))
  expect_equal(w$nw2, 20 * w$statistic)
  expect_equal(w$z, z)
  expect_equal(w$n, 20L)
  # it is non-negative, and minimised by a sample sitting on the
  # midpoints of its own distribution
  expect_gt(w$statistic, 0)
  perfect <- (2 * j - 1) / 40
  expect_equal(rmorie:::Cvmw2(perfect, stats::punif)$statistic, 1 / 240)
  # a badly-fitting sample scores higher than a well-fitting one
  expect_gt(rmorie:::Cvmw2(stats::runif(20, 0.8, 1), stats::punif)$statistic,
            w$statistic)

  # the comparison helper reports both criteria on one pass
  cmp <- rmorie:::Kscvmcmp(x, stats::punif)
  expect_equal(cmp$d, rmorie:::Ksdistfree(x, stats::punif)$statistic)
  expect_equal(cmp$w2, w$statistic)
  # the argmax is a 0-based index into the sorted sample
  expect_true(cmp$argmax >= 0L && cmp$argmax < 20L)
  # the reported share is that observation's contribution to W^2
  expect_true(cmp$share >= 0 && cmp$share <= 1)
  expect_error(rmorie:::Cvmw2(numeric(0), stats::punif), "non-empty")
  expect_error(rmorie:::Kscvmcmp(1, stats::punif), "at least 2")
})

test_that("the Lilliefors tests use their published tables", {
  set.seed(36)
  x <- stats::rnorm(20)
  l <- rmorie:::Lillienorm(x, alpha = 0.05)
  # the statistic is the KS distance to the FITTED normal, which is why
  # the null distribution needs its own table
  mu <- mean(x)
  sd <- stats::sd(x)
  z <- stats::pnorm((sort(x) - mu) / sd)
  j <- seq_len(20)
  expect_equal(l$statistic, max(pmax(j / 20 - z, z - (j - 1) / 20)))
  expect_equal(l$mean, mu)
  expect_equal(l$sd, sd)
  # the critical value is the table row for n = 20 at the 5% column
  expect_equal(l$n_table, 20L)
  expect_equal(l$dcrit, 0.192)
  expect_equal(l$reject, as.integer(l$statistic > 0.192))
  # a normal sample is not rejected; a markedly non-normal one is
  expect_equal(rmorie:::Lillienorm(stats::rnorm(30), 0.05)$reject, 0L)
  expect_equal(rmorie:::Lillienorm(stats::rexp(30, 1), 0.05)$reject, 1L)
  # the four levels are the four table columns, and a stricter level has
  # a larger critical value
  cs <- vapply(c(0.100, 0.050, 0.010, 0.001),
               function(a) rmorie:::Lillienorm(x, a)$dcrit, 0)
  expect_equal(cs, c(0.176, 0.192, 0.223, 0.266))
  expect_true(all(diff(cs) > 0))
  # beyond the table the asymptotic tail divisor is used
  big <- rmorie:::Lillienorm(stats::rnorm(200), 0.05)
  expect_equal(big$n_table, 0L)
  expect_equal(big$dcrit, 0.888 / sqrt(200))
  # the table is entered at the largest row not exceeding n
  expect_equal(rmorie:::Lillienorm(stats::rnorm(22), 0.05)$n_table, 20L)
  expect_equal(rmorie:::Lillienorm(stats::rnorm(25), 0.05)$n_table, 25L)

  expect_error(rmorie:::Lillienorm(c(1, 2, 3)), "at least 4")
  expect_error(rmorie:::Lillienorm(rep(2, 10)), "zero variance")
  expect_error(rmorie:::Lillienorm(x, alpha = 0.025),
               "one of 0\\.10, 0\\.05, 0\\.01, 0\\.001")

  # the exponential counterpart, against its own table
  e <- stats::rexp(20, rate = 2)
  le <- rmorie:::Lillieexp(e, alpha = 0.05)
  expect_equal(le$n_table, 20L)
  expect_equal(le$dcrit, 0.234)
  expect_equal(rmorie:::Lillieexp(stats::rexp(40, 3), 0.05)$reject, 0L)
  # a uniform sample is not exponential
  expect_equal(rmorie:::Lillieexp(stats::runif(40, 1, 2), 0.05)$reject, 1L)
})
