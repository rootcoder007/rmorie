# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Sec. 5.5-5.7: signed ranks, the Wilcoxon signed-rank statistic, its
# exact null distribution, its normal approximation with the tie
# correction, power and sample size, and the Walsh-average confidence
# interval with the Hodges-Lehmann estimator.
#
# Anchors outside the module: base R's exact signed-rank distribution
# (dsignrank, psignrank), stats::wilcox.test for the statistic and its
# Hodges-Lehmann interval, stats::rank for the midranks, and the
# Walsh-average counting identity -- T+ equals the number of Walsh
# averages exceeding the hypothesised centre -- which is a theorem about
# the statistic rather than a restatement of the code.

test_that("signed ranks are the midranks of the absolute differences", {
  d <- c(3, -1, 4, -1, 5, 0, -9)
  a <- rmorie:::Absrank(d)
  expect_equal(a$ranks, rank(abs(d), ties.method = "average"))
  expect_equal(a$signs, sign(d))
  expect_equal(a$signed, sign(d) * rank(abs(d), ties.method = "average"))
  expect_equal(a$n, 7L)
  # the tied absolute values are reported, here the two 1s
  expect_equal(a$ties, 2)
  # a zero difference has sign zero, so it contributes nothing signed
  expect_equal(a$signed[6], 0)
  # with no ties there is nothing to report
  expect_length(rmorie:::Absrank(c(1, -2, 3))$ties, 0L)
  expect_equal(rmorie:::Absrank(c(1, -2, 3))$ranks, c(1, 2, 3))
  # the signed ranks sum to T+ minus T-
  expect_equal(sum(a$signed), sum(a$ranks[d > 0]) - sum(a$ranks[d < 0]))
  expect_error(rmorie:::Absrank(numeric(0)), "non-empty")
})

test_that("the exact null distribution of T+ is stats::dsignrank", {
  for (n in 1:12) {
    total <- n * (n + 1) / 2
    pmf <- rmorie:::.gbWsrNull(n)
    expect_length(pmf, total + 1)
    # the subset-sum count against base R's own exact distribution
    expect_equal(pmf, stats::dsignrank(0:total, n))
    expect_equal(sum(pmf), 1)
    # and the cumulative version
    expect_equal(cumsum(pmf), stats::psignrank(0:total, n))
  }
  # symmetric about n(n+1)/4, which is what makes the two tails equal
  pmf <- rmorie:::.gbWsrNull(8)
  expect_equal(pmf, rev(pmf))
  # the extremes are the single subsets: all ranks or none
  expect_equal(pmf[1], 0.5^8)
  expect_equal(pmf[length(pmf)], 0.5^8)
})

test_that("T+ matches stats::wilcox.test", {
  set.seed(11)
  for (i in 1:6) {
    x <- stats::rnorm(15, mean = i / 4)
    got <- rmorie:::Wsr(x)
    want <- suppressWarnings(stats::wilcox.test(x))
    expect_equal(got$statistic, as.numeric(want$statistic))
  }
  # the hypothesised centre is subtracted first. Chosen so no difference
  # is exactly zero, because the two disagree on zeros by design: the
  # module discards them BEFORE ranking, which is the convention Gibbons
  # sets out, whereas stats::wilcox.test on this version ranks with the
  # zero still in place.
  x <- c(1, 4, 9, 16, 25)
  expect_false(any(x - 10 == 0))
  expect_equal(rmorie:::Wsr(x, m0 = 10)$statistic,
               as.numeric(suppressWarnings(
                 stats::wilcox.test(x, mu = 10)$statistic)))

  # the zero convention, asserted directly: a zero difference leaves the
  # sample entirely, so the ranks are formed from what remains
  zx <- rmorie:::Wsr(x, m0 = 9)
  d <- x - 9
  nz <- d[d != 0]
  expect_equal(zx$nzero, 1L)
  expect_equal(zx$n, 4L)
  expect_equal(zx$statistic, sum(rank(abs(nz))[nz > 0]))

  # T+ and T- partition the total rank sum
  w <- rmorie:::Wsr(x)
  expect_equal(w$statistic + w$tminus, w$n * (w$n + 1) / 2)
  # the null moments
  expect_equal(w$mean, 5 * 6 / 4)
  expect_equal(w$var, 5 * 6 * 11 / 24)
  # zeros are discarded from the sample size, as the convention requires
  z <- rmorie:::Wsr(c(1, 2, 0, 0, -3))
  expect_equal(z$nzero, 2L)
  expect_equal(z$n, 3L)
  # an all-positive sample gives the maximum statistic
  expect_equal(rmorie:::Wsr(c(1, 2, 3))$statistic, 6)
  expect_equal(rmorie:::Wsr(c(-1, -2, -3))$statistic, 0)

  # ties reduce the variance by the documented correction
  tied <- c(1, 1, 2, -2, 3)
  wt <- rmorie:::Wsr(tied)
  tb <- as.numeric(table(abs(tied)))
  tb <- tb[tb > 1]
  expect_equal(wt$var, 5 * 6 * 11 / 24 - sum(tb * (tb^2 - 1)) / 48)
  expect_lt(wt$var, 5 * 6 * 11 / 24)
  expect_error(rmorie:::Wsr(c(0, 0)), "no non-zero differences")
})

test_that("the null moments and the tie correction are their formulas", {
  for (n in c(1L, 5L, 20L, 50L)) {
    m <- rmorie:::Wsrmom(n)
    expect_equal(m$mean, n * (n + 1) / 4)
    expect_equal(m$var, n * (n + 1) * (2 * n + 1) / 24)
    expect_equal(m$sd, sqrt(m$var))
    expect_equal(m$total, n * (n + 1) / 2)
    # the null distribution is symmetric, so its skewness is zero
    expect_equal(m$skew, 0)
    # the mean is half the total, which symmetry requires
    expect_equal(m$mean, m$total / 2)
  }
  # cross-checked against the exact distribution for a small n
  n <- 9L
  total <- n * (n + 1) / 2
  pmf <- rmorie:::.gbWsrNull(n)
  expect_equal(rmorie:::Wsrmom(n)$mean, sum((0:total) * pmf))
  expect_equal(rmorie:::Wsrmom(n)$var,
               sum((0:total)^2 * pmf) - sum((0:total) * pmf)^2)

  # the tie correction
  d <- c(1, 1, 1, 2, -2, 3, 4)
  t <- rmorie:::Wsrties(d)
  tb <- as.numeric(table(abs(d)))
  ties <- tb[tb > 1]
  expect_equal(t$var_uncorrected, 7 * 8 * 15 / 24)
  expect_equal(t$correction, sum(ties * (ties^2 - 1)) / 48)
  expect_equal(t$var, t$var_uncorrected - t$correction)
  expect_equal(t$ties, ties)
  # no ties means no correction
  expect_equal(rmorie:::Wsrties(c(1, 2, 3))$correction, 0)
  expect_equal(rmorie:::Wsrties(c(1, 2, 3))$var, 3 * 4 * 7 / 24)
  # zeros are excluded before the ranks are formed
  expect_equal(rmorie:::Wsrties(c(1, 2, 0, 3))$n, 3L)
  expect_equal(rmorie:::Wsrties(c(1, 2, 0, 3))$nzero, 1L)
  expect_error(rmorie:::Wsrmom(0), "at least 1")
  expect_error(rmorie:::Wsrties(0), "no non-zero differences")
})

test_that("the signed-rank normal approximation is the standardised T+", {
  n <- 20L
  mean <- n * (n + 1) / 4
  sd <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  for (t in c(40, 105, 150)) {
    z <- rmorie:::Wsrz(t, n)
    expect_equal(z$z, (t - mean) / sd)
    expect_equal(z$p_value, min(1, 2 * (1 - stats::pnorm(abs(z$z)))))
    expect_equal(rmorie:::Wsrz(t, n, "greater")$p_value,
                 1 - stats::pnorm(z$z))
    expect_equal(rmorie:::Wsrz(t, n, "less")$p_value, stats::pnorm(z$z))
  }
  # at the null mean there is no evidence either way
  expect_equal(rmorie:::Wsrz(mean, n)$z, 0)
  expect_equal(rmorie:::Wsrz(mean, n)$p_value, 1)
  # the continuity correction always moves the statistic toward the null
  for (t in c(40, 150)) {
    expect_lte(abs(rmorie:::Wsrz(t, n, correct = TRUE)$z),
               abs(rmorie:::Wsrz(t, n, correct = FALSE)$z))
  }
  expect_equal(rmorie:::Wsrz(mean, n, correct = TRUE)$z, 0)
  # and the approximation tracks the exact tail for a moderate n
  expect_equal(rmorie:::Wsrz(150, 20L, "greater")$p_value,
               1 - stats::psignrank(149, 20), tolerance = 0.1)
  expect_error(rmorie:::Wsrz(1, 0L), "at least 1")
  expect_error(rmorie:::Wsrz(1, 5L, "sideways"),
               "two-sided, greater or less")
})

test_that("signed-rank power and sample size invert one another", {
  p <- rmorie:::Wsrpow(30L, p1 = 0.7, p2 = 0.65)
  shift <- 30 * (0.7 - 0.5) + 30 * 29 * (0.65 - 0.5) / 2
  sd0 <- sqrt(30 * 31 * 61 / 24)
  expect_equal(p$shift, shift)
  expect_equal(p$sd0, sd0)
  expect_equal(p$z_beta, shift / sd0 - stats::qnorm(0.95))
  expect_equal(p$power, stats::pnorm(p$z_beta))
  expect_true(p$power >= 0 && p$power <= 1)

  # power rises with n and with the departure from 1/2
  pw <- vapply(c(10L, 20L, 40L, 80L),
               function(n) rmorie:::Wsrpow(n, 0.7, 0.65)$power, 0)
  expect_true(all(diff(pw) > 0))
  p2s <- vapply(c(0.55, 0.6, 0.7, 0.8),
                function(q) rmorie:::Wsrpow(30L, 0.7, q)$power, 0)
  expect_true(all(diff(p2s) > 0))
  # a larger alpha buys power
  expect_gt(rmorie:::Wsrpow(30L, 0.7, 0.65, alpha = 0.10)$power, p$power)
  # at the null the power is the level
  expect_equal(rmorie:::Wsrpow(30L, 0.5, 0.5)$power, 0.05, tolerance = 1e-9)

  # the sample-size formula
  s <- rmorie:::Wsrn(0.65, alpha = 0.05, beta = 0.05)
  raw <- (stats::qnorm(0.95) + stats::qnorm(0.95))^2 / (3 * (0.65 - 0.5)^2)
  expect_equal(s$n_raw, raw)
  expect_equal(s$n, as.integer(ceiling(raw)))
  # two-sided spends alpha on both tails, so it needs more
  expect_gte(rmorie:::Wsrn(0.65, twosided = TRUE)$n, s$n)
  # a smaller effect needs a larger sample, symmetric about 1/2
  ns <- vapply(c(0.9, 0.8, 0.7, 0.6, 0.55),
               function(q) rmorie:::Wsrn(q)$n, 0L)
  expect_true(all(diff(ns) > 0))
  expect_equal(rmorie:::Wsrn(0.65)$n, rmorie:::Wsrn(0.35)$n)

  expect_error(rmorie:::Wsrpow(1L, 0.7, 0.6), "at least 2")
  expect_error(rmorie:::Wsrpow(10L, 0.7, 0.6, alpha = 0), "strictly inside")
  expect_error(rmorie:::Wsrn(0.5), "must differ from 0\\.5")
  expect_error(rmorie:::Wsrn(0), "strictly inside")
})

test_that("simulated signed-rank power counts its rejections", {
  # three samples with T+ of 6, 0 and 6
  m <- rbind(c(1, 2, 3), c(-1, -2, -3), c(3, 2, 1))
  r <- rmorie:::Wsrsimpow(m, m0 = 0, tcrit = 6)
  expect_equal(r$rejections, 2L)
  expect_equal(r$power, 2 / 3)
  expect_equal(r$nsim, 3L)
  expect_equal(r$tmean, (6 + 0 + 6) / 3)
  # a threshold nothing reaches, and one nothing misses
  expect_equal(rmorie:::Wsrsimpow(m, 0, 7)$power, 0)
  expect_equal(rmorie:::Wsrsimpow(m, 0, 0)$power, 1)
  # power is monotone decreasing in the critical value
  pw <- vapply(0:7, function(t) rmorie:::Wsrsimpow(m, 0, t)$power, 0)
  expect_true(all(diff(pw) <= 0))
  # a list of unequal-length samples is accepted
  l <- list(c(1, 2), c(-1, -2, -3), c(5, 6, 7, 8))
  expect_equal(rmorie:::Wsrsimpow(l, 0, 3)$nsim, 3L)
  # an all-zero sample has no non-zero differences, so T+ is zero
  expect_equal(rmorie:::Wsrsimpow(list(c(0, 0)), 0, 1)$power, 0)
  expect_error(rmorie:::Wsrsimpow(list(), 0, 1), "non-empty")
})

test_that("the Walsh-average interval is the Hodges-Lehmann one", {
  set.seed(12)
  x <- stats::rnorm(12)
  # the Hodges-Lehmann estimator is the median of the Walsh averages,
  # which is what wilcox.test reports as its estimate
  hl <- rmorie:::Hlwsrlink(x)
  want <- suppressWarnings(stats::wilcox.test(x, conf.int = TRUE))
  expect_equal(hl$estimate, as.numeric(want$estimate))

  # there are n(n+1)/2 Walsh averages
  expect_equal(hl$nwalsh, 12 * 13 / 2)
  # THE COUNTING IDENTITY: T+ is the number of Walsh averages above the
  # hypothesised centre (halving the ties). This is a theorem about the
  # statistic, so it ties the two representations together.
  for (m0 in c(-1, -0.3, 0, 0.5, 2)) {
    link <- rmorie:::Hlwsrlink(x, m0 = m0)
    expect_equal(link$tplus, rmorie:::Wsr(x, m0 = m0)$statistic)
    expect_equal(link$tplus + link$tminus, link$nwalsh)
    expect_equal(link$nbelow + link$nequal + link$nabove, link$nwalsh)
  }

  ci <- rmorie:::Wsrci(x, tcrit = 13L)
  # the endpoints are Walsh order statistics
  walsh <- sort(as.numeric(outer(x, x, "+")[!lower.tri(matrix(0, 12, 12))]) / 2)
  expect_equal(ci$lower, walsh[14L])
  expect_equal(ci$upper, walsh[length(walsh) - 13L])
  expect_equal(ci$nwalsh, length(walsh))
  expect_equal(ci$estimate, stats::median(walsh))
  # the coverage comes from the exact null distribution
  expect_equal(ci$tail, stats::psignrank(13, 12))
  expect_equal(ci$coverage, 1 - 2 * stats::psignrank(13, 12))
  # the interval contains its own point estimate
  expect_true(ci$lower <= ci$estimate && ci$estimate <= ci$upper)
  # a smaller critical value is a wider interval with more coverage
  wide <- rmorie:::Wsrci(x, tcrit = 5L)
  expect_lte(wide$lower, ci$lower)
  expect_gte(wide$upper, ci$upper)
  expect_gt(wide$coverage, ci$coverage)

  expect_error(rmorie:::Wsrci(1, 1L), "at least 2")
  expect_error(rmorie:::Wsrci(x, -1L), "non-negative")
  expect_error(rmorie:::Wsrci(x, 200L), "too large")
  expect_error(rmorie:::Hlwsrlink(1), "at least 2")
})

test_that("the symmetry test standardises T+ about its null mean", {
  set.seed(13)
  x <- stats::rnorm(20)
  s <- rmorie:::Wsrsym(x)
  expect_equal(s$statistic, rmorie:::Wsr(x)$statistic)
  expect_equal(s$mean, 20 * 21 / 4)
  expect_equal(s$var, 20 * 21 * 41 / 24)
  expect_equal(s$z, (s$statistic - s$mean) / sqrt(s$var))
  expect_equal(s$p_value, 2 * (1 - stats::pnorm(abs(s$z))))
  # the reported direction follows the statistic's side of the mean
  expect_equal(s$skewdir, if (s$statistic > s$mean) 1L else -1L)
  # T+ is a rank sum, so it is driven by HOW MANY differences are
  # positive rather than how large they are. Many small positives with a
  # few large negatives puts T+ above its null mean...
  right <- rmorie:::Wsrsym(c(rep(1, 8), -20, -30, -40))
  expect_equal(right$skewdir, 1L)
  expect_gt(right$statistic, right$mean)
  # ...and the mirror image puts it below, even though the large values
  # are the positive ones. That insensitivity to magnitude is the point
  # of a rank test, not a defect.
  left <- rmorie:::Wsrsym(c(rep(-1, 8), 20, 30, 40))
  expect_equal(left$skewdir, -1L)
  expect_lt(left$statistic, left$mean)
  # a perfectly balanced sample sits at the mean, with no direction
  bal <- rmorie:::Wsrsym(c(-2, -1, 1, 2))
  expect_equal(bal$statistic, bal$mean)
  expect_equal(bal$skewdir, 0L)
  expect_equal(bal$z, 0)
  expect_equal(bal$p_value, 1)
  # the centre is subtracted first, and zeros dropped
  expect_equal(rmorie:::Wsrsym(c(1, 2, 3), centre = 2)$n, 2L)
  expect_error(rmorie:::Wsrsym(c(1, 0)), "at least 2 non-zero")
})
