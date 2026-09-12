# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 5: the sign test, its exact and approximate p-values, the treatment
# of zero differences, power and sample size, and the median confidence
# interval obtained by inverting the test.
#
# Anchors outside the module: stats::binom.test, which for p = 0.5 gives
# exactly the doubled smaller tail because the binomial is symmetric;
# stats::pbinom and stats::pnorm; and for the median interval the
# definitional coverage statement P(X_(r) <= M <= X_(n-r+1)) =
# 1 - 2 P(Bin(n, 1/2) <= r - 1).

test_that("the sign statistic counts positive differences and drops zeros", {
  x <- c(3, 5, 0, -2, 7, 0, 1, -4)
  s <- rmorie:::Signk(x)
  expect_equal(s$statistic, 4L)
  expect_equal(s$nzero, 2L)
  # zeros leave the sample size used for inference
  expect_equal(s$n, 6L)
  expect_equal(s$n_raw, 8L)
  expect_equal(s$mean, 3)
  expect_equal(s$var, 1.5)
  # the hypothesised median is subtracted first
  s5 <- rmorie:::Signk(c(1, 4, 5, 9), m0 = 5)
  expect_equal(s5$statistic, 1L)
  expect_equal(s5$nzero, 1L)
  expect_equal(s5$n, 3L)
  # every observation above the median is a success
  expect_equal(rmorie:::Signk(c(1, 2, 3), m0 = 0)$statistic, 3L)
  expect_equal(rmorie:::Signk(c(-1, -2, -3))$statistic, 0L)
  # an all-zero sample leaves nothing to test
  allz <- rmorie:::Signk(c(0, 0, 0))
  expect_equal(allz$n, 0L)
  expect_equal(allz$var, 0)
  expect_error(rmorie:::Signk(numeric(0)), "non-empty")
})

test_that("the exact sign-test p-value is stats::binom.test", {
  for (n in c(6L, 11L, 20L)) {
    for (k in 0:n) {
      p <- rmorie:::Signp(k, n)
      # the binomial at p = 1/2 is symmetric, so the doubled smaller tail
      # is exactly binom.test's two-sided p-value
      expect_equal(p$p_value, stats::binom.test(k, n)$p.value)
      expect_equal(p$p_lower, stats::pbinom(k, n, 0.5))
      expect_equal(p$p_upper, 1 - stats::pbinom(k - 1L, n, 0.5))
      expect_equal(rmorie:::Signp(k, n, "less")$p_value, p$p_lower)
      expect_equal(rmorie:::Signp(k, n, "greater")$p_value, p$p_upper)
      # and the one-sided values agree with binom.test's own
      expect_equal(p$p_lower,
                   stats::binom.test(k, n, alternative = "less")$p.value)
      expect_equal(p$p_upper,
                   stats::binom.test(k, n, alternative = "greater")$p.value)
    }
  }
  # the two tails overlap at the observed value, so they sum to 1 + P(X = k)
  expect_equal(rmorie:::Signp(4L, 10L)$p_lower + rmorie:::Signp(4L, 10L)$p_upper,
               1 + stats::dbinom(4, 10, 0.5))
  # the extremes are the smallest attainable p-values
  expect_equal(rmorie:::Signp(0L, 8L, "less")$p_value, 0.5^8)
  expect_equal(rmorie:::Signp(8L, 8L, "greater")$p_value, 0.5^8)
  expect_equal(rmorie:::Signp(0L, 8L)$p_value, 2 * 0.5^8)
  # a statistic at the null mean cannot be evidence against it
  expect_equal(rmorie:::Signp(5L, 10L)$p_value, 1)
  expect_error(rmorie:::Signp(0L, 0L), "at least 1")
  expect_error(rmorie:::Signp(9L, 8L), "0\\.\\.n")
  expect_error(rmorie:::Signp(-1L, 8L), "0\\.\\.n")
  expect_error(rmorie:::Signp(4L, 8L, "sideways"),
               "two-sided, greater or less")
})

test_that("the normal approximation is the continuity-corrected z", {
  n <- 40L
  for (k in c(10L, 18L, 20L, 25L, 33L)) {
    z <- rmorie:::Signz(k, n)
    d <- k - n / 2
    dc <- if (d > 0) d - 0.5 else if (d < 0) d + 0.5 else d
    expect_equal(z$z, dc / sqrt(n / 4))
    expect_equal(z$p_value, min(1, 2 * (1 - stats::pnorm(abs(z$z)))))
    expect_equal(rmorie:::Signz(k, n, "greater")$p_value,
                 1 - stats::pnorm(z$z))
    expect_equal(rmorie:::Signz(k, n, "less")$p_value, stats::pnorm(z$z))
    # the correction always moves the statistic toward the null
    expect_true(abs(z$z) <= abs(rmorie:::Signz(k, n, correct = FALSE)$z))
  }
  # without the correction it is the plain standardisation
  expect_equal(rmorie:::Signz(25L, n, correct = FALSE)$z,
               (25 - 20) / sqrt(10))
  # at the null mean the correction has nothing to do
  expect_equal(rmorie:::Signz(20L, n)$z, 0)
  expect_equal(rmorie:::Signz(20L, n)$p_value, 1)
  # the approximation tracks the exact test for a large sample
  expect_equal(rmorie:::Signz(33L, 40L)$p_value,
               rmorie:::Signp(33L, 40L)$p_value, tolerance = 0.05)
  expect_equal(rmorie:::Signz(25L, n)$mean, 20)
  expect_equal(rmorie:::Signz(25L, n)$var, 10)
  expect_error(rmorie:::Signz(1L, 0L), "at least 1")
  expect_error(rmorie:::Signz(41L, 40L), "0\\.\\.n")
  expect_error(rmorie:::Signz(4L, 40L, "sideways"),
               "two-sided, greater or less")
})

test_that("the three zero-handling conventions differ as documented", {
  x <- c(2, 3, 0, 0, 0, -1, -4, -5, -6)
  # discarding the zeros is the usual convention
  d <- rmorie:::Signzero(x, method = "discard")
  expect_equal(d$statistic, 2)
  expect_equal(d$n, 6L)
  expect_equal(d$nzero, 3L)
  expect_equal(d$k_raw, 2L)
  expect_equal(d$n_raw, 9L)
  # splitting them keeps the full sample size
  h <- rmorie:::Signzero(x, method = "half")
  expect_equal(h$statistic, 2 + 3 / 2)
  expect_equal(h$n, 9L)
  # the conservative rule gives every zero to whichever side is smaller,
  # so it can never favour rejection
  cn <- rmorie:::Signzero(x, method = "conservative")
  expect_equal(cn$statistic, 5)
  expect_equal(cn$n, 9L)
  expect_true(abs(cn$statistic - cn$n / 2) <= abs(h$statistic - h$n / 2))
  # with the positives in the minority the zeros go the other way
  y <- c(5, 6, 7, 8, 0, 0, -1)
  expect_equal(rmorie:::Signzero(y, method = "conservative")$statistic, 4)
  # with no zeros every convention agrees
  z <- c(1, 2, -3, -4, 5)
  for (m in c("discard", "half", "conservative")) {
    r <- rmorie:::Signzero(z, method = m)
    expect_equal(r$statistic, 3)
    expect_equal(r$n, 5L)
  }
  expect_error(rmorie:::Signzero(x, method = "keep"),
               "discard, half or conservative")
  expect_error(rmorie:::Signzero(numeric(0)), "non-empty")
})

test_that("sign-test power rises with n and with the departure from 1/2", {
  p <- rmorie:::Signpow(20L, 0.75, alpha = 0.05)
  # the critical value is the smallest k whose upper tail is within alpha
  expect_equal(p$k_alpha, stats::qbinom(1 - 0.05, 20, 0.5) + 1L)
  expect_equal(p$alpha_exact, 1 - stats::pbinom(p$k_alpha - 1L, 20, 0.5))
  expect_true(p$alpha_exact <= 0.05)
  # the exact power is the binomial tail at the alternative
  expect_equal(p$power_exact, 1 - stats::pbinom(p$k_alpha - 1L, 20, 0.75))
  # and the reported approximation is the normal one
  za <- stats::qnorm(0.95)
  expect_equal(p$power,
               1 - stats::pnorm((20 * (0.5 - 0.75) + 0.5 * sqrt(20) * za) /
                                  sqrt(20 * 0.75 * 0.25)))
  # both are monotone in the sample size and in the alternative
  pw <- vapply(c(10L, 20L, 40L, 80L), function(n) {
    rmorie:::Signpow(n, 0.75)$power_exact
  }, 0)
  expect_true(all(diff(pw) > 0))
  th <- vapply(c(0.55, 0.65, 0.75, 0.9), function(t) {
    rmorie:::Signpow(40L, t)$power_exact
  }, 0)
  expect_true(all(diff(th) > 0))
  # at the null the exact power is the attained level, not the nominal one
  expect_equal(rmorie:::Signpow(20L, 0.5)$power_exact,
               rmorie:::Signpow(20L, 0.5)$alpha_exact)
  # a larger alpha buys power
  expect_true(rmorie:::Signpow(20L, 0.75, alpha = 0.10)$power_exact >=
                p$power_exact)
  # the exact branch can be skipped
  no <- rmorie:::Signpow(20L, 0.75, exact = FALSE)
  expect_true(is.nan(no$power_exact))
  expect_equal(no$power, p$power)
  expect_error(rmorie:::Signpow(0L, 0.75), "at least 1")
  expect_error(rmorie:::Signpow(20L, 0), "strictly inside")
  expect_error(rmorie:::Signpow(20L, 1), "strictly inside")
  expect_error(rmorie:::Signpow(20L, 0.75, alpha = 0), "strictly inside")
})

test_that("simulated power counts the rejections in the supplied samples", {
  # three samples, with 4, 1 and 5 values above the hypothesised median
  m <- rbind(c(1, 2, 3, 4, -1), c(-1, -2, -3, -4, 1), c(1, 2, 3, 4, 5))
  r <- rmorie:::Signsimpow(m, m0 = 0, kcrit = 4L)
  expect_equal(r$rejections, 2L)
  expect_equal(r$power, 2 / 3)
  expect_equal(r$nsim, 3L)
  expect_equal(r$kmean, (4 + 1 + 5) / 3)
  expect_equal(r$kcrit, 4L)
  # a list of unequal-length samples is accepted as well as a matrix
  l <- list(c(1, 2, -1), c(1, 2, 3, 4), c(-1, -2))
  rl <- rmorie:::Signsimpow(l, 0, 2L)
  expect_equal(rl$rejections, 2L)
  expect_equal(rl$nsim, 3L)
  expect_equal(rl$kmean, (2 + 4 + 0) / 3)
  # a critical value nothing can reach gives zero power, one nothing can
  # miss gives one
  expect_equal(rmorie:::Signsimpow(m, 0, 6L)$power, 0)
  expect_equal(rmorie:::Signsimpow(m, 0, 0L)$power, 1)
  # power is monotone decreasing in the critical value
  pw <- vapply(0:6, function(k) rmorie:::Signsimpow(m, 0, k)$power, 0)
  expect_true(all(diff(pw) <= 0))
  # raising the hypothesised median can only lower the counts
  expect_true(rmorie:::Signsimpow(m, 3, 4L)$kmean <= r$kmean)
  expect_error(rmorie:::Signsimpow(list(), 0, 1L), "non-empty")
})

test_that("the sample-size formulas invert the normal power statement", {
  for (theta in c(0.6, 0.7, 0.25)) {
    one <- rmorie:::Signn(theta, alpha = 0.05, beta = 0.10)
    za <- stats::qnorm(0.95)
    zb <- stats::qnorm(0.90)
    root <- (sqrt(theta * (1 - theta)) * zb + 0.5 * za) / (0.5 - theta)
    expect_equal(one$n_raw, root^2)
    expect_equal(one$n, as.integer(ceiling(root^2)))
    expect_equal(one$root_n, abs(root))
    expect_equal(one$z_alpha, za)
    expect_equal(one$z_beta, zb)
    # the two-sided version spends alpha on both tails, so it needs more
    two <- rmorie:::Signnasy(theta, alpha = 0.05, beta = 0.10)
    expect_equal(two$z_alpha, stats::qnorm(0.975))
    expect_true(two$n >= one$n)
  }
  # a smaller effect needs a larger sample, and so does a smaller beta
  ns <- vapply(c(0.9, 0.8, 0.7, 0.6, 0.55), function(t) rmorie:::Signn(t)$n, 0L)
  expect_true(all(diff(ns) > 0))
  expect_true(rmorie:::Signn(0.7, beta = 0.01)$n > rmorie:::Signn(0.7)$n)
  expect_true(rmorie:::Signn(0.7, alpha = 0.01)$n > rmorie:::Signn(0.7)$n)
  # the requirement is symmetric about the null
  expect_equal(rmorie:::Signn(0.7)$n, rmorie:::Signn(0.3)$n)
  expect_equal(rmorie:::Signnasy(0.7)$n, rmorie:::Signnasy(0.3)$n)
  expect_error(rmorie:::Signn(0.5), "must differ from 0\\.5")
  expect_error(rmorie:::Signnasy(0.5), "must differ from 0\\.5")
  expect_error(rmorie:::Signn(0), "strictly inside")
  expect_error(rmorie:::Signnasy(1), "strictly inside")
})

test_that("the median interval inverts the sign test at its stated level", {
  set.seed(3)
  x <- sort(stats::rnorm(15))
  ci <- rmorie:::Signmedci(x, alpha = 0.05)
  # r is the largest rank whose lower tail still fits in alpha / 2
  expect_equal(ci$tail, stats::pbinom(ci$r - 1L, 15L, 0.5))
  expect_true(ci$tail <= 0.025)
  expect_true(stats::pbinom(ci$r, 15L, 0.5) > 0.025)
  expect_equal(ci$s, 15L - ci$r + 1L)
  expect_equal(ci$lower, x[ci$r])
  expect_equal(ci$upper, x[ci$s])
  # the coverage is the definitional one, and is at least 1 - alpha
  expect_equal(ci$coverage, 1 - 2 * ci$tail)
  expect_true(ci$coverage >= 0.95)
  # the interval is symmetric in rank, so it is built from the same number
  # of observations at each end
  expect_equal(ci$r - 1L, 15L - ci$s)
  # a smaller alpha gives a wider interval
  wide <- rmorie:::Signmedci(x, alpha = 0.01)
  expect_true(wide$r <= ci$r)
  expect_true(wide$coverage >= ci$coverage)
  expect_true(wide$upper - wide$lower >= ci$upper - ci$lower)
  # with too few observations no interval attains the level, and the
  # module says so rather than returning the range anyway
  small <- rmorie:::Signmedci(c(1, 2, 3), alpha = 0.05)
  expect_equal(small$r, 0L)
  expect_true(is.nan(small$lower))
  expect_true(is.nan(small$coverage))
  # the level is attainable from n = 6 at alpha = 0.05, since 2 * 0.5^6
  # is under 0.05
  expect_equal(rmorie:::Signmedci(c(1, 2, 3, 4, 5, 6))$r, 1L)
  expect_equal(rmorie:::Signmedci(c(1, 2, 3, 4, 5, 6))$coverage, 1 - 2 * 0.5^6)
  expect_error(rmorie:::Signmedci(1), "at least 2")
  expect_error(rmorie:::Signmedci(x, alpha = 0), "strictly inside")
})
