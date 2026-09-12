# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Sec. 6.4: the two-sample median test, its confidence interval, its
# exact critical region, the three conventions for ties at the combined
# median, and the precedence-test power calculation.
#
# Anchors outside the module: stats::dhyper, since the null distribution
# of the median-test statistic IS hypergeometric; the closed-form
# integrals Simpson's rule must reproduce exactly (it is exact for
# cubics); and the order statistics the interval is built from,
# recomputed by hand.

test_that("the median-test statistic is hypergeometric under the null", {
  set.seed(91)
  x <- stats::rnorm(9)
  y <- stats::rnorm(11, mean = 1)
  got <- rmorie:::Medtest(x, y)
  m <- 9
  n <- 11
  nn <- 20
  pooled <- sort(c(x, y))
  med <- (pooled[10] + pooled[11]) / 2
  expect_equal(got$median, med)
  expect_equal(got$t, sum(pooled > med))
  expect_equal(got$statistic, sum(x > med))

  # The number of x observations above the combined median, given that t
  # of the N are above it, is hypergeometric: choosing t of N without
  # replacement, how many come from the first sample.
  t <- got$t
  for (k in 0:m) {
    pk <- if (k > t || t - k > n) 0 else
      choose(m, k) * choose(n, t - k) / choose(nn, t)
    expect_equal(pk, stats::dhyper(k, m, n, t))
  }
  # so the tails are the hypergeometric tails
  u <- got$statistic
  expect_equal(got$p_lower, stats::phyper(u, m, n, t))
  expect_equal(got$p_upper, 1 - stats::phyper(u - 1, m, n, t))
  expect_equal(got$p_value, min(1, 2 * min(got$p_lower, got$p_upper)))
  # and the moments are the hypergeometric ones
  expect_equal(got$mean, m * t / nn)
  expect_equal(got$var, m * n * t * (nn - t) / (nn^2 * (nn - 1)))

  # an even split is no evidence; complete separation is the most there
  # can be
  expect_equal(rmorie:::Medtest(c(1, 2), c(3, 4))$statistic, 0)
  expect_equal(rmorie:::Medtest(c(3, 4), c(1, 2))$statistic, 2)
  expect_lt(rmorie:::Medtest(stats::rnorm(40),
                             stats::rnorm(40, 3))$p_value, 0.01)
  expect_gt(rmorie:::Medtest(stats::rnorm(40), stats::rnorm(40))$p_value,
            0.01)
  expect_error(rmorie:::Medtest(numeric(0), 1), "non-empty")
})

test_that("the median test is Fisher's exact test on the 2 x 2 table", {
  # the median test tabulates each sample above and below the combined
  # median, so its exact distribution is the same hypergeometric one
  # fisher.test uses -- a useful cross-check on the arithmetic
  set.seed(92)
  x <- stats::rnorm(12)
  y <- stats::rnorm(14, 0.8)
  got <- rmorie:::Medtest(x, y)
  med <- got$median
  tb <- rbind(c(sum(x > med), sum(x <= med)),
              c(sum(y > med), sum(y <= med)))
  # the one-sided tails agree with fisher.test's
  expect_equal(got$p_upper,
               stats::fisher.test(tb, alternative = "greater")$p.value,
               tolerance = 1e-9)
  expect_equal(got$p_lower,
               stats::fisher.test(tb, alternative = "less")$p.value,
               tolerance = 1e-9)
  # the two-sided values need not agree, because this doubles the
  # smaller tail while fisher.test sums the outcomes no more likely than
  # the observed one
  expect_true(is.finite(got$p_value))
})

test_that("the median-test interval is built from order statistics", {
  x <- c(1, 3, 5, 7, 9)
  y <- c(2, 4, 6, 8, 10, 12)
  ci <- rmorie:::Medtestci(x, y, c = 2L)
  xs <- sort(x)
  ys <- sort(y)
  expect_equal(ci$lower, ys[2] - xs[5 - 2 + 1])
  expect_equal(ci$upper, ys[6 - 2 + 1] - xs[2])
  expect_equal(ci$estimate, stats::median(ys) - stats::median(xs))
  expect_equal(ci$c, 2L)
  # the interval brackets its own estimate
  expect_true(ci$lower <= ci$estimate && ci$estimate <= ci$upper)
  # a smaller c reaches further out into both samples, so the interval
  # is wider
  wide <- rmorie:::Medtestci(x, y, c = 1L)
  expect_lte(wide$lower, ci$lower)
  expect_gte(wide$upper, ci$upper)
  # c at its maximum is the narrowest available
  expect_equal(rmorie:::Medtestci(x, y, c = 5L)$c, 5L)
  expect_error(rmorie:::Medtestci(x, y, c = 0L), "1\\.\\.min")
  expect_error(rmorie:::Medtestci(x, y, c = 6L), "1\\.\\.min")
  expect_error(rmorie:::Medtestci(numeric(0), y, 1L), "non-empty")
})

test_that("Simpson's rule reproduces the integrals it must", {
  # Simpson's rule is EXACT for polynomials up to cubic, so these are
  # identities rather than approximations
  expect_equal(rmorie:::.gbSimpson(function(u) 1), 1)
  expect_equal(rmorie:::.gbSimpson(function(u) u), 1 / 2)
  expect_equal(rmorie:::.gbSimpson(function(u) u^2), 1 / 3)
  expect_equal(rmorie:::.gbSimpson(function(u) u^3), 1 / 4)
  # and accurate beyond that
  expect_equal(rmorie:::.gbSimpson(function(u) u^4), 1 / 5,
               tolerance = 1e-10)
  expect_equal(rmorie:::.gbSimpson(function(u) exp(u)), exp(1) - 1,
               tolerance = 1e-10)
  expect_equal(rmorie:::.gbSimpson(function(u) sin(pi * u)), 2 / pi,
               tolerance = 1e-9)
  # a beta density integrates to one
  expect_equal(rmorie:::.gbSimpson(function(u) stats::dbeta(u, 3, 5)), 1,
               tolerance = 1e-8)
  # an even node count is bumped to odd, since the rule needs pairs of
  # intervals -- the answer is unaffected
  expect_equal(rmorie:::.gbSimpson(function(u) u^2, nodes = 100),
               rmorie:::.gbSimpson(function(u) u^2, nodes = 101))
  # more nodes is at least as accurate
  e1 <- abs(rmorie:::.gbSimpson(function(u) u^5, nodes = 11) - 1 / 6)
  e2 <- abs(rmorie:::.gbSimpson(function(u) u^5, nodes = 1001) - 1 / 6)
  expect_lt(e2, e1)
})

test_that("the precedence power distribution is a distribution", {
  # g is the alternative's distribution function on (0, 1); the uniform
  # g(u) = u is the null
  null_g <- function(u) u
  p <- rmorie:::Medtestpow(8L, 6L, r = 4L, wcrit = 3L, g = null_g,
                           nodes = 401)
  expect_length(p$pmf, 7L)
  expect_equal(sum(p$pmf), 1, tolerance = 1e-6)
  expect_true(all(p$pmf >= -1e-12))
  expect_equal(p$power, sum(p$pmf[1:3]))
  expect_true(p$power >= 0 && p$power <= 1)
  expect_equal(c(p$m, p$n, p$r), c(8L, 6L, 4L))

  # a stochastically larger alternative puts more of the second sample
  # above the precedence point, which moves the power
  shifted <- rmorie:::Medtestpow(8L, 6L, r = 4L, wcrit = 3L,
                                 g = function(u) u^2, nodes = 401)
  expect_equal(sum(shifted$pmf), 1, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(shifted$power, p$power)))
  # a wider rejection region cannot lower the power
  expect_gte(rmorie:::Medtestpow(8L, 6L, 4L, 5L, null_g,
                                 nodes = 401)$power, p$power)
  # and a zero-width one has none
  expect_equal(rmorie:::Medtestpow(8L, 6L, 4L, 0L, null_g,
                                   nodes = 401)$power, 0)
  # g is clamped to [0, 1], so an out-of-range alternative cannot
  # produce a negative probability
  expect_true(all(rmorie:::Medtestpow(6L, 5L, 3L, 2L,
                                      function(u) 2 * u - 0.5,
                                      nodes = 401)$pmf >= -1e-12))

  expect_error(rmorie:::Medtestpow(0L, 5L, 1L, 1L, null_g), "at least 1")
  expect_error(rmorie:::Medtestpow(5L, 5L, 6L, 1L, null_g), "1 <= r <= m")
  expect_error(rmorie:::Medtestpow(5L, 5L, 0L, 1L, null_g), "1 <= r <= m")
})

test_that("the exact critical region respects the stated level", {
  set.seed(93)
  x <- stats::rnorm(10)
  y <- stats::rnorm(10, 1)
  r <- rmorie:::Medtest2(x, y, alpha = 0.05)
  expect_equal(r$statistic, rmorie:::Medtest(x, y)$statistic)
  expect_equal(r$p_value, rmorie:::Medtest(x, y)$p_value)
  # the attained level never exceeds the nominal one, which is the whole
  # point of an exact region
  expect_lte(r$alpha_exact, 0.05 + 1e-12)
  # the two bounds lie inside the statistic's range when they exist
  if (!is.nan(r$lower)) expect_true(r$lower >= 0 && r$lower <= 10)
  if (!is.nan(r$upper)) expect_true(r$upper >= 0 && r$upper <= 10)
  # a larger alpha admits a wider region and a larger attained level
  loose <- rmorie:::Medtest2(x, y, alpha = 0.20)
  expect_gte(loose$alpha_exact, r$alpha_exact)
  # a very small alpha may admit no region at all, which is reported as
  # NaN rather than as a region that does not hold its level
  tiny <- rmorie:::Medtest2(x, y, alpha = 1e-8)
  expect_true(is.nan(tiny$lower) || tiny$alpha_exact <= 1e-8)
  expect_error(rmorie:::Medtest2(x, y, alpha = 0), "strictly inside")
  expect_error(rmorie:::Medtest2(x, y, alpha = 1), "strictly inside")
})

test_that("the three tie conventions bracket one another", {
  # observations sitting exactly on the combined median have to be
  # assigned, and the choice changes the statistic
  x <- c(1, 2, 3, 3, 3)
  y <- c(3, 3, 4, 5, 6)
  t <- rmorie:::Medties(x, y)
  expect_equal(t$median, 3)
  expect_equal(t$nties, 5)
  expect_equal(t$m, 5L)
  expect_equal(t$n, 5L)
  # strict counts only what is ABOVE, inclusive counts the ties too, and
  # the split convention halves them -- so the three are ordered
  expect_equal(t$u_strict, sum(x > 3))
  expect_equal(t$u_inclusive, sum(x > 3) + sum(x == 3))
  expect_equal(t$u_split, t$u_strict + (t$u_inclusive - t$u_strict) / 2)
  expect_lte(t$u_strict, t$u_split)
  expect_lte(t$u_split, t$u_inclusive)
  # the same holds for the pooled count
  expect_lte(t$t_strict, t$t_inclusive)
  expect_equal(t$t_inclusive - t$t_strict, t$nties)

  # with no ties all three conventions coincide, which is why the choice
  # only matters on discrete or rounded data
  clean <- rmorie:::Medties(c(1, 2), c(3, 4))
  expect_equal(clean$nties, 0)
  expect_equal(clean$u_strict, clean$u_inclusive)
  expect_equal(clean$u_strict, clean$u_split)
  expect_equal(clean$t_strict, clean$t_inclusive)
  expect_error(rmorie:::Medties(numeric(0), 1), "non-empty")
})
