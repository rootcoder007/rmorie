# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 3: tests of randomness -- the runs test with both its
# parameterisations, runs up and down (Levene's moments), the exact runs
# distribution and its critical region, and the rank von Neumann ratio
# (Bartels).
#
# Anchors outside the module: the closed-form moments written out by
# hand, the identity that untied ranks have sum of squared deviations
# n(n^2 - 1)/12, the exact runs distribution already verified against an
# exhaustive enumeration elsewhere, and the requirement that an exact
# critical region hold its stated level.

test_that("the runs statistic carries both of its standardisations", {
  n1 <- 12L
  n2 <- 8L
  n <- 20
  for (r in c(5, 10, 15)) {
    got <- rmorie:::Runsz(r, n1, n2)
    # the asymptotic form uses lambda = n1 / n
    lam <- 12 / 20
    expect_equal(got$lam, lam)
    expect_equal(got$mean, 2 * n * lam * (1 - lam))
    expect_equal(got$var, (2 * sqrt(n) * lam * (1 - lam))^2)
    expect_equal(got$z, (r - got$mean) / sqrt(got$var))
    expect_equal(got$p_value, 2 * (1 - stats::pnorm(abs(got$z))))
    # the exact-moment form uses the hypergeometric-style moments
    expect_equal(got$mean_exact, 2 * 12 * 8 / n + 1)
    expect_equal(got$var_exact,
                 2 * 12 * 8 * (2 * 12 * 8 - n) / (n^2 * (n - 1)))
    expect_equal(got$z_exact,
                 (r - got$mean_exact) / sqrt(got$var_exact))
  }
  # the two means differ by exactly one, since the asymptotic form drops
  # the +1 that counting runs requires
  g <- rmorie:::Runsz(10, n1, n2)
  expect_equal(g$mean_exact - g$mean, 1)
  # the continuity correction always moves the statistic toward its mean
  for (r in c(5, 15)) {
    expect_lte(abs(rmorie:::Runsz(r, n1, n2, correct = TRUE)$z),
               abs(rmorie:::Runsz(r, n1, n2)$z))
  }
  # at the mean the correction has nothing to do
  expect_equal(rmorie:::Runsz(g$mean, n1, n2, correct = TRUE)$z, 0)
  # too few runs is evidence of clustering, too many of alternation, and
  # both are significant at the extremes
  expect_lt(rmorie:::Runsz(2, 20L, 20L)$p_value, 0.001)
  expect_lt(rmorie:::Runsz(40, 20L, 20L)$p_value, 0.001)
  expect_error(rmorie:::Runsz(5, 0L, 3L), "at least 1")
})

test_that("runs up and down use Levene's moments", {
  for (n in c(10L, 25L, 100L)) {
    m <- rmorie:::Runsudvar(n)
    # the number of runs up and down in a random permutation
    expect_equal(m$mean, (2 * n - 1) / 3)
    expect_equal(m$var, (16 * n - 29) / 90)
    expect_equal(m$sd, sqrt(m$var))
    expect_equal(m$n, n)
    # with no observed count there is nothing to test
    expect_true(is.nan(m$z_left))
    expect_true(is.nan(m$p_value))
    expect_equal(m$zcrit, stats::qnorm(0.975))
  }
  # a count at the mean is no evidence either way
  n <- 25L
  mu <- (2 * 25 - 1) / 3
  at_mean <- rmorie:::Runsudvar(n, r = mu)
  expect_gt(at_mean$p_value, 0.9)
  # the two z values bracket the count, since one uses +1/2 and the
  # other -1/2 -- the continuity correction on a discrete count
  expect_gt(at_mean$z_left, at_mean$z_right)
  expect_equal(at_mean$z_left - at_mean$z_right, 1 / at_mean$sd)
  # too few runs means a trend; too many means oscillation
  expect_lt(rmorie:::Runsudvar(50L, r = 10)$p_value, 0.001)
  expect_lt(rmorie:::Runsudvar(50L, r = 60)$p_value, 0.001)
  # a stricter alpha needs a larger critical value
  expect_gt(rmorie:::Runsudvar(25L, alpha = 0.01)$zcrit,
            rmorie:::Runsudvar(25L, alpha = 0.05)$zcrit)
  expect_error(rmorie:::Runsudvar(1L), "at least 2")
})

test_that("the rank von Neumann ratio is Bartels' statistic", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  n <- 10
  got <- rmorie:::Rvntest(x)
  ranks <- rank(x, ties.method = "average")
  # the numerator is the sum of squared successive rank differences
  expect_equal(got$nm, sum((ranks[-n] - ranks[-1])^2))
  # and the denominator the total sum of squared deviations
  expect_equal(got$denom, sum((ranks - (n + 1) / 2)^2))
  expect_equal(got$statistic, got$nm / got$denom)
  # the null mean is 2, and the variance is Bartels'
  expect_equal(got$mean, 2)
  expect_equal(got$var,
               4 * (n - 2) * (5 * n^2 - 2 * n - 9) /
                 (5 * n * (n + 1) * (n - 1)^2))
  expect_equal(got$z, (got$statistic - 2) / sqrt(got$var))

  # WITH NO TIES the denominator is exactly n(n^2 - 1)/12, which is what
  # makes the statistic a pure function of the permutation
  untied <- rmorie:::Rvntest(c(5, 2, 8, 1, 9, 3, 7, 4, 6, 10))
  expect_equal(untied$denom, 10 * (10^2 - 1) / 12)
  expect_equal(rmorie:::Rvnmom(10L)$denom, 10 * (10^2 - 1) / 12)

  # a monotone series has the smallest possible numerator -- every
  # successive difference is one -- so RVN is near zero and the test
  # rejects randomness
  mono <- rmorie:::Rvntest(1:20)
  expect_equal(mono$nm, 19)
  expect_lt(mono$statistic, 0.1)
  expect_lt(mono$p_value, 0.001)
  # a strongly alternating series has a large numerator instead
  alt <- rmorie:::Rvntest(c(1, 20, 2, 19, 3, 18, 4, 17, 5, 16))
  expect_gt(alt$statistic, 2)
  # the alternatives
  expect_equal(rmorie:::Rvntest(x, "less")$p_value, stats::pnorm(got$z))
  expect_equal(rmorie:::Rvntest(x, "greater")$p_value,
               1 - stats::pnorm(got$z))
  expect_error(rmorie:::Rvntest(c(1, 2)), "at least 3")
  expect_error(rmorie:::Rvntest(x, "sideways"),
               "two-sided, less or greater")

  # the moments in their own right, with the published approximation
  for (nn in c(10L, 30L, 100L)) {
    mm <- rmorie:::Rvnmom(nn)
    expect_equal(mm$mean, 2)
    expect_equal(mm$var, 4 * (nn - 2) * (5 * nn^2 - 2 * nn - 9) /
                   (5 * nn * (nn + 1) * (nn - 1)^2))
    expect_equal(mm$var_approx, 20 / (5 * nn + 7))
    # the approximation closes on the exact variance as n grows
    expect_equal(mm$var, mm$var_approx, tolerance = 0.3)
  }
  expect_lt(abs(rmorie:::Rvnmom(200L)$var - rmorie:::Rvnmom(200L)$var_approx),
            abs(rmorie:::Rvnmom(10L)$var - rmorie:::Rvnmom(10L)$var_approx))
  expect_error(rmorie:::Rvnmom(2L), "at least 3")
})

test_that("the exact runs table is a distribution with the right moments", {
  for (n1 in 2:6) {
    for (n2 in 2:6) {
      t <- rmorie:::Runstab(n1, n2)
      n <- n1 + n2
      expect_equal(t$support, 2:n)
      expect_equal(sum(t$pmf), 1)
      expect_equal(t$cdf, cumsum(t$pmf))
      expect_equal(t$cdf[length(t$cdf)], 1)
      # the closed-form moments of the number of runs
      expect_equal(t$mean, 2 * n1 * n2 / n + 1, tolerance = 1e-9)
      expect_equal(t$var,
                   2 * n1 * n2 * (2 * n1 * n2 - n) / (n^2 * (n - 1)),
                   tolerance = 1e-9)
    }
  }
  # the point and tail probabilities at a given r
  t <- rmorie:::Runstab(5L, 5L)
  for (r in 2:10) {
    q <- rmorie:::Runstab(5L, 5L, r = r)
    expect_equal(q$pmf_r, t$pmf[r - 1L])
    expect_equal(q$cdf_r, t$cdf[r - 1L])
    expect_equal(q$sf_r, 1 - (if (r > 2L) t$cdf[r - 2L] else 0))
  }
  # two runs is the fully separated arrangement, of which there are two
  expect_equal(rmorie:::Runstab(5L, 5L, r = 2L)$pmf_r, 2 / choose(10, 5))
  expect_error(rmorie:::Runstab(5L, 5L, r = 1L), "2\\.\\.n1\\+n2")
  expect_error(rmorie:::Runstab(5L, 5L, r = 11L), "2\\.\\.n1\\+n2")
  expect_error(rmorie:::Runstab(0L, 3L), "at least 1")
})

test_that("the exact critical region holds its stated level", {
  for (alpha in c(0.05, 0.10, 0.20)) {
    cr <- rmorie:::Runscrit(10L, 10L, alpha = alpha)
    t <- rmorie:::Runstab(10L, 10L)
    # each tail's attained level is within its half of alpha
    if (!is.nan(cr$lower)) {
      idx <- cr$lower - 1L
      expect_lte(sum(t$pmf[seq_len(idx)]), alpha / 2 + 1e-12)
    }
    if (!is.nan(cr$upper)) {
      idx <- cr$upper - 1L
      expect_lte(sum(t$pmf[idx:length(t$pmf)]), alpha / 2 + 1e-12)
    }
    # the lower bound sits below the upper one
    if (!is.nan(cr$lower) && !is.nan(cr$upper)) {
      expect_lt(cr$lower, cr$upper)
    }
  }
  # a one-sided region spends the whole of alpha on that side, so it
  # reaches further in than either half of a two-sided one
  left <- rmorie:::Runscrit(10L, 10L, 0.05, tail = "left")
  two <- rmorie:::Runscrit(10L, 10L, 0.05, tail = "two-sided")
  expect_true(is.nan(left$upper))
  expect_gte(left$lower, two$lower)
  right <- rmorie:::Runscrit(10L, 10L, 0.05, tail = "right")
  expect_true(is.nan(right$lower))
  expect_lte(right$upper, two$upper)
  # a larger alpha admits a wider region
  loose <- rmorie:::Runscrit(10L, 10L, 0.20)
  expect_gte(loose$lower, two$lower)
  expect_error(rmorie:::Runscrit(10L, 10L, tail = "sideways"),
               "two-sided, left or right")
  expect_error(rmorie:::Runscrit(0L, 5L), "at least 1")
})
