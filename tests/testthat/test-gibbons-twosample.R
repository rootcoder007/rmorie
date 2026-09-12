# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Sec. 6.2-6.3: the Wald-Wolfowitz two-sample runs test with its tie
# bounds, and the two-sample Kolmogorov-Smirnov (Smirnov) test with its
# exact lattice-path distribution.
#
# Anchors outside the module: stats::ks.test and stats::psmirnov for the
# two-sample statistic and its exact distribution, the closed-form runs
# pmf and its moments written out by hand, and an exhaustive enumeration
# of the arrangements for the runs distribution at small sizes.

test_that("the runs pmf is its closed form and a distribution", {
  for (m in 2:7) {
    for (n in 2:7) {
      nn <- m + n
      pmf <- rmorie:::.gbRunsPmf(m, n)
      expect_length(pmf, nn - 1L)
      expect_equal(sum(pmf), 1)
      expect_true(all(pmf >= 0))
      # the moments of the number of runs
      support <- 2:nn
      expect_equal(sum(support * pmf), 2 * m * n / nn + 1)
      v <- 2 * m * n * (2 * m * n - nn) / (nn^2 * (nn - 1))
      expect_equal(sum(support^2 * pmf) - (sum(support * pmf))^2, v,
                   tolerance = 1e-9)
    }
  }
  # EXHAUSTIVE CHECK: enumerate every arrangement of m zeros and n ones
  # and count its runs, for a size where that is feasible
  enumerate_runs <- function(m, n) {
    nn <- m + n
    idx <- utils::combn(nn, m)
    counts <- integer(nn - 1L)
    for (c in seq_len(ncol(idx))) {
      lab <- rep(1L, nn)
      lab[idx[, c]] <- 0L
      r <- 1L + sum(lab[-1] != lab[-nn])
      counts[r - 1L] <- counts[r - 1L] + 1L
    }
    counts / ncol(idx)
  }
  for (m in 2:5) {
    for (n in 2:5) {
      expect_equal(rmorie:::.gbRunsPmf(m, n), enumerate_runs(m, n))
    }
  }
  # the two names for the same function agree
  expect_equal(rmorie:::.gbRunsPmf2(4, 5), rmorie:::.gbRunsPmf(4, 5))
  # two runs is the fully separated arrangement, of which there are two
  expect_equal(rmorie:::.gbRunsPmf(3, 3)[1], 2 / choose(6, 3))
})

test_that("the Wald-Wolfowitz runs statistic counts the alternations", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  # fully separated: one run of x then one of y
  w <- rmorie:::Wwruns(x, y)
  expect_equal(w$statistic, 2L)
  # perfectly interleaved gives the maximum
  alt <- rmorie:::Wwruns(c(1, 3, 5), c(2, 4, 6))
  expect_equal(alt$statistic, 6L)
  # the moments
  expect_equal(w$mean, 2 * 3 * 3 / 6 + 1)
  expect_equal(w$var, 2 * 3 * 3 * (2 * 3 * 3 - 6) / (36 * 5))
  expect_equal(w$z, (2 - w$mean) / sqrt(w$var))
  expect_equal(w$p_normal, stats::pnorm(w$z))
  # the left tail from the exact pmf
  pmf <- rmorie:::.gbRunsPmf(3, 3)
  expect_equal(w$p_value, pmf[1])
  expect_equal(alt$p_value, 1)
  # too few runs is evidence that the samples differ, so the test is
  # one-sided to the left by construction
  expect_lt(w$p_value, alt$p_value)

  set.seed(41)
  a <- stats::rnorm(8)
  b <- stats::rnorm(9)
  r <- rmorie:::Wwruns(a, b)
  # recomputed by hand from the pooled ordering
  lab <- c(rep(0L, 8), rep(1L, 9))[order(c(a, b))]
  expect_equal(r$statistic, 1L + sum(lab[-1] != lab[-17]))
  expect_equal(r$m, 8L)
  expect_equal(r$n, 9L)
  expect_error(rmorie:::Wwruns(numeric(0), 1), "non-empty")
})

test_that("the exact runs test reports both tails", {
  set.seed(42)
  x <- stats::rnorm(6)
  y <- stats::rnorm(7)
  e <- rmorie:::Wwexact(x, y, tail = "left")
  pmf <- rmorie:::.gbRunsPmf(6, 7)
  support <- 2:13
  expect_equal(e$support, support)
  expect_equal(e$pmf, pmf)
  expect_equal(e$p_left, sum(pmf[support <= e$statistic]))
  expect_equal(e$p_right, sum(pmf[support >= e$statistic]))
  expect_equal(e$p_value, e$p_left)
  expect_equal(rmorie:::Wwexact(x, y, "right")$p_value, e$p_right)
  expect_equal(rmorie:::Wwexact(x, y, "two-sided")$p_value,
               min(1, 2 * min(e$p_left, e$p_right)))
  # the two tails overlap at the observed value
  expect_equal(e$p_left + e$p_right,
               1 + pmf[support == e$statistic])
  # the statistic agrees with the other entry point
  expect_equal(e$statistic, rmorie:::Wwruns(x, y)$statistic)
  # the extremes
  expect_equal(rmorie:::Wwexact(c(1, 2), c(3, 4), "left")$p_value,
               rmorie:::.gbRunsPmf(2, 2)[1])
  expect_equal(rmorie:::Wwexact(c(1, 2), c(3, 4), "right")$p_value, 1)
  expect_error(rmorie:::Wwexact(x, y, "sideways"),
               "left, right or two-sided")
})

test_that("the tie bounds bracket the attainable run counts", {
  # no ties: the bounds coincide, so nothing is ambiguous
  clean <- rmorie:::Wwties(c(1, 2, 3), c(4, 5, 6))
  expect_equal(clean$nties, 0L)
  expect_equal(clean$rmin, clean$rmax)
  expect_equal(clean$ambiguous, 0L)

  # a shared value could be broken either way, so the run count is only
  # bounded, not determined
  tied <- rmorie:::Wwties(c(1, 2, 2), c(2, 3, 4))
  expect_equal(tied$nties, 1L)
  expect_lte(tied$rmin, tied$rmax)
  expect_equal(tied$ambiguous, as.integer(tied$rmin != tied$rmax))
  # the bounds are attainable run counts, so they lie in 2..m+n
  for (b in c(tied$rmin, tied$rmax)) {
    expect_true(b >= 1L && b <= 6L)
  }
  # the observed statistic on one particular tie-break lies within them
  obs <- rmorie:::Wwruns(c(1, 2, 2), c(2, 3, 4))$statistic
  expect_gte(obs, tied$rmin)
  expect_lte(obs, tied$rmax)

  # every value shared: the bounds are as far apart as they get
  allshared <- rmorie:::Wwties(c(1, 1, 2), c(1, 2, 2))
  expect_equal(allshared$nties, 2L)
  expect_equal(allshared$ambiguous, 1L)
  expect_lt(allshared$rmin, allshared$rmax)
  expect_error(rmorie:::Wwties(numeric(0), 1), "non-empty")
})

test_that("the two-sample KS statistic matches stats::ks.test", {
  set.seed(43)
  for (i in 1:6) {
    x <- stats::rnorm(11)
    y <- stats::rnorm(13, mean = i / 3)
    got <- rmorie:::Ks2(x, y)
    want <- suppressWarnings(stats::ks.test(x, y))
    expect_equal(got$statistic, as.numeric(want$statistic))
    # the exact p-value from the lattice-path count
    expect_equal(got$p_value, want$p.value, tolerance = 1e-8)
  }
  # D is the larger one-sided deviation
  x <- c(1, 2, 3, 4)
  y <- c(2.5, 3.5, 4.5)
  k <- rmorie:::Ks2(x, y)
  expect_equal(k$statistic, max(k$dplus, k$dminus))
  # computed by hand at the pooled support
  pts <- sort(unique(c(x, y)))
  sm <- vapply(pts, function(t) sum(x <= t) / 4, 0)
  sn <- vapply(pts, function(t) sum(y <= t) / 3, 0)
  expect_equal(k$dplus, max(sm - sn))
  expect_equal(k$dminus, max(sn - sm))
  # completely separated samples give D = 1
  expect_equal(rmorie:::Ks2(c(1, 2), c(10, 20))$statistic, 1)
  expect_equal(rmorie:::Ks2(c(1, 2), c(10, 20))$p_value,
               suppressWarnings(stats::ks.test(c(1, 2),
                                               c(10, 20))$p.value),
               tolerance = 1e-9)
  # identical samples give D = 0 and no evidence
  expect_equal(rmorie:::Ks2(c(1, 2, 3), c(1, 2, 3))$statistic, 0)
  expect_equal(rmorie:::Ks2(c(1, 2, 3), c(1, 2, 3))$p_value, 1)
  expect_error(rmorie:::Ks2(numeric(0), 1), "non-empty")
})

test_that("the exact Smirnov distributions match stats::psmirnov", {
  for (m in c(4L, 7L)) {
    for (n in c(5L, 9L)) {
      for (d in c(0.3, 0.5, 0.75, 1)) {
        two <- rmorie:::Smirnov2(d, m, n)
        # psmirnov is the distribution function of the two-sample
        # statistic, so its upper tail is this survival function
        want <- stats::psmirnov(d, sizes = c(m, n),
                                alternative = "two.sided",
                                exact = TRUE, lower.tail = FALSE)
        expect_equal(two$sf, want, tolerance = 1e-9)
        expect_equal(two$cdf, 1 - two$sf)
        expect_equal(two$npaths, choose(m + n, m))
        expect_true(two$sf >= 0 && two$sf <= 1)

        one <- rmorie:::Smirnov1(d, m, n)
        wone <- stats::psmirnov(d, sizes = c(m, n),
                                alternative = "greater",
                                exact = TRUE, lower.tail = FALSE)
        expect_equal(one$sf, wone, tolerance = 1e-9)
        expect_equal(one$cdf, 1 - one$sf)
        # the one-sided tail is never the larger of the two
        expect_lte(one$sf, two$sf + 1e-12)
        # and its asymptotic form is the single exponential
        expect_equal(one$k, sqrt(m * n / (m + n)) * d)
        expect_equal(one$sf_asymp, exp(-2 * one$k^2))
      }
    }
  }
  # the tails fall as the threshold rises
  sfs <- vapply(seq(0.2, 1, by = 0.1),
                function(d) rmorie:::Smirnov2(d, 8L, 8L)$sf, 0)
  expect_true(all(diff(sfs) <= 0))
  # D = 1 is complete separation, whose probability is the two ways of
  # splitting the pooled order
  expect_equal(rmorie:::Smirnov2(1, 5L, 5L)$sf, 2 / choose(10, 5))
  expect_error(rmorie:::Smirnov2(0, 4L, 4L), "\\(0, 1\\]")
  expect_error(rmorie:::Smirnov2(1.5, 4L, 4L), "\\(0, 1\\]")
  expect_error(rmorie:::Smirnov1(0, 4L, 4L), "\\(0, 1\\]")
})

test_that("the asymptotic two-sample tail uses the effective size", {
  for (m in c(20L, 50L)) {
    for (n in c(30L, 80L)) {
      for (d in c(0.1, 0.25, 0.4)) {
        a <- rmorie:::Ks2asymp(d, m, n)
        expect_equal(a$neff, m * n / (m + n))
        expect_equal(a$k, sqrt(a$neff) * d)
        expect_equal(a$p_value, min(1, max(0, rmorie:::.gbKsQ(a$k))))
        expect_true(a$p_value >= 0 && a$p_value <= 1)
      }
    }
  }
  # zero deviation is no evidence at all
  expect_equal(rmorie:::Ks2asymp(0, 20L, 20L)$p_value, 1)
  # and it agrees with ks.test's own asymptotic p-value
  set.seed(44)
  x <- stats::rnorm(200)
  y <- stats::rnorm(250, 0.3)
  d <- rmorie:::Ks2(x, y)$statistic
  expect_equal(rmorie:::Ks2asymp(d, 200L, 250L)$p_value,
               suppressWarnings(stats::ks.test(x, y,
                                               exact = FALSE))$p.value,
               tolerance = 1e-6)
  # the asymptotic tail approaches the exact one as the samples grow
  expect_equal(rmorie:::Ks2asymp(0.3, 60L, 60L)$p_value,
               rmorie:::Smirnov2(0.3, 60L, 60L)$sf, tolerance = 0.02)
  expect_error(rmorie:::Ks2asymp(0.5, 0L, 5L), "at least 1")
  expect_error(rmorie:::Ks2asymp(-1, 5L, 5L), "non-negative")
})
