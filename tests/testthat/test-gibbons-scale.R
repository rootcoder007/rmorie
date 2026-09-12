# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 9: the two-sample scale tests -- Mood, Freund-Ansari-Bradley,
# Siegel-Tukey, Klotz normal scores and the percentile-modified rank
# test -- all of which are linear rank statistics over different score
# functions.
#
# Anchors outside the module: stats::mood.test and stats::ansari.test,
# the general linear-rank moment formulas written out by hand, and the
# fact that every one of these score sets is a function of position in
# the pooled ordering, so the tagging vector and the scores can each be
# checked independently.

test_that("the tagging vector marks the first sample's positions", {
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)
  z <- rmorie:::.gbTagged(x, y)
  # pooled order is 1,2,3,4,5,6 with x at positions 1, 3 and 5
  expect_equal(z, c(1, 0, 1, 0, 1, 0))
  expect_length(z, 6L)
  expect_equal(sum(z), 3)
  # fully separated samples
  expect_equal(rmorie:::.gbTagged(c(1, 2), c(10, 20)), c(1, 1, 0, 0))
  expect_equal(rmorie:::.gbTagged(c(10, 20), c(1, 2)), c(0, 0, 1, 1))
  # a tie puts the first sample first, because the label breaks the tie
  expect_equal(rmorie:::.gbTagged(c(5), c(5)), c(1, 0))
  # the indicator sums to the first sample's size whatever the overlap
  set.seed(81)
  for (i in 1:5) {
    a <- stats::rnorm(7)
    b <- stats::rnorm(9)
    expect_equal(sum(rmorie:::.gbTagged(a, b)), 7)
    expect_length(rmorie:::.gbTagged(a, b), 16L)
  }
})

test_that("the linear-rank moments are their general formulas", {
  for (a in list(1:10, c(1, 1, 2, 3, 5, 8), stats::qnorm((1:8) / 9))) {
    nn <- length(a)
    for (m in 2:(nn - 2)) {
      n <- nn - m
      mv <- rmorie:::.gbLrMoments(a, m, n)
      abar <- mean(a)
      ss <- sum((a - abar)^2)
      # E[sum of m randomly chosen scores] = m * mean(a)
      expect_equal(mv$mean, m * abar)
      # and the sampling-without-replacement variance
      expect_equal(mv$var, m * n * ss / (nn * (nn - 1)))
      expect_gte(mv$var, 0)
    }
  }
  # constant scores have no variance, since every subset sums the same
  expect_equal(rmorie:::.gbLrMoments(rep(3, 6), 2L, 4L)$var, 0)
  expect_equal(rmorie:::.gbLrMoments(rep(3, 6), 2L, 4L)$mean, 6)
})

test_that("Mood's scale statistic matches stats::mood.test", {
  set.seed(82)
  for (i in 1:6) {
    x <- stats::rnorm(10)
    y <- stats::rnorm(12, sd = 1 + i / 4)
    got <- rmorie:::Moodscale(x, y)
    # mood.test's T is the same sum of squared centred ranks over the
    # first sample
    nn <- 22
    z <- rmorie:::.gbTagged(x, y)
    expect_equal(got$statistic,
                 sum((seq_len(nn) - (nn + 1) / 2)^2 * z))
    expect_equal(got$z, as.numeric(stats::mood.test(x, y)$statistic),
                 tolerance = 1e-8)
    expect_equal(got$p_value, stats::mood.test(x, y)$p.value,
                 tolerance = 1e-8)
  }
  # the closed-form null moments
  m <- rmorie:::Moodscale(stats::rnorm(5), stats::rnorm(7))
  nn <- 12
  expect_equal(m$mean, 5 * (nn^2 - 1) / 12)
  expect_equal(m$var, 5 * 7 * (nn + 1) * (nn^2 - 4) / 180)
  # the specialised variance agrees with the general linear-rank one
  expect_equal(m$var, m$var_general, tolerance = 1e-8)
  # a scale difference is detected; equal scales are not
  expect_lt(rmorie:::Moodscale(stats::rnorm(60),
                               stats::rnorm(60, sd = 4))$p_value, 0.01)
  expect_gt(rmorie:::Moodscale(stats::rnorm(60),
                               stats::rnorm(60))$p_value, 0.01)
  expect_error(rmorie:::Moodscale(numeric(0), 1), "non-empty")

  # the moments in their own right
  for (mm in c(3L, 10L)) {
    for (nnn in c(4L, 15L)) {
      mo <- rmorie:::Moodmom(mm, nnn)
      N <- mm + nnn
      expect_equal(mo$mean, mm * (N^2 - 1) / 12)
      expect_equal(mo$var, mm * nnn * (N + 1) * (N^2 - 4) / 180)
      expect_equal(mo$sd, sqrt(mo$var))
      expect_equal(mo$N, N)
    }
  }
  expect_error(rmorie:::Moodmom(0L, 3L), "at least 1")
})

test_that("the Ansari-Bradley statistic is ansari.test's, shifted", {
  set.seed(83)
  x <- stats::rnorm(9)
  y <- stats::rnorm(11, sd = 2)
  got <- rmorie:::Ansbrad(x, y)
  nn <- 20
  # Gibbons scores the distance FROM the centre, |i - (N+1)/2|, while
  # ansari.test scores the distance from the nearer END,
  # min(i, N+1-i). The two differ by a constant, so the statistics are
  # related by AB = m(N+1)/2 - stat and the tests are equivalent.
  expect_equal(got$scores, abs(seq_len(nn) - (nn + 1) / 2))
  ab <- as.numeric(suppressWarnings(stats::ansari.test(x, y)$statistic))
  expect_equal(ab, 9 * (nn + 1) / 2 - got$statistic)
  # the two score sets sum to the same constant at every position
  expect_equal(got$scores + pmin(seq_len(nn), nn + 1 - seq_len(nn)),
               rep((nn + 1) / 2, nn))

  # the moments come from the general linear-rank formulas
  mv <- rmorie:::.gbLrMoments(got$scores, 9L, 11L)
  expect_equal(got$mean, mv$mean)
  expect_equal(got$var, mv$var)
  expect_equal(got$z, (got$statistic - got$mean) / sqrt(got$var))
  expect_equal(got$p_value, 2 * (1 - stats::pnorm(abs(got$z))))
  # and the normal approximation agrees with ansari.test's own
  expect_equal(got$p_value,
               suppressWarnings(stats::ansari.test(x, y,
                 exact = FALSE)$p.value), tolerance = 1e-8)
  # a large scale difference is detected
  expect_lt(rmorie:::Ansbrad(stats::rnorm(60),
                             stats::rnorm(60, sd = 4))$p_value, 0.01)
  expect_error(rmorie:::Ansbrad(1, numeric(0)), "non-empty")
})

test_that("Siegel-Tukey assigns its scores from both ends inward", {
  x <- c(1, 3, 5, 7)
  y <- c(2, 4, 6, 8)
  s <- rmorie:::Sgltukey(x, y)
  # the scores are a permutation of 1..N
  expect_equal(sort(s$scores), as.numeric(seq_len(8)))
  # the pattern is 1 at the low end, 2 and 3 at the high end, 4 and 5
  # back at the low end, and so on -- so the EXTREMES get the low scores
  expect_equal(s$scores[1], 1)
  expect_equal(s$scores[8], 2)
  expect_equal(s$scores[7], 3)
  expect_equal(s$scores[2], 4)
  # and the middle gets the high ones, which is what makes a
  # concentrated sample score high
  expect_true(max(s$scores) %in% s$scores[4:5])
  expect_equal(s$dropped, 0L)
  expect_equal(c(s$m, s$n), c(4L, 4L))
  # the moments and standardisation
  mv <- rmorie:::.gbLrMoments(s$scores, 4L, 4L)
  expect_equal(s$mean, mv$mean)
  expect_equal(s$var, mv$var)
  expect_equal(s$z, (s$statistic - s$mean) / sqrt(s$var))

  # an ODD pooled size drops the middle observation, since the scores
  # must pair from both ends
  odd <- rmorie:::Sgltukey(c(1, 3, 5), c(2, 4))
  expect_equal(odd$dropped, 1L)
  expect_equal(odd$m + odd$n, 4L)
  expect_equal(sort(odd$scores), as.numeric(1:4))
  # a difference in scale is detected
  set.seed(84)
  expect_lt(rmorie:::Sgltukey(stats::rnorm(50),
                              stats::rnorm(50, sd = 5))$p_value, 0.05)
  expect_error(rmorie:::Sgltukey(numeric(0), 1), "non-empty")
  # dropping the middle must not empty a sample
  expect_error(rmorie:::Sgltukey(5, c(1, 9)), "emptied a sample")
})

test_that("the Klotz test uses squared normal scores", {
  set.seed(85)
  x <- stats::rnorm(8)
  y <- stats::rnorm(10, sd = 2)
  k <- rmorie:::Klotzsc(x, y)
  nn <- 18
  # the scores are the squared normal quantiles at the plotting
  # positions, so they are large at BOTH tails -- which is what makes
  # the statistic sensitive to spread rather than location
  expect_equal(k$scores, stats::qnorm(seq_len(nn) / (nn + 1))^2)
  expect_gt(k$scores[1], k$scores[nn %/% 2])
  expect_gt(k$scores[nn], k$scores[nn %/% 2])
  expect_true(all(k$scores >= 0))
  # symmetric about the centre
  expect_equal(k$scores, rev(k$scores), tolerance = 1e-9)
  # the moments and standardisation
  mv <- rmorie:::.gbLrMoments(k$scores, 8L, 10L)
  expect_equal(k$mean, mv$mean)
  expect_equal(k$var, mv$var)
  expect_equal(k$z, (k$statistic - k$mean) / sqrt(k$var))
  expect_equal(k$p_value, 2 * (1 - stats::pnorm(abs(k$z))))
  # a real scale difference is detected
  expect_lt(rmorie:::Klotzsc(stats::rnorm(60),
                             stats::rnorm(60, sd = 4))$p_value, 0.01)
  # and equal scales are not
  expect_gt(rmorie:::Klotzsc(stats::rnorm(60), stats::rnorm(60))$p_value,
            0.01)
  expect_error(rmorie:::Klotzsc(numeric(0), 1), "non-empty")
})

test_that("the percentile-modified scale test weights both tails", {
  set.seed(86)
  x <- stats::rnorm(10)
  y <- stats::rnorm(10, sd = 2)
  p <- rmorie:::Pctranksc(x, y, s = 0.5)
  # the statistic is the sum of the two tail contributions, which is
  # what makes it a SCALE test: a sample spread into both tails scores
  # high, a concentrated one scores low
  expect_equal(p$statistic, p$tupper + p$blower)
  expect_gte(p$tupper, 0)
  expect_gte(p$blower, 0)
  expect_equal(p$m, 10L)
  expect_equal(p$n, 10L)
  # the tail counts follow from the fractions
  expect_equal(p$S, min(as.integer(floor(20 * 0.5)) + 1L, 20L))
  expect_equal(p$R, p$S)
  # r defaults to s
  expect_equal(rmorie:::Pctranksc(x, y, s = 0.3)$R,
               rmorie:::Pctranksc(x, y, s = 0.3)$S)
  # a separate lower fraction is honoured
  asym <- rmorie:::Pctranksc(x, y, s = 0.3, r = 0.6)
  expect_false(asym$S == asym$R)
  expect_equal(asym$R, min(as.integer(floor(20 * 0.6)) + 1L, 20L))

  # the standardisation
  expect_equal(p$z, (p$statistic - p$mean) / sqrt(p$var))
  expect_equal(p$p_value, 2 * (1 - stats::pnorm(abs(p$z))))
  expect_gt(p$var, 0)

  # THE BOOK'S CLOSED FORMS are reported only when they apply -- an even
  # pooled size with equal tail fractions -- and must then agree with
  # the general linear-rank moments computed from the scores
  expect_false(is.na(p$mean_book))
  expect_equal(p$mean_book, 10 * p$S^2 / 20)
  expect_equal(p$var_book,
               10 * 10 * p$S * (4 * 20 * p$S^2 - 20 - 6 * p$S^3) /
                 (6 * 20^2 * 19))
  # an odd pooled size has no such closed form on offer
  odd <- rmorie:::Pctranksc(stats::rnorm(9), stats::rnorm(10))
  expect_true(is.na(odd$mean_book))
  expect_true(is.na(odd$var_book))
  # nor do unequal tail fractions
  expect_true(is.na(asym$mean_book))

  # a bigger fraction takes in more of each tail
  expect_gt(rmorie:::Pctranksc(x, y, s = 1)$S,
            rmorie:::Pctranksc(x, y, s = 0.2)$S)
  # and s = 1 uses the whole sample
  expect_equal(rmorie:::Pctranksc(x, y, s = 1)$S, 20L)

  expect_error(rmorie:::Pctranksc(x, y, s = 0), "\\(0, 1\\]")
  expect_error(rmorie:::Pctranksc(x, y, s = 1.5), "\\(0, 1\\]")
  expect_error(rmorie:::Pctranksc(x, y, r = 0), "\\(0, 1\\]")
  expect_error(rmorie:::Pctranksc(numeric(0), 1), "non-empty")
})

test_that("all five scale tests agree on a large scale difference", {
  set.seed(87)
  # a sample four times as spread should be flagged by every one of them
  x <- stats::rnorm(80)
  y <- stats::rnorm(80, sd = 4)
  for (f in list(rmorie:::Moodscale, rmorie:::Ansbrad,
                 rmorie:::Sgltukey, rmorie:::Klotzsc)) {
    expect_lt(f(x, y)$p_value, 0.01)
  }
  # and none of them should flag two samples of equal spread
  a <- stats::rnorm(80)
  b <- stats::rnorm(80)
  for (f in list(rmorie:::Moodscale, rmorie:::Ansbrad,
                 rmorie:::Sgltukey, rmorie:::Klotzsc)) {
    expect_gt(f(a, b)$p_value, 0.01)
  }
  # a LOCATION shift with equal spread is not a scale difference, which
  # is what distinguishes these from the rank-sum tests
  c2 <- stats::rnorm(80, mean = 3)
  expect_gt(rmorie:::Moodscale(a, c2)$p_value, 0.001)
})
