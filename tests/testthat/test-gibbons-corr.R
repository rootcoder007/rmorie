# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 11-12: Kendall's tau and its exact null distribution, the tau
# trend test, Spearman's rho with its shortcut form, the tests of zero
# correlation, the normal-scores correlation, and Kendall's partial tau.
#
# Anchors outside the module: an exhaustive enumeration of the inversion
# counts over all n! permutations for the exact tau null distribution,
# stats::cor for Kendall's tau and Spearman's rho, stats::pt and
# stats::pnorm for the tests, and the 2-by-2 concordance table
# recomputed by hand for the partial tau.

test_that("the exact null distribution of S is the inversion count", {
  # Enumerate every permutation and count its inversions. The number of
  # permutations of n with k inversions (the Mahonian numbers) IS the
  # null distribution of Kendall's S, so this is the definition rather
  # than a restatement of the code.
  all_perms <- function(v) {
    if (length(v) <= 1L) return(list(v))
    out <- list()
    for (i in seq_along(v)) {
      for (p in all_perms(v[-i])) out[[length(out) + 1L]] <- c(v[i], p)
    }
    out
  }
  for (n in 2:7) {
    maxinv <- n * (n - 1L) / 2L
    t <- rmorie:::Taunull(n)
    counts <- integer(maxinv + 1L)
    for (p in all_perms(seq_len(n))) {
      k <- sum(outer(seq_len(n), seq_len(n), "<") & outer(p, p, ">"))
      counts[k + 1L] <- counts[k + 1L] + 1L
    }
    expect_equal(as.numeric(t$pmf), counts / factorial(n))
    expect_equal(sum(t$pmf), 1)
    # S runs from +maxinv (no inversions) down to -maxinv in steps of 2
    expect_equal(t$support, maxinv - 2 * (0:maxinv))
    expect_equal(t$support[1], maxinv)
    expect_equal(t$support[length(t$support)], -maxinv)
    # the distribution is symmetric about zero
    expect_equal(as.numeric(t$pmf), rev(as.numeric(t$pmf)))
    # the asymptotic variance of tau
    expect_equal(t$var_tau, 2 * (2 * n + 5) / (9 * n * (n - 1)))
    expect_equal(t$var_s, t$var_tau * maxinv^2)
  }

  # the point and tail probabilities at a given S
  t <- rmorie:::Taunull(5L)
  maxinv <- 10L
  for (s in seq(-maxinv, maxinv, by = 2L)) {
    q <- rmorie:::Taunull(5L, s = s)
    idx <- (maxinv - s) %/% 2L
    expect_equal(q$pmf_s, t$pmf[idx + 1L])
    expect_equal(q$sf_s, sum(t$pmf[1:(idx + 1L)]))
    expect_equal(q$cdf_s, sum(t$pmf[(idx + 1L):length(t$pmf)]))
  }
  # the extreme value is the single ordered permutation
  expect_equal(rmorie:::Taunull(5L, s = 10L)$pmf_s, 1 / factorial(5))
  # S must lie on the support's lattice
  expect_error(rmorie:::Taunull(5L, s = 1L), "outside the support")
  expect_error(rmorie:::Taunull(5L, s = 12L), "outside the support")
  expect_error(rmorie:::Taunull(1L), "at least 2")
})

test_that("the tau trend test matches Kendall's tau against the index", {
  set.seed(61)
  for (i in 1:5) {
    y <- stats::rnorm(14) + i * seq_len(14) / 14
    got <- rmorie:::Tautrend(y)
    # the trend test is Kendall's tau between the observation and its
    # position, which is what "trend" means here
    expect_equal(got$tau, stats::cor(seq_along(y), y, method = "kendall"))
  }
  # a strictly increasing series has tau = 1 and every pair concordant
  up <- rmorie:::Tautrend(1:8)
  expect_equal(up$tau, 1)
  expect_equal(up$P, 28)
  expect_equal(up$Q, 0)
  expect_equal(up$statistic, 28)
  # and a decreasing one is its mirror
  down <- rmorie:::Tautrend(8:1)
  expect_equal(down$tau, -1)
  expect_equal(down$statistic, -28)
  expect_equal(down$P, 0)
  # P and Q partition the pairs when there are no ties
  expect_equal(up$P + up$Q, 8 * 7 / 2)
  # the variance and standardisation
  expect_equal(up$var, 2 * (2 * 8 + 5) / (9 * 8 * 7))
  expect_equal(up$z, up$tau / sqrt(up$var))
  expect_lt(up$p_value, 0.001)
  # the alternatives
  expect_equal(rmorie:::Tautrend(1:8, "greater")$p_value,
               1 - stats::pnorm(up$z))
  expect_equal(rmorie:::Tautrend(1:8, "less")$p_value, stats::pnorm(up$z))
  # a series with no systematic trend is not significant at any
  # conventional level, though this one leans mildly upward
  expect_gt(rmorie:::Tautrend(c(3, 1, 4, 1, 5, 9, 2, 6))$p_value, 0.05)
  # a series with exactly as many concordant as discordant pairs has
  # tau of precisely zero, and so no evidence whatever
  flat <- rmorie:::Tautrend(c(1, 4, 3, 2))
  expect_equal(flat$P, flat$Q)
  expect_equal(flat$tau, 0)
  expect_equal(flat$z, 0)
  expect_equal(flat$p_value, 1)
  # ties are counted in neither P nor Q, so they no longer partition
  tied <- rmorie:::Tautrend(c(1, 1, 2, 2, 3))
  expect_lt(tied$P + tied$Q, 5 * 4 / 2)

  expect_error(rmorie:::Tautrend(c(1, 2)), "at least 3")
  expect_error(rmorie:::Tautrend(1:5, "sideways"),
               "two-sided, greater or less")
})

test_that("Spearman's rho matches stats::cor and its shortcut", {
  set.seed(62)
  x <- stats::rnorm(20)
  y <- stats::rnorm(20)
  r <- rmorie:::Spearrho(x, y)
  expect_equal(r$statistic, stats::cor(x, y, method = "spearman"))
  expect_equal(r$n, 20L)
  expect_equal(r$tied, 0L)
  # with no ties the shortcut form is exact
  expect_equal(r$r_shortcut, r$statistic)
  d2 <- sum((rank(x) - rank(y))^2)
  expect_equal(r$sumd2, d2)
  expect_equal(r$r_shortcut, 1 - 6 * d2 / (20 * (20^2 - 1)))
  expect_equal(r$var, 1 / 19)

  # perfect agreement and perfect reversal
  expect_equal(rmorie:::Spearrho(1:10, 1:10)$statistic, 1)
  expect_equal(rmorie:::Spearrho(1:10, 10:1)$statistic, -1)
  expect_equal(rmorie:::Spearrho(1:10, 1:10)$sumd2, 0)
  # monotone but non-linear is still 1, which is the point of a rank
  # correlation
  expect_equal(rmorie:::Spearrho(1:10, exp(1:10))$statistic, 1)

  # WITH ties the shortcut form is no longer exact, and the module
  # reports the tie flag so a caller knows which to trust
  tx <- c(1, 2, 2, 3, 4, 5, 5)
  ty <- c(2, 1, 3, 3, 5, 4, 6)
  rt <- rmorie:::Spearrho(tx, ty)
  expect_equal(rt$tied, 1L)
  expect_equal(rt$statistic, stats::cor(tx, ty, method = "spearman"))
  expect_false(isTRUE(all.equal(rt$r_shortcut, rt$statistic)))

  expect_error(rmorie:::Spearrho(1:5, 1:4), "same length")
  expect_error(rmorie:::Spearrho(1:2, 1:2), "at least 3")
})

test_that("the zero-correlation tests are their two approximations", {
  for (n in c(10L, 30L)) {
    for (r in c(-0.6, 0, 0.35, 0.9)) {
      h <- rmorie:::Rhotest(r, n)
      # the normal approximation uses the 1/(n-1) variance
      expect_equal(h$z, r * sqrt(n - 1))
      expect_equal(h$var, 1 / (n - 1))
      expect_equal(h$p_normal, min(1, 2 * (1 - stats::pnorm(abs(h$z)))))
      # the t approximation is the usual correlation t on n - 2 df
      expect_equal(h$t, r * sqrt((n - 2) / (1 - r^2)))
      expect_equal(h$df, n - 2L)
      expect_equal(h$p_value,
                   min(1, 2 * stats::pt(abs(h$t), n - 2,
                                        lower.tail = FALSE)))
      expect_equal(rmorie:::Rhotest(r, n, "greater")$p_value,
                   stats::pt(h$t, n - 2, lower.tail = FALSE))
      expect_equal(rmorie:::Rhotest(r, n, "less")$p_value,
                   stats::pt(h$t, n - 2))
    }
  }
  # zero correlation is no evidence at all
  expect_equal(rmorie:::Rhotest(0, 20L)$p_value, 1)
  expect_equal(rmorie:::Rhotest(0, 20L)$z, 0)
  # a perfect correlation sends the t statistic to infinity rather than
  # dividing by zero
  expect_equal(rmorie:::Rhotest(1, 20L)$t, Inf)
  expect_equal(rmorie:::Rhotest(-1, 20L)$t, -Inf)
  expect_equal(rmorie:::Rhotest(1, 20L)$p_value, 0)
  # the t test agrees with cor.test's own on the ranks
  set.seed(63)
  x <- stats::rnorm(25)
  y <- x + stats::rnorm(25)
  rr <- rmorie:::Spearrho(x, y)$statistic
  expect_equal(rmorie:::Rhotest(rr, 25L)$p_value,
               stats::cor.test(rank(x), rank(y))$p.value, tolerance = 1e-8)
  expect_error(rmorie:::Rhotest(0.5, 2L), "at least 3")
  expect_error(rmorie:::Rhotest(1.5, 10L), "\\[-1, 1\\]")
  expect_error(rmorie:::Rhotest(0.5, 10L, "sideways"),
               "two-sided, greater or less")
})

test_that("the normal-scores correlation uses the expected order statistics", {
  set.seed(64)
  n <- 12
  x <- stats::rnorm(n)
  y <- x + stats::rnorm(n, sd = 0.3)
  nc <- rmorie:::Normcorr(x, y, nodes = 801)
  expect_equal(nc$n, n)
  expect_length(nc$scores, n)
  # the scores are the expected standard-normal order statistics: they
  # sum to zero by symmetry and increase
  expect_equal(sum(nc$scores), 0, tolerance = 1e-6)
  expect_true(all(diff(nc$scores) > 0))
  # and they bracket roughly the right range for n = 12
  expect_lt(nc$scores[1], -1)
  expect_gt(nc$scores[n], 1)
  # the statistic is a correlation
  expect_true(nc$statistic >= -1 && nc$statistic <= 1)
  # strongly associated data gives a high value
  expect_gt(nc$statistic, 0.7)
  # the Fisher transform and its moments
  expect_equal(nc$zf, atanh(nc$statistic))
  expect_equal(nc$var_zf, 1 / (n - 3))
  expect_equal(nc$mean_zf, atanh(0) * (1 - 0.6 / (n + 8)))
  expect_equal(nc$z, (nc$zf - nc$mean_zf) / sqrt(nc$var_zf))
  expect_equal(nc$p_value, 2 * (1 - stats::pnorm(abs(nc$z))))

  # perfectly monotone data attains the maximum
  perfect <- rmorie:::Normcorr(1:10, 1:10, nodes = 801)
  expect_equal(perfect$statistic, 1, tolerance = 1e-6)
  expect_lt(perfect$p_value, 0.01)
  # and perfectly reversed data the minimum
  expect_equal(rmorie:::Normcorr(1:10, 10:1, nodes = 801)$statistic, -1,
               tolerance = 1e-6)
  # a non-zero null shifts the reference point
  expect_lt(rmorie:::Normcorr(x, y, rho = 0.9, nodes = 801)$mean_zf,
            atanh(0.9))
  expect_error(rmorie:::Normcorr(1:5, 1:4), "same length")
  expect_error(rmorie:::Normcorr(1:3, 1:3), "at least 4")
})

test_that("the partial tau is built from the concordance table", {
  set.seed(65)
  n <- 15
  z <- stats::rnorm(n)
  x <- z + stats::rnorm(n, sd = 0.4)
  y <- z + stats::rnorm(n, sd = 0.4)
  p <- rmorie:::Taupartial(x, y, z)

  # the four cells count the pairs by whether x and y each agree with z
  expect_equal(p$x11 + p$x12 + p$x21 + p$x22 + p$dropped, p$npairs)
  expect_equal(p$npairs, as.integer(n * (n - 1) / 2))
  expect_equal(p$dropped, 0L)      # continuous data has no tied pair
  # recomputed by hand
  x11 <- x12 <- x21 <- x22 <- 0L
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    sx <- sign(x[j] - x[i]); sy <- sign(y[j] - y[i]); sz <- sign(z[j] - z[i])
    xc <- sx * sz > 0; yc <- sy * sz > 0
    if (yc && xc) x11 <- x11 + 1L else if (yc && !xc) x12 <- x12 + 1L
    else if (!yc && xc) x21 <- x21 + 1L else x22 <- x22 + 1L
  }
  expect_equal(c(p$x11, p$x12, p$x21, p$x22), c(x11, x12, x21, x22))
  # the statistic is the table's phi coefficient
  den <- (x11 + x21) * (x12 + x22) * (x11 + x12) * (x21 + x22)
  expect_equal(p$statistic, (x11 * x22 - x12 * x21) / sqrt(den))
  expect_true(p$statistic >= -1 && p$statistic <= 1)

  # x and y related ONLY through z: once z is partialled out there is
  # little left, so the partial tau is far below their raw association
  expect_lt(abs(p$statistic),
            abs(stats::cor(x, y, method = "kendall")))

  # tied triples are dropped rather than counted arbitrarily
  tied <- rmorie:::Taupartial(c(1, 1, 2, 3), c(1, 2, 3, 4), c(1, 2, 3, 4))
  expect_gt(tied$dropped, 0L)
  expect_equal(tied$x11 + tied$x12 + tied$x21 + tied$x22 + tied$dropped,
               tied$npairs)
  # a degenerate table has no phi coefficient to report
  deg <- rmorie:::Taupartial(1:5, 1:5, 1:5)
  expect_true(is.nan(deg$statistic))
  expect_equal(deg$x11, 10L)

  expect_error(rmorie:::Taupartial(1:5, 1:4, 1:5), "same length")
  expect_error(rmorie:::Taupartial(1:2, 1:2, 1:2), "at least 3")
})
