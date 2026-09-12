# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 12: Friedman's two-way analysis of variance by ranks with its tie
# correction, moments and multiple comparisons, and Page's L test for
# ordered alternatives with its exact null distribution.
#
# Anchors outside the module: stats::friedman.test for Q and its
# p-value, tied data included -- the two derive the tie correction
# differently and must still agree; an exhaustive enumeration of the
# within-block permutations for Page's exact distribution; and the
# closed-form moments written out by hand.
#
# The layout: `data` is a list of BLOCKS, each holding one observation
# per treatment. So k is the number of blocks and n the number of
# treatments, which is the transpose of how friedman.test takes its
# matrix.

test_that("Friedman's Q matches stats::friedman.test", {
  set.seed(71)
  for (i in 1:5) {
    k <- 8L    # blocks
    n <- 4L    # treatments
    m <- matrix(stats::rnorm(k * n), nrow = k, ncol = n)
    m[, 2] <- m[, 2] + i / 4      # one treatment shifted
    blocks <- split(m, row(m))
    got <- rmorie:::Friedq(blocks)
    want <- stats::friedman.test(m)
    expect_equal(got$statistic, as.numeric(want$statistic))
    expect_equal(got$df, as.integer(want$parameter))
    expect_equal(got$p_value, want$p.value)
  }

  # TIED data: the module divides by k n (n^2 - 1) - sum(u^3 - u) while
  # friedman.test divides by n_blocks k (k+1) - sum(u^3 - u)/(k-1) in its
  # own notation. Those are the same expression rearranged, so they must
  # agree -- and this is the check that they do.
  tied <- matrix(c(1, 1, 2, 3,
                   2, 2, 2, 1,
                   3, 1, 1, 2,
                   1, 2, 3, 3,
                   2, 2, 1, 1), nrow = 5, byrow = TRUE)
  gt <- rmorie:::Friedq(split(tied, row(tied)))
  expect_equal(gt$statistic,
               as.numeric(stats::friedman.test(tied)$statistic))
  expect_equal(gt$p_value, stats::friedman.test(tied)$p.value)
  # the correction moves the statistic off the uncorrected one
  expect_false(isTRUE(all.equal(gt$statistic, gt$q_raw)))
  # and switching it off returns the uncorrected form
  expect_equal(rmorie:::Friedq(split(tied, row(tied)),
                               correct = FALSE)$statistic, gt$q_raw)

  # the rank sums account for every rank in every block
  b <- list(c(1, 2, 3), c(3, 1, 2), c(2, 3, 1))
  f <- rmorie:::Friedq(b)
  expect_equal(sum(f$rank_sums), 3 * (3 * 4 / 2))
  expect_equal(f$k, 3L)
  expect_equal(f$n, 3L)
  # all blocks ranking identically is the largest attainable Q
  same <- rmorie:::Friedq(list(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3)))
  expect_equal(same$rank_sums, c(3, 6, 9))
  expect_gt(same$statistic, f$statistic)
  expect_equal(same$statistic, as.numeric(stats::friedman.test(
    matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE))$statistic))

  expect_error(rmorie:::Friedq(list(c(1, 2))), "at least 2 blocks")
  expect_error(rmorie:::Friedq(list(1, 2)), "at least 2 treatments")
  expect_error(rmorie:::Friedq(list(c(1, 2, 3), c(1, 2))),
               "n observations")
})

test_that("the tie correction is reported in its own right", {
  tied <- list(c(1, 1, 2), c(2, 2, 1), c(1, 2, 2))
  t <- rmorie:::Friedties(tied)
  fr <- rmorie:::Friedq(tied)
  expect_equal(t$statistic, fr$statistic)
  expect_equal(t$rank_sums, fr$rank_sums)
  expect_equal(t$s, fr$s)
  # the tie sum is sum over tie groups of u^3 - u
  tiesum <- 0
  for (r in tied) {
    tb <- as.numeric(table(r))
    tb <- tb[tb > 1]
    if (length(tb)) tiesum <- tiesum + sum(tb * (tb^2 - 1))
  }
  expect_equal(t$tiesum, tiesum)
  expect_gt(t$tiesum, 0)
  # the correction inflates the statistic, so the factor exceeds one
  expect_gt(t$factor, 1)
  expect_equal(t$factor, t$statistic / t$q_raw)
  # untied data needs no correction
  clean <- list(c(1, 2, 3), c(3, 1, 2))
  expect_equal(rmorie:::Friedties(clean)$tiesum, 0)
  expect_equal(rmorie:::Friedties(clean)$factor, 1)
  expect_equal(rmorie:::Friedties(clean)$statistic,
               rmorie:::Friedties(clean)$q_raw)
  expect_error(rmorie:::Friedties(list(c(1, 2))), "at least 2 blocks")
})

test_that("the chi-square approximation reports its own deficiency", {
  for (k in c(3L, 10L, 40L)) {
    for (n in c(3L, 5L)) {
      c1 <- rmorie:::Friedchi(6, k, n)
      expect_equal(c1$df, n - 1L)
      expect_equal(c1$p_value, stats::pchisq(6, n - 1L, lower.tail = FALSE))
      expect_equal(c1$mean, n - 1)
      # Q's exact variance falls short of the chi-square's by a factor
      # (k-1)/k, so the approximation is conservative for small k
      expect_equal(c1$var_exact, 2 * (n - 1) * (k - 1) / k)
      expect_equal(c1$var_chi2, 2 * (n - 1))
      expect_equal(c1$ratio, (k - 1) / k)
      expect_lt(c1$ratio, 1)
    }
  }
  # the shortfall vanishes as the number of blocks grows
  expect_gt(rmorie:::Friedchi(6, 100L, 4L)$ratio,
            rmorie:::Friedchi(6, 3L, 4L)$ratio)
  expect_equal(rmorie:::Friedchi(6, 1000L, 4L)$ratio, 0.999)

  # the moments in their own right
  for (k in c(4L, 12L)) {
    for (n in c(3L, 6L)) {
      v <- rmorie:::Friedvar(k, n)
      expect_equal(v$mean_s, k * n * (n^2 - 1) / 12)
      expect_equal(v$var_s, n^2 * k * (k - 1) * (n + 1)^2 / 72)
      expect_equal(v$mean_q, n - 1)
      expect_equal(v$var_q, 2 * (n - 1) * (k - 1) / k)
      expect_equal(v$var_chi2, 2 * (n - 1))
      expect_equal(v$deficit, v$var_chi2 - v$var_q)
      expect_gt(v$deficit, 0)
    }
  }
  expect_error(rmorie:::Friedchi(1, 1L, 3L), "k >= 2")
  expect_error(rmorie:::Friedvar(3L, 1L), "n >= 2")
})

test_that("Friedman multiple comparisons use one common bound", {
  rs <- c(6, 12, 24)
  mc <- rmorie:::Friedmc(rs, k = 8L, alpha = 0.20)
  # the critical point splits alpha over the n(n-1) ordered pairs
  expect_equal(mc$zstar, stats::qnorm(1 - 0.20 / (3 * 2)))
  # a balanced design gives every pair the same bound
  expect_equal(mc$bound, mc$zstar * sqrt(8 * 3 * 4 / 6))
  for (i in 1:3) for (j in 1:3) {
    expect_equal(mc$diffs[i, j], abs(rs[i] - rs[j]))
  }
  # treatments 1 and 3 differ by 18
  flagged <- vapply(mc$significant, function(p) paste(p, collapse = "-"),
                    character(1))
  expect_true("0-2" %in% flagged)
  expect_true(all(vapply(mc$significant, function(p) p[1] < p[2], TRUE)))
  # a stricter alpha flags no more pairs, and a wider bound
  strict <- rmorie:::Friedmc(rs, 8L, alpha = 0.001)
  expect_gt(strict$bound, mc$bound)
  expect_lte(length(strict$significant), length(mc$significant))
  # identical rank sums differ nowhere
  expect_length(rmorie:::Friedmc(c(10, 10, 10), 8L)$significant, 0L)
  # more blocks means a wider bound on the rank-sum scale
  expect_gt(rmorie:::Friedmc(rs, 32L)$bound, mc$bound)
  expect_error(rmorie:::Friedmc(c(1), 8L), "at least 2 treatments")
  expect_error(rmorie:::Friedmc(rs, 1L), "at least 2 blocks")
  expect_error(rmorie:::Friedmc(rs, 8L, alpha = 1), "strictly inside")
})

test_that("Page's L weights the rank sums by treatment order", {
  # every block ranking in the same increasing order is the maximum L
  b <- list(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3))
  p <- rmorie:::Pagel(b)
  expect_equal(p$rank_sums, c(3, 6, 9))
  expect_equal(p$statistic, 1 * 3 + 2 * 6 + 3 * 9)
  # the reverse order is the minimum
  rev_b <- list(c(3, 2, 1), c(3, 2, 1), c(3, 2, 1))
  expect_equal(rmorie:::Pagel(rev_b)$statistic, 1 * 9 + 2 * 6 + 3 * 3)
  expect_lt(rmorie:::Pagel(rev_b)$statistic, p$statistic)
  # an increasing trend is evidence for the ordered alternative
  expect_lt(p$p_value, 0.05)
  expect_gt(rmorie:::Pagel(rev_b)$p_value, 0.95)
  # the standardisation
  expect_equal(p$z, (12 * (p$statistic - 0.5) - 3 * 3 * 3 * 16) /
                 (3 * 4 * sqrt(3 * 2)))
  expect_equal(p$p_value, 1 - stats::pnorm(p$z))
  expect_equal(p$k, 3L)
  expect_equal(p$n, 3L)
  # the average rank correlation it implies
  expect_equal(p$rav, 12 * p$statistic / (3 * (27 - 3)) - 3 * 4 / 2)
  # custom weights are honoured
  expect_equal(rmorie:::Pagel(b, weights = c(1, 1, 1))$statistic,
               sum(p$rank_sums))
  expect_error(rmorie:::Pagel(b, weights = c(1, 2)), "length n")
  expect_error(rmorie:::Pagel(list(c(1, 2))), "at least 2 blocks")
})

test_that("Page's exact distribution matches an enumeration", {
  # For one block, L is the weighted rank sum over a single permutation,
  # so its distribution is exactly the enumeration of all n! orderings.
  all_perms <- function(v) {
    if (length(v) <= 1L) return(list(v))
    out <- list()
    for (i in seq_along(v)) {
      for (q in all_perms(v[-i])) out[[length(out) + 1L]] <- c(v[i], q)
    }
    out
  }
  for (n in 2:5) {
    e <- rmorie:::Pageexact(1L, n)
    lo <- sum(seq_len(n) * rev(seq_len(n)))
    hi <- sum(seq_len(n)^2)
    counts <- numeric(hi - lo + 1L)
    for (p in all_perms(seq_len(n))) {
      # rank v of treatment j contributes j * v
      l <- sum(seq_len(n) * p)
      counts[l - lo + 1L] <- counts[l - lo + 1L] + 1
    }
    expect_equal(as.numeric(e$pmf), counts / factorial(n))
    expect_equal(e$support, lo:hi)
    expect_equal(sum(e$pmf), 1)
    # the moments, against the closed form the asymptotic test uses
    a <- rmorie:::Pageasymp(0, 1L, n)
    expect_equal(e$mean, a$mean, tolerance = 1e-9)
    expect_equal(e$var, a$var, tolerance = 1e-9)
  }

  # several blocks: the distribution is the convolution, so its support
  # scales and its total stays one
  e3 <- rmorie:::Pageexact(3L, 3L)
  expect_equal(sum(e3$pmf), 1)
  expect_equal(min(e3$support), 3 * sum(seq_len(3) * rev(seq_len(3))))
  expect_equal(max(e3$support), 3 * sum(seq_len(3)^2))
  expect_equal(e3$mean, rmorie:::Pageasymp(0, 3L, 3L)$mean, tolerance = 1e-9)
  expect_equal(e3$var, rmorie:::Pageasymp(0, 3L, 3L)$var, tolerance = 1e-9)
  # the point and tail probabilities
  top <- max(e3$support)
  q <- rmorie:::Pageexact(3L, 3L, ell = top)
  expect_equal(q$pmf_l, e3$pmf[length(e3$pmf)])
  expect_equal(q$sf_l, q$pmf_l)
  # the maximum L is attained only when every block orders identically
  expect_equal(q$pmf_l, (1 / factorial(3))^3)
  # outside the support the tails saturate rather than erroring
  expect_equal(rmorie:::Pageexact(3L, 3L, ell = top + 10)$sf_l, 0)
  expect_equal(rmorie:::Pageexact(3L, 3L, ell = 0)$sf_l, 1)

  expect_error(rmorie:::Pageexact(0L, 3L), "at least 1")
  expect_error(rmorie:::Pageexact(2L, 9L), "2\\.\\.8")
  expect_error(rmorie:::Pageexact(2L, 1L), "2\\.\\.8")
})

test_that("Page's normal approximation tracks the exact tail", {
  for (k in c(4L, 10L)) {
    for (n in c(3L, 4L)) {
      a <- rmorie:::Pageasymp(100, k, n)
      expect_equal(a$mean, k * n * (n + 1)^2 / 4)
      expect_equal(a$var, k * n^2 * (n + 1)^2 * (n - 1) / 144)
      expect_equal(a$z, (12 * (100 - 0.5) - 3 * k * n * (n + 1)^2) /
                     (n * (n + 1) * sqrt(k * (n - 1))))
      expect_equal(a$p_value, 1 - stats::pnorm(a$z))
      # the continuity correction shifts the statistic toward the null
      expect_lt(a$z, rmorie:::Pageasymp(100, k, n, correct = FALSE)$z)
    }
  }
  # at the null mean there is no evidence of a trend
  k <- 8L
  n <- 4L
  mu <- k * n * (n + 1)^2 / 4
  expect_equal(rmorie:::Pageasymp(mu, k, n, correct = FALSE)$z, 0)
  expect_equal(rmorie:::Pageasymp(mu, k, n, correct = FALSE)$p_value, 0.5)
  # and the approximation is close to the exact tail for a moderate k
  e <- rmorie:::Pageexact(6L, 4L)
  target <- e$support[which(cumsum(rev(e$pmf)) > 0.05)[1]]
  target <- rev(e$support)[which(cumsum(rev(e$pmf)) > 0.05)[1]]
  expect_equal(rmorie:::Pageasymp(target, 6L, 4L)$p_value, 0.05,
               tolerance = 0.4)
  expect_error(rmorie:::Pageasymp(1, 0L, 3L), "at least 1")
  expect_error(rmorie:::Pageasymp(1, 2L, 1L), "at least 2")
})
