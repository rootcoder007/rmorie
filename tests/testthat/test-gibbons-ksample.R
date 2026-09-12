# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 10: the Kruskal-Wallis test with its tie correction and multiple
# comparisons, and the Jonckheere-Terpstra test for ordered
# alternatives.
#
# Anchors outside the module: stats::kruskal.test for H and its
# p-value, the algebraic identity between the defining and computing
# forms of H, the JT null moments written out from their closed forms,
# and the fact that JT counts exactly the concordant pairs across every
# ordered pair of samples.

test_that("H matches stats::kruskal.test, ties and all", {
  set.seed(51)
  for (i in 1:5) {
    g <- list(stats::rnorm(7), stats::rnorm(9, i / 4), stats::rnorm(8, -i / 4))
    got <- rmorie:::Kwh(g)
    want <- stats::kruskal.test(g)
    expect_equal(got$statistic, as.numeric(want$statistic))
    expect_equal(got$df, as.integer(want$parameter))
    expect_equal(got$p_value, want$p.value)
  }
  # tied observations: kruskal.test applies the same correction
  tied <- list(c(1, 2, 2, 3), c(2, 3, 3, 4), c(1, 1, 4, 5))
  expect_equal(rmorie:::Kwh(tied)$statistic,
               as.numeric(stats::kruskal.test(tied)$statistic))
  kt <- rmorie:::Kwh(tied)
  v <- unlist(tied)
  tb <- as.numeric(table(v))
  tb <- tb[tb > 1]
  expect_equal(kt$correction,
               1 - sum(tb * (tb^2 - 1)) / (12 * (12^2 - 1)))
  expect_lt(kt$correction, 1)
  # the correction inflates H, since it divides by something below one
  expect_gt(kt$statistic, kt$h_raw)
  # switching it off returns the raw statistic
  expect_equal(rmorie:::Kwh(tied, correct = FALSE)$statistic, kt$h_raw)
  expect_equal(rmorie:::Kwh(tied, correct = FALSE)$correction, 1)
  # untied data needs no correction
  untied <- list(c(1, 2, 3), c(4, 5, 6))
  expect_equal(rmorie:::Kwh(untied)$correction, 1)
  expect_equal(rmorie:::Kwh(untied)$statistic, rmorie:::Kwh(untied)$h_raw)

  # the rank sums and means are reported, and account for every rank
  g <- list(c(1, 3, 5), c(2, 4, 6))
  w <- rmorie:::Kwh(g)
  expect_equal(sum(w$rank_sums), 6 * 7 / 2)
  expect_equal(w$rank_sums, c(1 + 3 + 5, 2 + 4 + 6))
  expect_equal(w$rank_means, w$rank_sums / c(3, 3))
  expect_equal(w$k, 2L)
  expect_equal(w$n, 6L)
  # perfectly separated groups give the largest attainable H
  sep <- rmorie:::Kwh(list(c(1, 2, 3), c(10, 11, 12)))
  expect_gt(sep$statistic, w$statistic)
  # with two groups H is the square of the rank-sum z, so it agrees with
  # the Wilcoxon test on the same data
  expect_equal(sep$p_value,
               stats::kruskal.test(list(c(1, 2, 3), c(10, 11, 12)))$p.value)

  expect_error(rmorie:::Kwh(list(1:3)), "at least 2 samples")
  expect_error(rmorie:::Kwh(list(1:3, numeric(0))), "must be non-empty")
})

test_that("the two forms of H are algebraically the same", {
  # the defining form sums squared departures of each rank sum from its
  # null expectation; the computing form is the rearrangement used in
  # practice. They must agree exactly.
  for (ns in list(c(3L, 4L, 5L), c(6L, 6L), c(2L, 3L, 4L, 5L))) {
    nn <- sum(ns)
    set.seed(52)
    # any valid set of rank sums, taken from an actual ranking
    v <- stats::rnorm(nn)
    grp <- rep(seq_along(ns), ns)
    rk <- rank(v)
    rs <- vapply(seq_along(ns), function(i) sum(rk[grp == i]), 0)
    a <- rmorie:::Kwalt(rs, ns)
    expect_equal(a$statistic, a$h_computing)
    expect_equal(a$resid, 0)
    expect_equal(a$df, length(ns) - 1L)
    expect_equal(a$n, nn)
    # and both agree with the statistic computed from the data
    expect_equal(a$statistic,
                 rmorie:::Kwh(split(v, grp), correct = FALSE)$h_raw)
  }
  # rank sums at their null expectation give H = 0. For n_i = 2 and
  # N = 4 that expectation is n_i (N + 1) / 2 = 5 each, and the two must
  # sum to the total rank sum N(N + 1)/2 = 10.
  expect_equal(rmorie:::Kwalt(c(5, 5), c(2L, 2L))$statistic, 0)
  expect_equal(rmorie:::Kwalt(c(5, 5), c(2L, 2L))$h_computing, 0)
  # and three equal groups likewise
  expect_equal(rmorie:::Kwalt(c(15, 15, 15), c(3L, 3L, 3L))$statistic, 0)
  expect_error(rmorie:::Kwalt(c(1), c(1L)), "at least 2 samples")
  expect_error(rmorie:::Kwalt(c(1, 2), c(1L)), "matching sizes")
  expect_error(rmorie:::Kwalt(c(1, 2), c(0L, 1L)), "at least 1")
})

test_that("the chi-square approximation flags the small-sample case", {
  for (h in c(0.5, 2, 6, 15)) {
    for (k in c(2L, 3L, 5L)) {
      c1 <- rmorie:::Kwchi(h, k)
      expect_equal(c1$df, k - 1L)
      expect_equal(c1$p_value, stats::pchisq(h, k - 1L, lower.tail = FALSE))
      expect_true(c1$p_value >= 0 && c1$p_value <= 1)
    }
  }
  # three small samples are where the chi-square approximation is poor
  # enough that the book directs you to its exact table
  expect_equal(rmorie:::Kwchi(5, 3L, ns = c(3L, 4L, 5L))$table_k, 1L)
  expect_equal(rmorie:::Kwchi(5, 3L, ns = c(3L, 4L, 9L))$table_k, 0L)
  expect_equal(rmorie:::Kwchi(5, 4L, ns = c(3L, 3L, 3L, 3L))$table_k, 0L)
  expect_equal(rmorie:::Kwchi(5, 3L)$table_k, 0L)
  # a larger statistic is more significant
  expect_lt(rmorie:::Kwchi(15, 3L)$p_value, rmorie:::Kwchi(2, 3L)$p_value)
  expect_error(rmorie:::Kwchi(1, 1L), "at least 2")
})

test_that("multiple comparisons use the Bonferroni-adjusted normal point", {
  rm <- c(5, 10, 18)
  ns <- c(4L, 4L, 4L)
  mc <- rmorie:::Kwmc(rm, ns, alpha = 0.20)
  nn <- 12
  # the critical point splits alpha over the k(k-1) ordered pairs
  expect_equal(mc$zstar, stats::qnorm(1 - 0.20 / (3 * 2)))
  # the per-pair bound
  for (i in 1:3) for (j in 1:3) {
    expect_equal(mc$bounds[i, j],
                 mc$zstar * sqrt(nn * (nn + 1) / 12 *
                                   (1 / ns[i] + 1 / ns[j])))
    expect_equal(mc$diffs[i, j], abs(rm[i] - rm[j]))
  }
  # with equal sample sizes there is one common bound
  expect_equal(mc$bound, mc$zstar * sqrt(3 * (nn + 1) / 6))
  expect_equal(mc$bound, mc$bounds[1, 2])
  # the significant pairs are 0-based and only the upper triangle
  expect_true(all(vapply(mc$significant, function(p) p[1] < p[2], TRUE)))
  flagged <- vapply(mc$significant, function(p) paste(p, collapse = "-"),
                    character(1))
  # groups 1 and 3 differ by 13, which clears the bound
  expect_true("0-2" %in% flagged)
  # a stricter alpha flags no more pairs
  expect_lte(length(rmorie:::Kwmc(rm, ns, alpha = 0.001)$significant),
             length(mc$significant))
  # unequal sizes have no single bound to report
  expect_true(is.nan(rmorie:::Kwmc(rm, c(3L, 4L, 5L))$bound))
  expect_false(is.nan(rmorie:::Kwmc(rm, c(3L, 4L, 5L))$bounds[1, 2]))
  # identical rank means differ nowhere
  expect_length(rmorie:::Kwmc(c(5, 5, 5), ns)$significant, 0L)

  expect_error(rmorie:::Kwmc(c(1), c(1L)), "at least 2 samples")
  expect_error(rmorie:::Kwmc(c(1, 2), c(1L)), "matching sizes")
  expect_error(rmorie:::Kwmc(rm, ns, alpha = 0), "strictly inside")
})

test_that("Jonckheere-Terpstra counts the concordant pairs", {
  # three samples in increasing order: every cross-sample pair is
  # concordant, so B attains its maximum
  g <- list(c(1, 2), c(3, 4), c(5, 6))
  j <- rmorie:::Jtstat(g)
  # 2*2 + 2*2 + 2*2 = 12 ordered pairs, all concordant
  expect_equal(j$statistic, 12)
  # the reverse order gives none
  expect_equal(rmorie:::Jtstat(list(c(5, 6), c(3, 4), c(1, 2)))$statistic, 0)
  # the null moments
  ns <- c(2, 2, 2)
  nn <- 6
  expect_equal(j$mean, (nn^2 - sum(ns^2)) / 4)
  expect_equal(j$var, (nn^2 * (2 * nn + 3) - sum(ns^2 * (2 * ns + 3))) / 72)
  expect_equal(j$z, (j$statistic - j$mean) / sqrt(j$var))
  # an increasing trend is evidence for the ordered alternative
  expect_lt(j$p_value, 0.05)
  expect_gt(rmorie:::Jtstat(list(c(5, 6), c(3, 4), c(1, 2)))$p_value, 0.9)
  # the alternatives
  expect_equal(rmorie:::Jtstat(g, "less")$p_value, stats::pnorm(j$z))
  expect_equal(rmorie:::Jtstat(g, "two-sided")$p_value,
               min(1, 2 * (1 - stats::pnorm(abs(j$z)))))

  # exact ties count a half each, which keeps B symmetric under
  # reversal about its maximum
  t1 <- rmorie:::Jtstat(list(c(1, 1), c(1, 1)))$statistic
  expect_equal(t1, 2)
  # recomputed by hand on a larger case
  set.seed(53)
  gg <- list(stats::rnorm(5), stats::rnorm(6, 1), stats::rnorm(4, 2))
  b <- 0
  for (i in 1:2) for (jj in (i + 1):3) {
    b <- b + sum(outer(gg[[i]], gg[[jj]], "<"))
  }
  expect_equal(rmorie:::Jtstat(gg)$statistic, b)
  expect_equal(rmorie:::Jtstat(gg)$k, 3L)
  expect_equal(rmorie:::Jtstat(gg)$n, 15L)

  expect_error(rmorie:::Jtstat(list(1:3)), "at least 2 samples")
  expect_error(rmorie:::Jtstat(list(1:3, numeric(0))), "non-empty")
  expect_error(rmorie:::Jtstat(g, "sideways"),
               "greater, less or two-sided")
})

test_that("the JT moments and the pairwise matrix agree with the statistic", {
  for (ns in list(c(2L, 3L, 4L), c(5L, 5L), c(2L, 2L, 2L, 2L))) {
    m <- rmorie:::Jtmom(ns)
    nn <- sum(ns)
    expect_equal(m$mean, (nn^2 - sum(ns^2)) / 4)
    expect_equal(m$var, (nn^2 * (2 * nn + 3) - sum(ns^2 * (2 * ns + 3))) / 72)
    expect_equal(m$sd, sqrt(m$var))
    # the mean is half the number of cross-sample pairs, which is what
    # "no trend" means: each pair concordant with probability one half
    pair <- 0
    for (i in seq_len(length(ns) - 1L)) {
      for (j in (i + 1L):length(ns)) pair <- pair + ns[i] * ns[j] / 2
    }
    expect_equal(m$mean_pairwise, pair)
    expect_equal(m$mean, pair)
    expect_equal(m$k, length(ns))
    expect_equal(m$n, nn)
  }

  # the matrix form sums to the statistic
  set.seed(54)
  g <- list(stats::rnorm(4), stats::rnorm(5, 0.5), stats::rnorm(3, 1))
  s <- rmorie:::Jtsum(g)
  expect_equal(s$statistic, rmorie:::Jtstat(g)$statistic)
  expect_equal(s$statistic, sum(s$u[upper.tri(s$u)]))
  expect_equal(s$npairs, 3L)
  # only the upper triangle is filled, since the pairs are ordered
  expect_true(all(s$u[lower.tri(s$u, diag = TRUE)] == 0))
  expect_equal(dim(s$u), c(3L, 3L))
  # each entry is a Mann-Whitney count between that ordered pair
  expect_equal(s$u[1, 2], sum(outer(g[[1]], g[[2]], "<")))
  expect_equal(s$u[1, 3], sum(outer(g[[1]], g[[3]], "<")))
  # and is bounded by the product of the two sizes
  expect_lte(s$u[1, 2], 4 * 5)
  expect_equal(s$n, 12L)

  expect_error(rmorie:::Jtmom(c(3L)), "at least 2 samples")
  expect_error(rmorie:::Jtmom(c(0L, 2L)), "at least 1")
  expect_error(rmorie:::Jtsum(list(1:3)), "at least 2 samples")
})
