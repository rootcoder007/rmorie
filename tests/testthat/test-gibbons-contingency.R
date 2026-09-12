# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 14: chi-square tests of independence and of equal proportions,
# Fisher's exact test, McNemar's test with its exact binomial form and
# confidence interval, multinomial goodness of fit, the linear-by-linear
# trend test, and the odds ratio by Woolf's logit method.
#
# Anchors outside the module: stats::chisq.test, stats::fisher.test,
# stats::mcnemar.test and stats::binom.test, plus the hypergeometric
# point probabilities recomputed with stats::dhyper.

test_that("the independence chi-square matches stats::chisq.test", {
  tb <- rbind(c(12, 7, 9), c(8, 15, 6))
  got <- rmorie:::Chiindep(tb)
  want <- stats::chisq.test(tb, correct = FALSE)
  expect_equal(got$statistic, as.numeric(want$statistic))
  expect_equal(got$df, as.integer(want$parameter))
  expect_equal(got$p_value, want$p.value)
  expect_equal(got$expected, want$expected, ignore_attr = TRUE)
  expect_equal(got$n, sum(tb))
  expect_equal(c(got$r, got$c), c(2L, 3L))
  # the expected counts are the outer product of the margins over n
  expect_equal(got$expected, outer(rowSums(tb), colSums(tb)) / sum(tb),
               ignore_attr = TRUE)

  # Yates's correction applies only to a 2 x 2 table, which is where
  # stats::chisq.test applies it too
  two <- rbind(c(10, 4), c(3, 12))
  expect_equal(rmorie:::Chiindep(two, correct = TRUE)$statistic,
               as.numeric(stats::chisq.test(two, correct = TRUE)$statistic))
  expect_equal(rmorie:::Chiindep(two, correct = FALSE)$statistic,
               as.numeric(stats::chisq.test(two, correct = FALSE)$statistic))
  # asking for it on a larger table leaves the statistic alone
  expect_equal(rmorie:::Chiindep(tb, correct = TRUE)$statistic,
               rmorie:::Chiindep(tb, correct = FALSE)$statistic)
  # the correction can only shrink the statistic
  expect_lt(rmorie:::Chiindep(two, correct = TRUE)$statistic,
            rmorie:::Chiindep(two, correct = FALSE)$statistic)

  # a table already at its expected counts has nothing to explain
  perfect <- rbind(c(10, 20), c(20, 40))
  expect_equal(rmorie:::Chiindep(perfect)$statistic, 0)
  expect_equal(rmorie:::Chiindep(perfect)$p_value, 1)

  expect_error(rmorie:::Chiindep(matrix(1:2, nrow = 1)), "at least 2 rows")
  expect_error(rmorie:::Chiindep(matrix(1:2, ncol = 1)),
               "at least 2 columns")
  expect_error(rmorie:::Chiindep(matrix(0, 2, 2)), "positive counts")
  expect_error(rmorie:::Chiindep(rbind(c(0, 5), c(0, 7))),
               "expected frequency is zero")
})

test_that("the k-by-2 proportions test is the same chi-square", {
  y <- c(12, 8, 15)
  ns <- c(30, 25, 40)
  got <- rmorie:::Chik2(y, ns)
  # the equivalent 2 x k table of successes and failures
  tb <- rbind(y, ns - y)
  expect_equal(got$statistic,
               as.numeric(stats::chisq.test(tb, correct = FALSE)$statistic),
               tolerance = 1e-8)
  expect_equal(got$df, 2L)
  expect_equal(got$p_value, stats::pchisq(got$statistic, 2,
                                          lower.tail = FALSE))
  expect_equal(got$phat, sum(y) / sum(ns))
  expect_equal(got$props, y / ns)
  expect_equal(got$n, sum(ns))

  # groups at identical proportions have nothing to explain
  expect_equal(rmorie:::Chik2(c(10, 20), c(20, 40))$statistic, 0,
               tolerance = 1e-9)
  # a wider spread of proportions gives a larger statistic
  expect_gt(rmorie:::Chik2(c(2, 38), c(40, 40))$statistic, got$statistic)
  expect_error(rmorie:::Chik2(c(1), c(10)), "at least 2 groups")
  expect_error(rmorie:::Chik2(c(1, 2), c(10)), "matching sizes")
  expect_error(rmorie:::Chik2(c(1, 2), c(0, 10)), "must be positive")
  expect_error(rmorie:::Chik2(c(0, 0), c(10, 10)), "inside \\(0, 1\\)")
  expect_error(rmorie:::Chik2(c(10, 10), c(10, 10)), "inside \\(0, 1\\)")
})

test_that("the hypergeometric cell probability is stats::dhyper", {
  # P(a) for a 2 x 2 table with fixed margins is hypergeometric: from r1
  # in the first row and r2 in the second, c1 are drawn
  for (r1 in c(5, 8)) {
    for (r2 in c(4, 9)) {
      for (c1 in c(3, 6)) {
        for (a in 0:min(r1, c1)) {
          expect_equal(rmorie:::.gbHyper(a, r1, r2, c1),
                       stats::dhyper(a, r1, r2, c1))
        }
      }
    }
  }
  # outside the support the probability is zero, not an error
  expect_equal(rmorie:::.gbHyper(-1, 5, 5, 3), 0)
  expect_equal(rmorie:::.gbHyper(99, 5, 5, 3), 0)
  # the support's probabilities sum to one
  r1 <- 6; r2 <- 5; c1 <- 4
  ks <- max(0, c1 - r2):min(r1, c1)
  expect_equal(sum(vapply(ks, function(k) rmorie:::.gbHyper(k, r1, r2, c1),
                          0)), 1)
})

test_that("Fisher's exact test matches stats::fisher.test", {
  tables <- list(rbind(c(3, 1), c(1, 3)),
                 rbind(c(10, 2), c(3, 15)),
                 rbind(c(1, 9), c(11, 3)),
                 rbind(c(5, 5), c(5, 5)))
  for (tb in tables) {
    got <- rmorie:::Fisherex(tb)
    want <- stats::fisher.test(tb)
    expect_equal(got$p_value, want$p.value, tolerance = 1e-9)
    # the one-sided tails against fisher.test's own
    expect_equal(rmorie:::Fisherex(tb, "greater")$p_value,
                 stats::fisher.test(tb, alternative = "greater")$p.value,
                 tolerance = 1e-9)
    expect_equal(rmorie:::Fisherex(tb, "less")$p_value,
                 stats::fisher.test(tb, alternative = "less")$p.value,
                 tolerance = 1e-9)
    # the point probability of the observed table
    expect_equal(got$prob,
                 stats::dhyper(tb[1, 1], sum(tb[1, ]), sum(tb[2, ]),
                               sum(tb[, 1])))
    expect_equal(got$statistic, tb[1, 1])
  }
  # the two tails overlap at the observed table
  tb <- rbind(c(3, 1), c(1, 3))
  f <- rmorie:::Fisherex(tb)
  expect_equal(f$p_greater + f$p_less, 1 + f$prob)
  expect_equal(f$support, c(max(0, 4 - 4), min(4, 4)))
  # the one-sided entry point agrees with the two-sided one's tails
  o <- rmorie:::Fisherex1(tb, "greater")
  expect_equal(o$p_value, f$p_greater)
  expect_equal(rmorie:::Fisherex1(tb, "less")$p_value, f$p_less)
  expect_equal(o$mean, sum(tb[1, ]) * sum(tb[, 1]) / sum(tb))

  expect_error(rmorie:::Fisherex(rbind(c(1, 2, 3), c(4, 5, 6))), "2 x 2")
  expect_error(rmorie:::Fisherex(rbind(c(-1, 2), c(3, 4))),
               "non-negative")
  expect_error(rmorie:::Fisherex(tb, "sideways"),
               "two-sided, greater or less")
  expect_error(rmorie:::Fisherex1(tb, "two-sided"), "greater or less")
})

test_that("McNemar's test matches stats::mcnemar.test and binom.test", {
  tb <- rbind(c(20, 12), c(5, 30))
  got <- rmorie:::Mcnemarq(tb)
  expect_equal(got$statistic,
               as.numeric(stats::mcnemar.test(tb, correct = FALSE)$statistic))
  expect_equal(got$df, 1L)
  expect_equal(got$p_value, stats::mcnemar.test(tb,
                                                correct = FALSE)$p.value)
  # the correction is the one mcnemar.test applies by default
  expect_equal(rmorie:::Mcnemarq(tb, correct = TRUE)$statistic,
               as.numeric(stats::mcnemar.test(tb)$statistic))
  # only the discordant pairs matter: the diagonal is irrelevant
  other <- rbind(c(999, 12), c(5, 1)) 
  expect_equal(rmorie:::Mcnemarq(other)$statistic, got$statistic)
  expect_equal(got$x12, 12)
  expect_equal(got$x21, 5)
  expect_equal(got$ndisc, 17)

  # THE EXACT FORM is the two-sided sign test on the discordant pairs,
  # which is binom.test at p = 1/2
  expect_equal(got$p_exact, stats::binom.test(5, 17)$p.value,
               tolerance = 1e-9)
  expect_equal(rmorie:::Mcnemarq(rbind(c(0, 9), c(1, 0)))$p_exact,
               stats::binom.test(1, 10)$p.value, tolerance = 1e-9)
  # equal discordant counts are no evidence at all
  eq <- rmorie:::Mcnemarq(rbind(c(5, 8), c(8, 5)))
  expect_equal(eq$statistic, 0)
  expect_equal(eq$p_value, 1)
  expect_equal(eq$p_exact, 1)
  expect_error(rmorie:::Mcnemarq(rbind(c(5, 0), c(0, 5))),
               "no discordant pairs")
  expect_error(rmorie:::Mcnemarq(matrix(1:6, nrow = 2)), "2 x 2")

  # the interval for the difference in marginal proportions
  ci <- rmorie:::Mcnemarci(tb, alpha = 0.05)
  nn <- sum(tb)
  p12 <- 12 / nn
  p21 <- 5 / nn
  expect_equal(ci$estimate, p12 - p21)
  expect_equal(ci$se, sqrt((p12 + p21 - (p12 - p21)^2) / nn))
  expect_equal(ci$se_null, sqrt((p12 + p21) / nn))
  expect_equal(ci$lower, ci$estimate - stats::qnorm(0.975) * ci$se)
  expect_equal(ci$upper, ci$estimate + stats::qnorm(0.975) * ci$se)
  expect_equal(ci$n, nn)
  # the interval brackets its estimate, and a smaller alpha widens it
  expect_true(ci$lower <= ci$estimate && ci$estimate <= ci$upper)
  expect_lt(rmorie:::Mcnemarci(tb, alpha = 0.01)$lower, ci$lower)
  # symmetric discordance estimates no difference
  expect_equal(rmorie:::Mcnemarci(rbind(c(5, 8), c(8, 5)))$estimate, 0)
  expect_error(rmorie:::Mcnemarci(tb, alpha = 0), "strictly inside")
  expect_error(rmorie:::Mcnemarci(matrix(0, 2, 2)), "positive counts")
})

test_that("multinomial goodness of fit matches stats::chisq.test", {
  o <- c(20, 30, 25, 25)
  p <- c(0.2, 0.3, 0.25, 0.25)
  got <- rmorie:::Multgof(o, p)
  want <- stats::chisq.test(o, p = p)
  expect_equal(got$statistic, as.numeric(want$statistic))
  expect_equal(got$df, as.integer(want$parameter))
  expect_equal(got$p_value, want$p.value)
  # counts exactly at expectation have nothing to explain
  expect_equal(rmorie:::Multgof(c(20, 30, 25, 25) * 2, p)$statistic, 0,
               tolerance = 1e-9)
  # estimated parameters cost degrees of freedom. Shown on counts that
  # do NOT fit perfectly -- with a statistic of zero both p-values are
  # one and the comparison says nothing.
  off <- c(25, 25, 25, 25)
  base <- rmorie:::Multgof(off, p)
  fewer <- rmorie:::Multgof(off, p, ddof = 1)
  expect_gt(base$statistic, 0)
  expect_equal(base$df, 3L)
  expect_equal(fewer$df, 2L)
  expect_equal(fewer$statistic, base$statistic)
  # the same statistic on fewer degrees of freedom is more significant
  expect_lt(fewer$p_value, base$p_value)
  expect_equal(rmorie:::Multgof(o, p, ddof = 1)$df, 2L)
  # a badly-fitting set of counts gives a large statistic
  expect_gt(rmorie:::Multgof(c(80, 5, 5, 10), p)$statistic, got$statistic)

  expect_error(rmorie:::Multgof(c(1), c(1)), "at least 2 matching")
  expect_error(rmorie:::Multgof(o, c(0.1, 0.2, 0.3)),
               "at least 2 matching")
  expect_error(rmorie:::Multgof(o, c(0.2, 0.2, 0.2, 0.2)), "sum to 1")
  expect_error(rmorie:::Multgof(o, c(0.5, 0.5, 0, 0)),
               "strictly positive")
})

test_that("the linear-by-linear test uses midrank scores by default", {
  tb <- rbind(c(10, 8, 4), c(3, 7, 12))
  got <- rmorie:::Linbylin(tb)
  cs <- colSums(tb)
  # the default scores are the midranks of the pooled column ordering
  expect_equal(got$scores, cumsum(c(0, cs[-3])) + (cs + 1) / 2,
               ignore_attr = TRUE)
  n1 <- sum(tb[1, ])
  nn <- sum(tb)
  w <- got$scores
  wbar <- sum(cs * w) / nn
  expect_equal(got$statistic, sum(w * tb[1, ]))
  expect_equal(got$mean, n1 * wbar)
  expect_equal(got$var, n1 * sum(tb[2, ]) / (nn * (nn - 1)) *
                 sum(cs * (w - wbar)^2))
  expect_equal(got$z, (got$statistic - got$mean) / sqrt(got$var))
  expect_equal(got$p_twosided, 2 * (1 - stats::pnorm(abs(got$z))))
  expect_equal(got$n, nn)
  # this table has the first row concentrated in the low columns, so the
  # trend is real
  expect_lt(got$p_twosided, 0.01)

  # custom scores are honoured, and equally-spaced ones give the usual
  # linear trend test
  eq <- rmorie:::Linbylin(tb, scores = c(1, 2, 3))
  expect_equal(eq$scores, c(1, 2, 3))
  expect_equal(eq$statistic, sum(c(1, 2, 3) * tb[1, ]))
  expect_lt(eq$p_twosided, 0.01)
  # a table with no trend at all
  flat <- rbind(c(10, 10, 10), c(10, 10, 10))
  expect_equal(rmorie:::Linbylin(flat)$z, 0)
  expect_equal(rmorie:::Linbylin(flat)$p_twosided, 1)

  expect_error(rmorie:::Linbylin(matrix(1:9, nrow = 3)), "exactly 2 rows")
  expect_error(rmorie:::Linbylin(matrix(1:2, nrow = 2)), ">= 2")
  expect_error(rmorie:::Linbylin(tb, scores = c(1, 2)), "length c")
})

test_that("the odds ratio is Woolf's logit interval", {
  tb <- rbind(c(20, 10), c(8, 25))
  got <- rmorie:::Oddsrat(tb)
  expect_equal(got$estimate, 20 * 25 / (10 * 8))
  expect_equal(got$log_or, log(got$estimate))
  # Woolf's variance is the sum of the reciprocal cell counts
  expect_equal(got$se, sqrt(1 / 20 + 1 / 10 + 1 / 8 + 1 / 25))
  z <- stats::qnorm(0.975)
  expect_equal(got$lower, exp(got$log_or - z * got$se))
  expect_equal(got$upper, exp(got$log_or + z * got$se))
  # the interval brackets the estimate, on the ratio scale
  expect_true(got$lower < got$estimate && got$estimate < got$upper)
  # the Wald chi-square on the log scale
  expect_equal(got$statistic, got$log_or^2 / got$se^2)
  expect_equal(got$df, 1L)
  expect_equal(got$p_value, stats::pchisq(got$statistic, 1,
                                          lower.tail = FALSE))
  # an odds ratio of 1 is no association: the interval covers 1
  null <- rmorie:::Oddsrat(rbind(c(10, 10), c(10, 10)))
  expect_equal(null$estimate, 1)
  expect_equal(null$log_or, 0)
  expect_equal(null$statistic, 0)
  expect_equal(null$p_value, 1)
  expect_true(null$lower < 1 && null$upper > 1)
  # the association is significant at conventional levels
  expect_lt(got$p_value, 0.01)
  # the point estimate agrees with fisher.test's conditional MLE in
  # direction, though not in value
  expect_gt(got$estimate, 1)
  expect_gt(as.numeric(stats::fisher.test(tb)$estimate), 1)

  # a zero cell has no logit, and the continuity constant is the way out
  zero <- rbind(c(0, 10), c(8, 25))
  expect_error(rmorie:::Oddsrat(zero), "every cell must be positive")
  fixed <- rmorie:::Oddsrat(zero, cc = 0.5)
  expect_equal(fixed$estimate, 0.5 * 25.5 / (10.5 * 8.5))
  expect_true(is.finite(fixed$lower) && is.finite(fixed$upper))
  expect_error(rmorie:::Oddsrat(matrix(1:6, nrow = 2)), "2 x 2")
  expect_error(rmorie:::Oddsrat(tb, alpha = 1), "strictly inside")
})
