# Rank-weighted average treatment effects (Yadlowsky et al. 2025): AIPW
# scores, the TOC curve, Qini and AUTOC as its weighted integrals, the
# cost-weighted Qini curve, and the half-sample bootstrap test replayed.

sg_g <- c(1.5, -0.2, 0.8, 2.1, 0.1, -0.7, 1.0, 0.4, 0.3, 1.2)
sg_s <- c(0.9, 0.1, 0.5, 1.3, 0.2, -0.4, 0.6, 0.5, 0.0, 1.1)

test_that("AIPW scores combine the outcome model and the IPW residual", {
  y <- c(1, 0, 2, 1.5)
  w <- c(1, 0, 1, 0)
  m1 <- c(0.8, 0.6, 1.5, 1.2)
  m0 <- c(0.2, 0.1, 0.9, 1.0)
  e <- c(0.4, 0.5, 0.6, 0.3)
  ref <- m1 - m0 + w * (y - m1) / e - (1 - w) * (y - m0) / (1 - e)
  expect_equal(aipw_scores(y, w, m1, m0, e), ref, tolerance = 1e-15)
  expect_equal(aipw_scores(y, w, m1, m0, 0.5),
               m1 - m0 + w * (y - m1) / 0.5 - (1 - w) * (y - m0) / 0.5, tolerance = 1e-15)
  expect_error(aipw_scores(y, w * 2, m1, m0, e), "0/1")
  expect_error(aipw_scores(y, w, m1, m0, c(0, e[-1])), "overlap fails")
  expect_error(aipw_scores(y, w[-1], m1, m0, e), "W has 3 entries")
})

test_that("TOC is the top-u mean effect minus the ATE, and RATEs integrate it", {
  tc <- toc_curve(sg_g, sg_s)
  o <- order(-sg_s, seq_along(sg_s))
  ref <- cumsum(sg_g[o]) / seq_along(o) - mean(sg_g)
  expect_equal(tc$toc, ref, tolerance = 1e-15)
  expect_equal(tc$toc[10], 0, tolerance = 1e-15)
  u <- (1:10) / 10
  expect_equal(qini_coefficient(sg_g, sg_s), sum(u * ref) / 10, tolerance = 1e-15)
  expect_equal(autoc(sg_g, sg_s), sum(ref) / 10, tolerance = 1e-15)
  expect_equal(rate(sg_g, sg_s, "uniform")$estimate, sum(ref) / 10, tolerance = 1e-15)
  expect_identical(slicedgrf, rate)
  expect_error(rate(sg_g, sg_s, "linear"), "weight must be one of")
  expect_error(toc_curve(1, 1), "at least 2 units")
  expect_error(toc_curve(sg_g, sg_s[-1]), "10 scores but 9")
})

test_that("the Qini curve accumulates gain against spend", {
  q <- qini_curve(sg_g, sg_s)
  o <- order(-sg_s, seq_along(sg_s))
  expect_equal(q$gain, cumsum(sg_g[o]) / 10, tolerance = 1e-15)
  expect_equal(q$spend, (1:10) / 10, tolerance = 1e-15)
  cost <- seq(1, 2, length.out = 10)
  qc <- qini_curve(sg_g, sg_s, cost = cost)
  expect_equal(qc$spend, cumsum(cost[o]) / sum(cost), tolerance = 1e-15)
  expect_true(qc$constrained)
  expect_error(qini_curve(sg_g, sg_s, cost = c(1, 2)), "2 costs for 10")
  expect_error(qini_curve(sg_g, sg_s, cost = 0), "positive")
})

test_that("the half-sample bootstrap replays and scales the variance by 1/2", {
  r <- rate_test(sg_g, sg_s, "qini", reps = 25, seed = 3)
  e <- .ghc_rng(3)
  d <- vapply(1:25, function(k) {
    idx <- order(.ghc_unif(e, 10L), 0:9)[1:5]
    qini_coefficient(sg_g[idx], sg_s[idx])
  }, 1)
  se <- sqrt(stats::var(d) / 2)
  expect_equal(r$se, se, tolerance = 1e-14)
  expect_equal(r$z, qini_coefficient(sg_g, sg_s) / se, tolerance = 1e-13)
  expect_equal(r$p_value, 2 * stats::pnorm(-abs(r$z)), tolerance = 1e-14)
  expect_error(rate_test(sg_g[1:6], sg_s[1:6]), "at least 8 units")
  expect_same_function(morie_slvgrf$rate, rate)
})
