# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Fauzi and Maesono (2023) kernel
# distribution-function shelf. Anchors printed from the Python
# modules at full double precision -- testthat tolerances are
# RELATIVE, so a rounded anchor silently weakens the test.
#
# The fixture is an explicit linear congruential generator pushed
# through the exponential quantile rather than an RNG draw: R and
# Python produce bit-identical values from it, which is what makes an
# exact anchor possible. Exponential is the right law for this book --
# its support is bounded below at zero (so the boundary problem is
# live), its density at the boundary is NOT zero (so the problem
# actually bites), and its mean residual life is constant at 1/rate
# for every t, which is an exact oracle for Ch. 4.

fz_fixture <- function(n = 200L, rate = 1) {
  s <- 12345
  out <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    out[i] <- -log(1 - (s + 0.5) / 4294967296) / rate
  }
  out
}

test_that("the fixture is identical to the one Python anchored against", {
  x <- fz_fixture(200L)
  expect_equal(x[1:3], c(0.020613695695873388, 0.016686293433723794,
                         0.7834128534616488), tolerance = 1e-14)
  expect_equal(mean(x), 1.0106542528838751, tolerance = 1e-12)
})

test_that("morie_fauzi_kde matches morie.fn.fzkde", {
  x <- fz_fixture(200L)
  o <- morie_fauzi_kde(x, grid = c(0.25, 1, 2.5))
  expect_equal(o$density,
               c(0.5785608675984389, 0.4180821185803181,
                 0.08759051900274618), tolerance = 1e-10)
  expect_equal(o$bandwidth, 0.26951743245170817, tolerance = 1e-12)
  expect_false(o$boundary_consistent)
  # the density is exp(-x) on the positive half-line and exactly zero
  # below it. A symmetric kernel cannot know that, and leaks mass
  # onto the negative side -- the defect the whole book is about.
  wide <- morie_fauzi_kde(x, grid = seq(-1, 8, length.out = 400L))
  neg <- wide$grid < 0
  expect_gt(sum(diff(wide$grid[neg]) *
                  (wide$density[neg][-1L] +
                     wide$density[neg][-sum(neg)]) / 2), 0.01)
  expect_error(morie_fauzi_kde(1), "at least 2 observations")
  expect_error(morie_fauzi_kde(x, h = -1), "must be positive")
})

test_that("morie_fauzi_mise matches Python and its optimum is a real minimum", {
  o <- morie_fauzi_mise(1000)
  expect_equal(o$h_optimal, 0.19501993780458401, tolerance = 1e-12)
  expect_equal(o$mise_optimal, 0.001808115076268162, tolerance = 1e-12)
  expect_equal(morie_fauzi_mise(1000, h = 0.2)$mise,
               0.001810473958869391, tolerance = 1e-12)
  expect_equal(o$rate_exponent, -0.8)
  # perturbing the optimiser either way costs MISE
  for (f in c(0.7, 1.4)) {
    expect_gt(morie_fauzi_mise(1000, h = o$h_optimal * f)$mise, o$mise_optimal)
  }
  # the two parts pull opposite ways: that IS the bias-variance trade
  small <- morie_fauzi_mise(1000, h = 0.05)
  large <- morie_fauzi_mise(1000, h = 0.5)
  expect_gt(small$variance_part, large$variance_part)
  expect_lt(small$bias_part, large$bias_part)
  expect_error(morie_fauzi_mise(1), "at least 2")
})

test_that("morie_fauzi_gamma_kde matches Python and is consistent at zero", {
  x <- fz_fixture(200L)
  o <- morie_fauzi_gamma_kde(x, grid = c(0, 1, 3), h = 0.05)
  expect_equal(o$density,
               c(1.028684750688036, 0.39988628864109005,
                 0.047654903076054786), tolerance = 1e-10)
  expect_equal(morie_fauzi_gamma_kde(x, grid = 1)$bandwidth,
               0.1310764497372727, tolerance = 1e-12)
  expect_true(o$boundary_consistent)
  m <- morie_fauzi_gamma_kde(x, grid = c(0, 1, 3), h = 0.05, modified = TRUE)
  expect_equal(m$density,
               c(1.1483212414709147, 0.4175201084458151,
                 0.046621819788810664), tolerance = 1e-10)
  expect_false(identical(o$bias_order, m$bias_order))
  # truth is f(0) = 1, and the gamma kernel gets there while the
  # Gaussian one sits near a half
  expect_lt(abs(o$density[1L] - 1),
            abs(morie_fauzi_kde(x, grid = 0)$density - 1))
  expect_error(morie_fauzi_gamma_kde(c(-1, 2), grid = 1, h = 0.1),
               "0, infinity")
  expect_error(morie_fauzi_gamma_kde(x, grid = 1, h = 0.1, modified = TRUE,
                                     a = 1), "not 1")
})

test_that("morie_fauzi_kdfe matches Python and uses the n^{-1/3} rule", {
  x <- fz_fixture(200L)
  o <- morie_fauzi_kdfe(x, grid = c(0.25, 1, 2.5))
  expect_equal(o$F_hat,
               c(0.20452027147264093, 0.6210325074704985,
                 0.9223749966316391), tolerance = 1e-10)
  expect_equal(o$bandwidth, 0.18660645798666559, tolerance = 1e-12)
  # Sec. 5.3.2 of the book: Azzalini recommended c n^{-1/3} for
  # DISTRIBUTION-function estimation, and the book's own simulations
  # use it. Under the n^{-1/5} density rule this estimator
  # oversmooths enough to lose, in MSE, to the step function it
  # exists to improve on.
  expect_equal(o$bandwidth, stats::sd(x) * 200^(-1 / 3), tolerance = 1e-12)
  expect_true(o$monotone)
  expect_true(all(o$F_hat >= 0 & o$F_hat <= 1))
  # it tracks the truth 1 - exp(-t)
  g <- seq(0.1, 6, length.out = 60L)
  expect_lt(max(abs(morie_fauzi_kdfe(x, grid = g)$F_hat - (1 - exp(-g)))),
            0.07)
})

test_that("morie_fauzi_boundary_free_kde matches Python and carries the Jacobian", {
  x <- fz_fixture(200L)
  g <- c(0.25, 1, 2.5)
  o <- morie_fauzi_boundary_free_kde(x, grid = g)
  expect_equal(o$density,
               c(0.7121311070183438, 0.35415902027854484,
                 0.07738945886512737), tolerance = 1e-10)
  expect_equal(o$bandwidth, 0.46698596234632633, tolerance = 1e-12)
  # for g = exp the change-of-variables factor is exactly 1/t
  expect_equal(o$jacobian, c(4, 1, 0.4), tolerance = 1e-12)
  expect_equal(o$g_prime, g, tolerance = 1e-12)
  # and it beats the naive estimator right at the boundary, where
  # f(0) = 1 is the truth
  expect_lt(abs(morie_fauzi_boundary_free_kde(x, grid = 0.02)$density - 1),
            abs(morie_fauzi_kde(x, grid = 0.02)$density - 1))
  expect_error(morie_fauzi_boundary_free_kde(c(-1, 1, 2), grid = 1),
               "strictly inside")
  expect_error(morie_fauzi_boundary_free_kde(x, grid = -1), "strictly inside")
})

test_that("the two cumulative survival estimators match Python", {
  x <- fz_fixture(60L)
  tg <- c(0.4, 1.2)
  o1 <- morie_fauzi_cumulative_survival_1(x, tg)
  expect_equal(o1$S_cumulative,
               c(0.9605038613590627, 0.5678410430196819), tolerance = 1e-9)
  expect_equal(o1$S_survival,
               c(0.6949935828677948, 0.3364406483098804), tolerance = 1e-10)
  expect_equal(o1$bandwidth, 0.614334209910001, tolerance = 1e-12)
  o2 <- morie_fauzi_cumulative_survival_2(x, tg)
  expect_equal(o2$S_cumulative,
               c(0.698460895513802, 0.34592641526891804), tolerance = 1e-9)
  # the survival part is shared; only the cumulative construction
  # differs, and with it the bias coefficient
  expect_equal(o2$S_survival, o1$S_survival, tolerance = 1e-12)
  expect_true(o1$preserves_derivative_relation)
  expect_false(o2$preserves_derivative_relation)
  expect_true(startsWith(o1$bias_coefficient, "b_2"))
  expect_true(startsWith(o2$bias_coefficient, "b_3"))
  expect_true(o2$same_covariance_as_first)
})

test_that("the first cumulative survival really preserves d/dt = -S", {
  # this is the structural claim that distinguishes (4.8) from
  # (4.17), so it is checked numerically rather than trusted
  x <- fz_fixture(80L)
  tg <- seq(0.4, 2.5, length.out = 40L)
  o <- morie_fauzi_cumulative_survival_1(x, tg)
  d <- diff(o$S_cumulative) / diff(tg)
  mid <- (o$S_survival[-1L] + o$S_survival[-length(tg)]) / 2
  expect_lt(max(abs(d + mid)), 0.02)
})

test_that("the bias coefficients b_1, b_2, b_3 match Python", {
  tv <- c(0.5, 1.5, 3)
  fx <- exp(-tv)
  expect_equal(morie_fauzi_b1_coefficient(tv, fx, f_X_prime = -fx)$b_1,
               c(0.15163266492815836, -0.16734762011132231,
                 -0.2987224102071838), tolerance = 1e-10)
  expect_equal(morie_fauzi_b2_coefficient(tv, fx)$b_2,
               c(0.9616259186391922, 0.9360242227685784,
                 0.4480836153107757), tolerance = 1e-8)
  expect_equal(morie_fauzi_b3_coefficient(tv, fx, S_X = exp(-tv))$b_3,
               c(-0.15163266492815836, 0.16734762011132231,
                 0.2987224102071838), tolerance = 1e-10)
  # for g = exp, g' = g'' = t at z = log t -- a value the
  # implementation cannot produce by accident
  o <- morie_fauzi_b1_coefficient(tv, fx, f_X_prime = -fx)
  expect_equal(o$g_prime, tv, tolerance = 1e-12)
  expect_equal(o$g_double_prime, tv, tolerance = 1e-12)
  # under the identity transform g'' is zero, which must change b_1
  i <- morie_fauzi_b1_coefficient(tv, fx, f_X_prime = -fx,
                                  transform = "identity")
  expect_equal(i$g_double_prime, rep(0, 3L))
  expect_false(isTRUE(all.equal(o$b_1, i$b_1)))
  expect_error(morie_fauzi_b1_coefficient(tv, fx), "f_X_prime")
  expect_error(morie_fauzi_b3_coefficient(tv, fx), "S_X")
  expect_error(morie_fauzi_b3_coefficient(tv, fx, S_X = c(2, 0.5, 0.1)),
               "\\[0, 1\\]")
  expect_error(morie_fauzi_b1_coefficient(tv, c(-1, 1, 1), f_X_prime = -fx),
               "non-negative")
})

test_that("the MRL estimators match Python and recover the exponential mean", {
  xs <- fz_fixture(60L)
  tg <- c(0.4, 1.2)
  expect_equal(morie_fauzi_mrl_boundary_free_2(xs, tg)$mrl,
               c(1.0049889851237184, 1.028194473547384), tolerance = 1e-9)
  expect_equal(morie_fauzi_mrl_naive(fz_fixture(200L), tg)$mrl,
               c(1.035925809425633, 1.0204061215185543), tolerance = 1e-9)
  # memorylessness: E[X - t | X > t] = 1/rate at EVERY t, which is a
  # far stronger oracle than a single value
  x2 <- fz_fixture(200L, rate = 0.5)
  o <- morie_fauzi_mrl_boundary_free_2(x2, seq(0.5, 3, length.out = 8L))
  expect_lt(max(abs(o$mrl - 2)), 0.35)
  expect_false(morie_fauzi_mrl_naive(x2, tg)$boundary_safe)
})

test_that("the boundary-free MRL beats the naive one AT the boundary", {
  # the book's headline claim, measured where it is actually made:
  # near t = 0 the naive (4.2) bias is O(h) and can degrade to O(1)
  x <- fz_fixture(200L)
  tg <- c(0.01, 0.05, 0.1)
  nv <- mean(abs(morie_fauzi_mrl_naive(x, tg)$mrl - 1))
  bf <- mean(abs(morie_fauzi_mrl_boundary_free_2(x, tg)$mrl - 1))
  expect_lt(bf, nv)
})

test_that("morie_fauzi_theorem_4_3 matches Python and separates b_2 from b_3", {
  o <- morie_fauzi_theorem_4_3(1, 0.4, 0.5, 1.25, 0.3, b2 = 0.7, b3 = -0.2,
                               n = 500L, h = 0.1)
  expect_equal(o$bias_1, 0.013437500000000002, tolerance = 1e-12)
  expect_equal(o$bias_2, 0.0021875, tolerance = 1e-12)
  expect_equal(o$variance, 0.004136533609816643, tolerance = 1e-12)
  expect_equal(o$b4, 0.375, tolerance = 1e-12)
  expect_equal(o$b5, 1.5625, tolerance = 1e-12)
  # (4.25)-(4.28) differ ONLY in b_2 versus b_3, so holding them
  # equal must collapse the two biases onto each other
  same <- morie_fauzi_theorem_4_3(1, 0.4, 0.5, 1.25, 0.3, b2 = 0.7,
                                  b3 = 0.7, n = 500L, h = 0.1)
  expect_equal(same$bias_1, same$bias_2, tolerance = 1e-12)
  expect_equal(same$bias_1, o$bias_1, tolerance = 1e-12)
  # bias is O(h^2): quadrupling h multiplies it by sixteen
  big <- morie_fauzi_theorem_4_3(1, 0.4, 0.5, 1.25, 0.3, b2 = 0.7,
                                 n = 500L, h = 0.4)
  expect_equal(big$bias_1 / o$bias_1, 16, tolerance = 1e-9)
  # variance is O(1/n); the bandwidth enters only at O(h/n)
  v10 <- morie_fauzi_theorem_4_3(1, 0.4, 0.5, 1.25, 0.3, b2 = 0.7,
                                 n = 5000L, h = 0.1)$variance
  expect_equal(o$variance / v10, 10, tolerance = 0.05)
  expect_null(morie_fauzi_theorem_4_3(1, 0.4, 0.5, 1.25, 0.3)$bias_1)
  expect_error(morie_fauzi_theorem_4_3(1, 0, 0.5, 1.25, 0.3),
               "strictly positive")
})

test_that("morie_fauzi_theorem_4_4 matches Python", {
  o <- morie_fauzi_theorem_4_4(c(1.2, 0.9), c(1, 1), c(0.01, 0.04))
  expect_equal(o$z, c(1.9999999999999996, -0.4999999999999999),
               tolerance = 1e-12)
  expect_equal(o$p_two_sided, c(0.04550026389635843, 0.6170750774519739),
               tolerance = 1e-12)
  expect_true(o$valid_at_boundary)
  # an estimate sitting on the truth cannot be evidence against it
  expect_equal(morie_fauzi_theorem_4_4(1, 1, 0.01)$p_two_sided, 1,
               tolerance = 1e-12)
  expect_error(morie_fauzi_theorem_4_4(1, 1, -1), "non-negative")
  expect_error(morie_fauzi_theorem_4_4(c(1, 2), 1, 0.1), "same length")
})

test_that("morie_fauzi_theorem_4_5 matches Python and honours the interval", {
  tt <- seq(0, 5, length.out = 51L)
  e <- 0.3 * exp(-((tt - 3)^2) / 0.5)
  o <- morie_fauzi_theorem_4_5(1 + e, rep(1, 51L), tt)
  expect_equal(o$sup_error, 0.30000000000000004, tolerance = 1e-12)
  expect_equal(o$argmax_t, 3, tolerance = 1e-12)
  expect_true(o$requires_bounded_B)
  # restricting to a sub-interval must exclude the spike
  expect_lt(morie_fauzi_theorem_4_5(1 + e, rep(1, 51L), tt,
                                    interval = c(0, 1))$sup_error, 0.01)
  # uniform consistency is stated on a BOUNDED B, so an unbounded
  # request is an error rather than a silent truncation
  expect_error(morie_fauzi_theorem_4_5(1 + e, rep(1, 51L), tt,
                                       interval = c(0, Inf)), "bounded")
  expect_error(morie_fauzi_theorem_4_5(1 + e, rep(1, 51L), tt,
                                       interval = c(9, 10)),
               "no grid points")
})

test_that("morie_fauzi_theorem_4_6 matches Python and recovers the mean", {
  x <- fz_fixture(200L)
  o <- morie_fauzi_theorem_4_6(x, 0, 1.05, h = 0.2)
  expect_equal(o$identity_lhs, 1.05, tolerance = 1e-12)
  expect_equal(o$sample_mean, 1.0106542528838751, tolerance = 1e-12)
  expect_equal(o$gap, 0.03934574711612493, tolerance = 1e-10)
  # (4.29) itself: the MRL at the start of support, plus a_1, IS the
  # sample mean up to O(h^2)
  m0 <- morie_fauzi_mrl_boundary_free_2(x, 1e-6)$mrl
  expect_equal(morie_fauzi_theorem_4_6(x, 0, m0)$identity_lhs,
               mean(x), tolerance = 0.2)
  expect_error(morie_fauzi_theorem_4_6(x, 1, 1), "not a lower bound")
})

test_that("morie_fauzi_conditions_c1_c6 names the binding conditions", {
  o <- morie_fauzi_conditions_c1_c6(fz_fixture(50L))
  expect_true(o$C3_bijective)
  expect_equal(o$binding_in_practice, c("C5", "C6"))
  expect_length(o$conditions, 6L)
  expect_false(o$heavy_tail_warning)
  expect_equal(o$C6_moments$E_X, mean(fz_fixture(50L)), tolerance = 1e-12)
  expect_error(morie_fauzi_conditions_c1_c6(1:3), "at least 4")
})

test_that("morie_fauzi_kernel_quantile matches Python", {
  x <- fz_fixture(200L)
  o <- morie_fauzi_kernel_quantile(x, c(0.25, 0.5, 0.75))
  expect_equal(o$quantile,
               c(0.32896306054178054, 0.7432690123218483,
                 1.4754676174909547), tolerance = 1e-10)
  expect_equal(o$bandwidth, 0.12011244339814311, tolerance = 1e-12)
  expect_equal(o$weights_sum,
               c(0.9813005689072365, 0.9999685584515449,
                 0.9813005689072366), tolerance = 1e-12)
  # truth for Exp(1) is -log(1 - p)
  expect_lt(max(abs(o$quantile - (-log(1 - c(0.25, 0.5, 0.75))))), 0.15)
  expect_error(morie_fauzi_kernel_quantile(x, 0), "strictly in")
  expect_error(morie_fauzi_kernel_quantile(c(1, 2), 0.5), "at least 3")
})

test_that("the kernel quantile is smoother in p than the sample quantile", {
  # the sample quantile is a step function of p -- one order
  # statistic, hopping to the next. This averages all of them.
  x <- fz_fixture(60L)
  ps <- seq(0.3, 0.7, length.out = 200L)
  kq <- vapply(ps, function(p) morie_fauzi_kernel_quantile(x, p)$quantile,
               numeric(1))
  sq <- unname(stats::quantile(x, ps, type = 7L))
  expect_lt(max(abs(diff(kq))), max(abs(diff(sq))))
  expect_true(all(diff(kq) > -1e-9))   # still monotone in p
})

test_that("morie_fauzi_quantile_amse matches Python in both parameterisations", {
  o <- morie_fauzi_quantile_amse(c(0.25, 0.5), 500L,
                                 f_at_quantile = c(0.75, 0.5))
  expect_equal(o$amse, c(0.0006666666666666666, 0.002), tolerance = 1e-12)
  expect_equal(o$binomial_part, c(0.000375, 0.0005), tolerance = 1e-12)
  # (3.3) has two equivalent forms because Q' = 1/f(Q); supplying
  # either input must give the same number
  b <- morie_fauzi_quantile_amse(c(0.25, 0.5), 500L,
                                 Q_prime = 1 / c(0.75, 0.5))
  expect_equal(b$amse, o$amse, tolerance = 1e-12)
  expect_equal(o$se, sqrt(o$amse), tolerance = 1e-12)
  # halving n doubles the AMSE
  expect_equal(morie_fauzi_quantile_amse(0.5, 250L, f_at_quantile = 0.5)$amse,
               2 * o$amse[2L], tolerance = 1e-12)
  # p(1-p) shrinks into the tail, but 1/f^2 grows faster: extreme
  # quantiles get HARDER, not easier
  prev <- NULL
  for (p in c(0.5, 0.9, 0.99, 0.999)) {
    a <- morie_fauzi_quantile_amse(p, 1000L, f_at_quantile = 1 - p)$amse
    if (!is.null(prev)) expect_gt(a, prev)
    prev <- a
  }
  expect_error(morie_fauzi_quantile_amse(0.5, 500L), "either the density")
})

test_that("morie_fauzi_order_m_kernel matches Python and its moments vanish", {
  uu <- c(0, 0.5, 1, 2)
  expect_equal(morie_fauzi_order_m_kernel(uu, m = 2L)$K,
               c(0.3989422804014327, 0.3520653267642995,
                 0.24197072451914337, 0.05399096651318806),
               tolerance = 1e-12)
  expect_equal(morie_fauzi_order_m_kernel(uu, m = 4L)$K,
               c(0.5984134206021491, 0.48408982430091185,
                 0.24197072451914337, -0.02699548325659403),
               tolerance = 1e-12)
  expect_equal(morie_fauzi_order_m_kernel(uu, m = 6L)$K,
               c(0.7480167757526863, 0.5528525834345641,
                 0.18147804338935752, -0.06073983732733657),
               tolerance = 1e-12)
  for (m in c(2L, 4L, 6L)) {
    o <- morie_fauzi_order_m_kernel(uu, m = m)
    expect_equal(o$moments[["0"]], 1, tolerance = 1e-8)
    for (j in seq_len(m - 1L)) {
      expect_lt(abs(o$moments[[as.character(j)]]), 1e-6)
    }
    # the m-th moment is finite and NOT zero: that is what makes the
    # order exactly m rather than higher
    expect_gt(abs(o$moments[[as.character(m)]]), 1e-6)
    expect_equal(o$bias_order, sprintf("O(h^%d)", m))
  }
  # a non-negative function cannot have a vanishing second moment, so
  # the faster rate is bought with negative kernel values
  expect_false(morie_fauzi_order_m_kernel(uu, m = 2L)$takes_negative_values)
  expect_true(morie_fauzi_order_m_kernel(uu, m = 4L)$takes_negative_values)
  expect_true(morie_fauzi_order_m_kernel(uu, m = 6L)$takes_negative_values)
  expect_error(morie_fauzi_order_m_kernel(uu, m = 3L), "2, 4 or 6")
})

test_that("morie_fauzi_muller_kernel turns negative exactly at root three", {
  o <- morie_fauzi_muller_kernel(c(0, 0.5, 1, 2))
  expect_equal(o$K,
               c(0.5984134206021491, 0.48408982430091185,
                 0.24197072451914337, -0.02699548325659403),
               tolerance = 1e-12)
  expect_equal(o$mu0, 1, tolerance = 1e-9)
  expect_lt(abs(o$mu2), 1e-9)
  expect_equal(o$mu4, -3.000000000000001, tolerance = 1e-9)
  expect_equal(o$negative_beyond, sqrt(3), tolerance = 1e-12)
  # the sign change is at u^2 = 3 by construction; a wrong constant
  # would still be smooth and still integrate to one, but would move it
  s <- morie_fauzi_muller_kernel(c(sqrt(3) - 1e-6, sqrt(3), sqrt(3) + 1e-6))$K
  expect_gt(s[1L], 0)
  expect_lt(abs(s[2L]), 1e-6)
  expect_lt(s[3L], 0)
})

test_that("morie_fauzi_lemma_3_1 matches Python and needs the true centre", {
  x <- fz_fixture(200L)
  o <- morie_fauzi_lemma_3_1(x, 0.5, q_true = log(2))
  expect_equal(o$linear_term, 0.009896326355204535, tolerance = 1e-10)
  expect_equal(o$estimate, 0.7432690123218484, tolerance = 1e-10)
  expect_equal(o$remainder, 0.040225505406698565, tolerance = 1e-9)
  expect_equal(o$density_at_quantile, 0.505237986353439, tolerance = 1e-11)
  expect_equal(o$asymptotic_variance, 0.00489686376643578, tolerance = 1e-11)
  expect_equal(o$linear_term, mean(o$influence), tolerance = 1e-12)
  # centred on the sample quantile the linear term collapses -- the
  # empirical df at its own p-quantile IS p up to 1/n -- so the
  # decomposition carries no information, and the module says so
  d <- morie_fauzi_lemma_3_1(x, 0.5)
  expect_lt(abs(d$linear_term), 0.1 * abs(o$linear_term))
  expect_true(grepl("degenerate", d$centred_at))
  expect_error(morie_fauzi_lemma_3_1(x, 1.5), "strictly in")
  expect_error(morie_fauzi_lemma_3_1(1:3, 0.5), "at least 5")
})
