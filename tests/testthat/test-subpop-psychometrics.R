test_that("DIF sample size grows as the effect shrinks", {
  big <- morie_dif_sample_size(0.6, odds_ratio = 2.0)$n_focal
  small <- morie_dif_sample_size(0.6, odds_ratio = 1.2)$n_focal
  expect_lt(big, small)
  expect_error(morie_dif_sample_size(0.6, odds_ratio = 1), "same response probability")
})

test_that("odds ratio converts to the focal probability correctly", {
  r <- morie_dif_sample_size(0.6, odds_ratio = 1.5)
  # odds 0.6/0.4 = 1.5; times 1.5 = 2.25; p = 2.25/3.25
  expect_equal(r$p_focal, 2.25 / 3.25)
  # Supplying p_focal directly recovers the same odds ratio.
  r2 <- morie_dif_sample_size(0.6, p_focal = 2.25 / 3.25)
  expect_equal(r2$odds_ratio, 1.5)
  expect_equal(r2$n_focal, r$n_focal)
})

test_that("equal-group sample size matches the two-proportion formula", {
  p_r <- 0.6; p_f <- 2.25 / 3.25
  z_a <- stats::qnorm(0.975); z_b <- stats::qnorm(0.8)
  p_bar <- (p_f + p_r) / 2
  expected <- (z_a * sqrt(2 * p_bar * (1 - p_bar)) +
                 z_b * sqrt(p_f * (1 - p_f) + p_r * (1 - p_r)))^2 / (p_r - p_f)^2
  expect_equal(morie_dif_sample_size(0.6, odds_ratio = 1.5)$n_focal, expected)
})

test_that("an unbalanced focal group needs more of the reference group", {
  bal <- morie_dif_sample_size(0.6, odds_ratio = 1.5, ratio = 1)
  unbal <- morie_dif_sample_size(0.6, odds_ratio = 1.5, ratio = 4)
  # The focal requirement falls somewhat, but the total climbs sharply.
  expect_lt(unbal$n_focal, bal$n_focal)
  expect_gt(unbal$n_total, bal$n_total)
  expect_equal(unbal$n_reference, 4 * unbal$n_focal)
})

test_that("higher power and stricter alpha both cost sample", {
  base <- morie_dif_sample_size(0.6, odds_ratio = 1.5)$n_focal
  expect_gt(morie_dif_sample_size(0.6, odds_ratio = 1.5, power = 0.95)$n_focal, base)
  expect_gt(morie_dif_sample_size(0.6, odds_ratio = 1.5, alpha = 0.01)$n_focal, base)
})

test_that("ETS delta scale and classification are exact", {
  d <- morie_dif_delta_mh(c(1, 1.35, 1.9, 0.5))
  expect_equal(d$delta_mh, -2.35 * log(c(1, 1.35, 1.9, 0.5)))
  # OR 0.5 gives delta = -2.35*log(0.5) = 1.629, which is C, not B.
  expect_equal(d$magnitude, c("A", "A", "C", "C"))
  expect_equal(d$favours, c("neither", "focal", "focal", "reference"))
  # Boundary: |delta| = 1 exactly is B, not A.
  or_b <- exp(-1 / 2.35)
  expect_equal(morie_dif_delta_mh(or_b)$magnitude, "B")
  expect_error(morie_dif_delta_mh(-1), "must be positive")
})

test_that("delta is symmetric under inversion of the odds ratio", {
  a <- morie_dif_delta_mh(1.8)$delta_mh
  b <- morie_dif_delta_mh(1 / 1.8)$delta_mh
  expect_equal(a, -b)
})

test_that("invariance comparison computes the nested tests", {
  fits <- data.frame(
    model = c("configural", "metric", "scalar"),
    chisq = c(120.3, 128.9, 162.4),
    df = c(48, 54, 60),
    cfi = c(0.981, 0.979, 0.964),
    rmsea = c(0.041, 0.040, 0.052)
  )
  r <- morie_invariance_compare(fits)
  expect_equal(nrow(r), 2L)
  expect_equal(r$delta_chisq, c(8.6, 33.5))
  expect_equal(r$delta_df, c(6, 6))
  expect_equal(r$p_value, stats::pchisq(c(8.6, 33.5), 6, lower.tail = FALSE))
  expect_equal(r$delta_cfi, c(-0.002, -0.015), tolerance = 1e-12)
  # Metric holds (CFI drop 0.002); scalar fails (drop 0.015 exceeds 0.01).
  expect_equal(r$supported, c(TRUE, FALSE))
})

test_that("invariance comparison enforces ordering and reports odd chi-squares", {
  bad <- data.frame(model = c("a", "b"), chisq = c(10, 12), df = c(20, 10),
                    cfi = c(0.99, 0.98), rmsea = c(0.03, 0.04))
  expect_error(morie_invariance_compare(bad), "least to most constrained")
  # A negative chi-square difference yields NA rather than a nonsense p-value.
  neg <- data.frame(model = c("a", "b"), chisq = c(20, 18), df = c(10, 12),
                    cfi = c(0.98, 0.985), rmsea = c(0.04, 0.038))
  r <- morie_invariance_compare(neg)
  expect_true(is.na(r$p_value))
  expect_true(r$supported)
})

test_that("IRT standard error is the reciprocal square root of information", {
  expect_equal(morie_irt_theta_se(c(4, 9, 16)), c(0.5, 1/3, 0.25))
  expect_error(morie_irt_theta_se(0), "must be positive")
})

test_that("marginal reliability follows its definition", {
  se <- c(0.3, 0.35, 0.4, 0.5)
  expect_equal(morie_irt_marginal_reliability(se), 1 - mean(se^2))
  # Error variance equal to trait variance gives zero reliability.
  expect_equal(morie_irt_marginal_reliability(1), 0)
  # Larger errors, lower reliability.
  expect_lt(morie_irt_marginal_reliability(c(0.6, 0.6)),
            morie_irt_marginal_reliability(c(0.2, 0.2)))
})
