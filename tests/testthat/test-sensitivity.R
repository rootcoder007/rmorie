# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/sensitivity.R
# ---------------------------------------------------------------------------

set.seed(1)

# ---------------------------------------------------------------------------
# E-values
# ---------------------------------------------------------------------------

test_that("e_value_rr point estimate matches closed-form for rr > 1", {
  res <- e_value_rr(2.0)
  expect_equal(as.numeric(res$e_value_point),
               2 + sqrt(2 * (2 - 1)), tolerance = 1e-6)
})

test_that("e_value_rr inverts rr < 1", {
  res <- e_value_rr(0.5)
  expect_equal(as.numeric(res$e_value_point),
               2 + sqrt(2 * (2 - 1)), tolerance = 1e-6)
})

test_that("e_value_rr with CI returns finite CI E-value", {
  res <- e_value_rr(2.0, ci_lower = 1.5, ci_upper = 2.5)
  expect_true(is.finite(as.numeric(res$e_value_ci)))
})

test_that("e_value_or rare-outcome branch equals e_value_rr", {
  a <- e_value_or(2.0)
  b <- e_value_rr(2.0)
  expect_equal(as.numeric(a$e_value_point),
               as.numeric(b$e_value_point), tolerance = 1e-6)
})

test_that("e_value_or common-outcome branch corrects via Zhang-Yu", {
  res <- e_value_or(2.0, prevalence = 0.3)
  expect_true(is.finite(as.numeric(res$e_value_point)))
})

test_that("e_value_hr runs and returns finite point", {
  res <- e_value_hr(2.0, ci_lower = 1.5, ci_upper = 2.5)
  expect_true(is.finite(as.numeric(res$e_value_point)))
})

test_that("e_value_hr handles HR = 1 degenerate case", {
  res <- e_value_hr(1.0)
  expect_true(is.finite(as.numeric(res$e_value_point)))
})

test_that("e_value_d works with se and with n branches", {
  a <- e_value_d(0.5, se = 0.1)
  b <- e_value_d(0.5, n = 100)
  c <- e_value_d(0.5)
  for (r in list(a, b, c))
    expect_true(is.finite(as.numeric(r$e_value_point)))
})

# ---------------------------------------------------------------------------
# Rosenbaum bounds
# ---------------------------------------------------------------------------

test_that("rosenbaum_bounds wilcoxon method runs", {
  set.seed(1)
  t_out <- rnorm(40, mean = 0.5)
  c_out <- rnorm(40)
  res <- rosenbaum_bounds(t_out, c_out, method = "wilcoxon")
  expect_true(is.finite(res$critical_gamma))
  expect_equal(length(res$gamma_values), length(res$p_upper))
})

test_that("rosenbaum_bounds sign method runs", {
  set.seed(1)
  res <- rosenbaum_bounds(rnorm(40, 0.5), rnorm(40), method = "sign")
  expect_true(is.finite(res$critical_gamma))
})

test_that("rosenbaum_bounds mcnemar method runs on binary data", {
  set.seed(1)
  t_out <- rbinom(50, 1, 0.6)
  c_out <- rbinom(50, 1, 0.4)
  res <- rosenbaum_bounds(t_out, c_out, method = "mcnemar")
  expect_true(is.finite(res$critical_gamma))
})

test_that("rosenbaum_bounds unknown method errors", {
  expect_error(rosenbaum_bounds(rnorm(20), rnorm(20), method = "no_such"))
})

test_that("rosenbaum_bounds custom gamma_range honoured", {
  set.seed(1)
  res <- rosenbaum_bounds(rnorm(30, 0.4), rnorm(30),
                          gamma_range = c(1, 2, 3))
  expect_equal(length(res$gamma_values), 3L)
})

# ---------------------------------------------------------------------------
# Tipping-point analysis
# ---------------------------------------------------------------------------

test_that("tipping_point_analysis default delta_range runs", {
  res <- tipping_point_analysis(0.5, 0.1, n_treated = 100, n_control = 100)
  expect_equal(length(res$delta_values), 101L)
  expect_true(is.finite(res$tipping_point))
})

test_that("tipping_point_analysis custom delta_range honoured", {
  res <- tipping_point_analysis(0.5, 0.2, n_treated = 50, n_control = 50,
                                delta_range = seq(-1, 1, length.out = 25))
  expect_equal(length(res$delta_values), 25L)
})

test_that("tipping_point_analysis with tiny SE marks robust", {
  res <- tipping_point_analysis(1.0, 0.01, n_treated = 200, n_control = 200)
  expect_true(is.finite(res$tipping_point))
})

# ---------------------------------------------------------------------------
# Omitted variable bias
# ---------------------------------------------------------------------------

test_that("omitted_variable_bias returns rv_q in [0, 1]", {
  res <- omitted_variable_bias(estimate = 0.5, se = 0.1, dof = 100,
                               r2_yd_x = 0.2, partial_r2_treatment = 0.1)
  expect_gte(res$rv_q, 0)
  expect_lte(res$rv_q, 1)
  expect_gte(res$rv_qa, 0)
  expect_lte(res$rv_qa, 1)
})

test_that("omitted_variable_bias: a non-significant estimate has rv_qa = 0", {
  res <- omitted_variable_bias(estimate = 0.05, se = 0.1, dof = 100,
                               r2_yd_x = 0.01, partial_r2_treatment = 0.01)
  # sensemakr: fq = |t| / sqrt(dof) = 0.05, RV_q = 2 / (1 + sqrt(1 + 4 / fq^2))
  expect_equal(res$rv_q, 2 / (1 + sqrt(1 + 4 / 0.05^2)), tolerance = 1e-12)
  expect_equal(res$rv_qa, 0)
})

test_that("omitted_variable_bias benchmark_covariates writes bounds", {
  res <- omitted_variable_bias(0.5, 0.1, 100, 0.2, 0.1,
                               benchmark_covariates = list(z = 0.05))
  expect_true("z" %in% names(res$benchmark_bounds))
  expect_equal(nrow(res$benchmark_bounds[["z"]]), 1L)
  expect_true("adjusted_estimate" %in% names(res$benchmark_bounds[["z"]]))
})

# ---------------------------------------------------------------------------
# specification_curve
# ---------------------------------------------------------------------------

test_that("specification_curve runs across covariate sets and OLS", {
  set.seed(1)
  n <- 200
  d <- data.frame(
    y = rnorm(n), tx = rbinom(n, 1, 0.5),
    a = rnorm(n), b = rnorm(n), c = rnorm(n))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("a"), c("a", "b"),
                                                   c("a", "b", "c")))
  expect_equal(length(res$estimates), 3L)
})

test_that("specification_curve accepts Python-style (name, fn) sample filters", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), tx = rbinom(120, 1, 0.5),
                  a = rnorm(120), grp = sample(c("x", "y"), 120, replace = TRUE))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("a")),
                             sample_filters = list(
                               list("full", function(df) df),
                               list("x_only", function(df) df[df$grp == "x", ])))
  expect_equal(length(res$estimates), 2L)
})

test_that("specification_curve invalid filter shape errors", {
  d <- data.frame(y = rnorm(40), tx = rbinom(40, 1, 0.5), a = rnorm(40))
  expect_error(specification_curve(d, "y", "tx",
                                   covariate_sets = list(c("a")),
                                   sample_filters = list(list("bad"))))
})

test_that("specification_curve empty result path returns NA medians", {
  set.seed(1)
  d <- data.frame(y = rnorm(20), tx = rbinom(20, 1, 0.5))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("no_such_col")))
  expect_equal(length(res$estimates), 0L)
})

test_that("specification_curve robust model branch runs when MASS available", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), tx = rbinom(120, 1, 0.5),
                  a = rnorm(120))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("a")),
                             model_types = c("ols", "robust"))
  expect_gte(length(res$estimates), 1L)
})

# ---------------------------------------------------------------------------
# manski_bounds
# ---------------------------------------------------------------------------

test_that("manski_bounds returns lower <= upper", {
  set.seed(1)
  res <- manski_bounds(runif(50), runif(50), p_treated = 0.5)
  expect_lte(res$lower_bound, res$upper_bound)
  expect_equal(res$width, res$upper_bound - res$lower_bound)
})

test_that("manski_bounds honours custom outcome_range", {
  set.seed(1)
  res <- manski_bounds(rnorm(50, 1, 0.3), rnorm(50, 0, 0.3),
                       p_treated = 0.4, outcome_range = c(-3, 3))
  expect_true(is.finite(res$width))
})

# ---------------------------------------------------------------------------
# bias_adjusted_estimate + probabilistic_bias_analysis
# ---------------------------------------------------------------------------

test_that("bias_adjusted_estimate returns finite adjusted CI", {
  res <- bias_adjusted_estimate(0.5, 0.1, rr_ud = 2, rr_eu = 2)
  expect_true(is.finite(res$adjusted_estimate))
  expect_lt(res$adjusted_ci_lower, res$adjusted_ci_upper)
})

test_that("probabilistic_bias_analysis returns summaries from MC draws", {
  res <- probabilistic_bias_analysis(0.5, 0.1, n_simulations = 200L,
                                     seed = 1L)
  for (k in c("median_adjusted", "mean_adjusted", "ci_2.5", "ci_97.5",
              "pct_null_included", "pct_same_sign")) {
    expect_true(is.finite(res[[k]]))
  }
  expect_equal(res$n_simulations, 200L)
})

test_that("probabilistic_bias_analysis custom bias_parms honoured", {
  bp <- list(rr_ud = c(1.2, 0.1), rr_eu = c(1.2, 0.1),
             prevalence = c(0.2, 0.05))
  res <- probabilistic_bias_analysis(0.4, 0.1, n_simulations = 100L,
                                     bias_parms = bp, seed = 2L)
  expect_equal(res$n_simulations, 100L)
})

# ---------------------------------------------------------------------------
# sensitivity_summary
# ---------------------------------------------------------------------------

test_that("sensitivity_summary minimal call returns base rows", {
  res <- sensitivity_summary(0.5, 0.1)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("estimate", "se", "ci_lower", "ci_upper", "p_value")
                  %in% res$metric))
})

test_that("sensitivity_summary appends RR / OR / HR rows when supplied", {
  res <- sensitivity_summary(0.5, 0.1, rr = 2, odds_ratio = 2,
                             hazard_ratio = 2, prevalence = 0.2)
  expect_s3_class(res, "data.frame")
  expect_gte(nrow(res), 5L)
})

# ---------------------------------------------------------------------------
# Phase 1.g wrapper-as-extender entry points
# ---------------------------------------------------------------------------

test_that("morie_sensitivity_evalue dispatches OLS via EValue", {
  out <- morie_sensitivity_evalue(estimate = 0.5, se = 0.1, sd = 1,
                                  type = "OLS")
  expect_s3_class(out, "morie_sensitivity_evalue")
  expect_true(is.finite(as.numeric(out$e_value_point)))
  expect_equal(out$type, "OLS")
})

test_that("morie_sensitivity_evalue computes natively (no EValue needed)", {
  out <- morie_sensitivity_evalue(estimate = 0.5, se = 0.1, sd = 1)
  expect_s3_class(out, "morie_sensitivity_evalue")
  expect_true(is.finite(as.numeric(out$e_value_point)))
  expect_true(as.numeric(out$e_value_point) >= 1)
})

test_that("morie_sensitivity_tipping_point dispatches tipr::tip", {
  out <- morie_sensitivity_tipping_point(estimate = 0.5, smd = 0.5)
  expect_s3_class(out, "morie_sensitivity_tipping_point")
  expect_true(grepl("tip", out$method))
  # tipr::tip: confounder-outcome effect b^(1 / smd)
  expect_equal(out$confounder_outcome_effect, 0.5^(1 / 0.5))
})

test_that("morie_sensitivity_tipping_point is native (no tipr)", {
  out <- morie_sensitivity_tipping_point(estimate = 0.5, smd = 0.5)
  expect_s3_class(out, "morie_sensitivity_tipping_point")
  expect_error(morie_sensitivity_tipping_point(estimate = 0.5, r2 = 0.1),
               "omitted_variable_bias")
})

test_that("morie_sensitivity_omitted_var_bias wraps sensemakr on lm", {
  set.seed(7)
  n <- 200L
  d <- rnorm(n)
  z <- rnorm(n)
  y <- 0.5 * d + 0.3 * z + rnorm(n)
  fit <- stats::lm(y ~ d + z)
  out <- morie_sensitivity_omitted_var_bias(fit, treatment = "d",
                                            benchmark_covariates = "z")
  expect_s3_class(out, "morie_sensitivity_omitted_var_bias")
  expect_true(is.finite(as.numeric(out$rv_q)))
  expect_true(grepl("sensemakr", out$method))
})

test_that("morie_sensitivity_omitted_var_bias is native (no sensemakr)", {
  fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
  out <- morie_sensitivity_omitted_var_bias(fit, treatment = "wt")
  expect_s3_class(out, "morie_sensitivity_omitted_var_bias")
  expect_null(out$benchmark_bounds)
})

test_that("morie_sensitivity_konfound dispatches pkonfound", {
  out <- morie_sensitivity_konfound(estimate = 0.5, se = 0.1, n = 200L,
                                    n_covariates = 3L)
  expect_s3_class(out, "morie_sensitivity_konfound")
  expect_true(grepl("konfound", out$method))
})

test_that("morie_sensitivity_konfound is native (no konfound)", {
  out <- morie_sensitivity_konfound(estimate = 0.5, se = 0.1, n = 200L)
  expect_s3_class(out, "morie_sensitivity_konfound")
  expect_true(out$percent_bias_to_invalidate > 0)
})

test_that("rosenbaum_bounds at Gamma = 1 is the signed-rank p-value, zero pairs dropped", {
  tt <- 0.4 + 0.5 * sin(3 * (0:59) + 1)
  cc <- rep(0, 60)
  tt[5] <- 0
  b <- rosenbaum_bounds(tt, cc, c(1, 2))
  d <- tt[tt != cc]
  rk <- rank(abs(d))
  z <- (sum(rk[d > 0]) - sum(rk) / 2) / sqrt(sum(rk^2) / 4)
  expect_equal(b$p_upper[1], stats::pnorm(z, lower.tail = FALSE), tolerance = 1e-12)
  expect_gte(b$p_upper[2], b$p_upper[1])
})

test_that("E-values equal EValue::evalues.*", {
  # reference: EValue evalues.OR(rare = FALSE), evalues.HR(rare = TRUE /
  # FALSE), evalues.MD(se = 0.2), evalues.OLS(se = 0.4, sd = 3)
  o <- e_value_or(2.5, 1.4, 4.0, prevalence = 0.3)
  expect_equal(c(o$e_value_point, o$e_value_ci),
               c(2.53971129469801, 1.6488166904897), tolerance = 1e-10)
  o <- e_value_hr(1.8, 1.2, 2.6, rare = TRUE)
  expect_equal(c(o$e_value_point, o$e_value_ci),
               c(3, 1.68989794855664), tolerance = 1e-10)
  o <- e_value_hr(1.8, 1.2, 2.6)
  expect_equal(c(o$e_value_point, o$e_value_ci),
               c(2.36714327129528, 1.52553091757873), tolerance = 1e-10)
  o <- e_value_d(0.5, se = 0.2)
  expect_equal(c(o$e_value_point, o$e_value_ci),
               c(2.52914198186127, 1.44302956344269), tolerance = 1e-10)
  o <- morie_sensitivity_evalue(1.2, se = 0.4, sd = 3, type = "OLS")
  expect_equal(c(o$e_value_point, o$e_value_ci),
               c(2.23397067263333, 1.52654088731104), tolerance = 1e-10)
})

test_that("omitted-variable bias equals sensemakr", {
  # reference: sensemakr(lm(y ~ d + x1 + x2), "d",
  #   benchmark_covariates = "x2", kd = 1:2)
  i <- 1:200
  dd <- data.frame(x1 = sin(i), x2 = cos(1.7 * i))
  dd$d <- 0.5 * dd$x1 + 0.6 * sin(3.1 * i)
  dd$y <- 0.4 * dd$d + 0.8 * dd$x1 + 0.3 * dd$x2 + 0.5 * cos(2.3 * i)
  m <- stats::lm(y ~ d + x1 + x2, dd)
  w <- morie_sensitivity_omitted_var_bias(m, "d", benchmark_covariates = "x2",
                                          kd = 1:2)
  expect_equal(c(w$rv_q, w$rv_qa, w$partial_r2_treatment),
               c(0.392125514801805, 0.302160713916727, 0.201884150359101),
               tolerance = 1e-10)
  b <- w$benchmark_bounds
  expect_equal(b$bound_label, c("1x x2", "2x x2"))
  expect_equal(b$r2yz.dx, c(0.360605510962075, 0.721211021924157),
               tolerance = 1e-10)
  expect_equal(b$adjusted_estimate, c(0.407276404370285, 0.407147837764754),
               tolerance = 1e-10)
  expect_equal(b$adjusted_lower_CI, c(0.315799103054239, 0.346743696480369),
               tolerance = 1e-10)
  expect_equal(b$adjusted_upper_CI, c(0.498753705686331, 0.46755197904914),
               tolerance = 1e-10)
  # closed form on the same inputs, and the extreme robustness value
  ov <- omitted_variable_bias(0.407404970966835, 0.0578602283905763, 196,
                              0.2, 0.2, benchmark_covariates =
                                list(x2 = c(6.9856611516024e-08,
                                            0.265033083453015)))
  expect_equal(c(ov$rv_q, ov$rv_qa),
               c(0.392125514801805, 0.302160713916727), tolerance = 1e-10)
  expect_equal(ov$benchmark_bounds$x2$adjusted_se, 0.0463847620602711,
               tolerance = 1e-10)
  ox <- omitted_variable_bias(40, 1, 30, 0, 0)
  expect_equal(c(ox$rv_qa, ox$rv_q), c(0.9789403653761, 0.981921804436961),
               tolerance = 1e-10)
})

test_that("konfound and tip equal konfound::pkonfound and tipr::tip", {
  k1 <- morie_sensitivity_konfound(0.4, 0.1, 150, 3)
  expect_equal(c(k1$percent_bias_to_invalidate, k1$rir,
                 k1$impact_threshold_confounder, k1$beta_threshold),
               c(50.5885109341772, 76, 0.182899388060936, 0.197645956263291),
               tolerance = 1e-10)
  k2 <- morie_sensitivity_konfound(-0.12, 0.1, 150, 3)
  expect_equal(c(k2$percent_bias_to_invalidate, k2$rir,
                 k2$impact_threshold_confounder, k2$beta_threshold),
               c(39.2853755934456, 59, 0.0540508223426875,
                 -0.197645956263291), tolerance = 1e-10)
  expect_equal(
    morie_sensitivity_tipping_point(1.8, smd = 0.5)$confounder_outcome_effect,
    3.24, tolerance = 1e-12)
})

test_that("McNemar Rosenbaum bound is the exact binomial tail; Manski width is b - a", {
  tt <- c(rep(1, 30), rep(0, 12), rep(1, 5))
  cc <- c(rep(0, 30), rep(1, 12), rep(1, 5))
  rb <- rosenbaum_bounds(tt, cc, gamma_range = c(1, 2), method = "mcnemar")
  expect_equal(rb$p_upper[1],
               stats::binom.test(30, 42, alternative = "greater")$p.value,
               tolerance = 1e-12)
  expect_equal(rb$p_upper[2],
               stats::pbinom(29, 42, 2 / 3, lower.tail = FALSE),
               tolerance = 1e-12)
  # sign test counts the non-zero pairs only
  rs <- rosenbaum_bounds(c(1, 2, 3, 3, 5), c(0, 0, 3, 3, 1),
                         gamma_range = 1, method = "sign")
  expect_equal(rs$p_upper, stats::pbinom(2, 3, 0.5, lower.tail = FALSE))
  mb <- manski_bounds(c(0.2, 0.9, 0.7), c(0.1, 0.4), p_treated = 0.6)
  expect_equal(mb$width, 1)
  expect_equal(mb$lower_bound, 0.6 * 0.6 + 0 - (0.25 * 0.4 + 0.6))
  # Ding-VanderWeele bounding factor and Schlesselman's exact bias
  ba <- bias_adjusted_estimate(0.5, 0.1, rr_ud = 2, rr_eu = 3)
  expect_equal(ba$bias, log(2 * 3 / (2 + 3 - 1)))
  ba2 <- bias_adjusted_estimate(0.5, 0.1, rr_ud = 2, rr_eu = 2,
                                prevalence_confounder = 0.2)
  expect_equal(ba2$bias, log((1 + 0.4) / (1 + 0.2)))
  tp <- tipping_point_analysis(0.5, 0.15, 100, 100)
  expect_equal(tp$tipping_point, 0.5 - stats::qnorm(0.975) * 0.15)
})
