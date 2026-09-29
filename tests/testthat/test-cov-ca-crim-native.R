# Coverage tests for R/ca_crim_native.R (Weisburd et al. 2022). Expected
# values are recomputed with lm, glm, anova, chisq.test and t.test.

test_that("simple OLS and the two-predictor standardised slopes", {
  x <- c(1.2, 2.5, 3.1, 4.8, 5.5, 7.2)
  y <- c(2.1, 3.9, 4.2, 7.5, 7.9, 11.3)
  r <- morie_ols_simple(x, y)
  fit <- summary(lm(y ~ x))
  expect_equal(c(r$b0, r$b1), unname(coef(fit)[, 1]), tolerance = 1e-12)
  expect_equal(r$se_b1, coef(fit)[2, 2], tolerance = 1e-12)
  expect_equal(r$t, coef(fit)[2, 3], tolerance = 1e-12)
  expect_equal(r$t_from_r, r$t, tolerance = 1e-12)
  expect_equal(r$r, cor(x, y), tolerance = 1e-12)
  expect_error(morie_ols_simple(1:2, 1:2))
  x2 <- c(0.3, 1.9, 1.1, 2.8, 2.2, 3.5)
  b <- coef(lm(y ~ x + x2))
  s <- morie_ols_two_iv(cor(y, x), cor(y, x2), cor(x, x2), sd(y), sd(x), sd(x2))
  expect_equal(c(s$b1, s$b2), unname(b[2:3]), tolerance = 1e-12)
  expect_error(morie_ols_two_iv(0.5, 0.5, 1, 1, 1, 1))
})

test_that("fit indices and the F change test", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  x2 <- c(2, 1, 4, 3, 6, 5, 8)
  y <- c(1.5, 2.2, 3.9, 4.1, 5.8, 6.1, 8.4)
  f1 <- lm(y ~ x)
  r <- morie_fit_indices(y, fitted(f1), k = 1)
  s <- summary(f1)
  expect_equal(r$r2, s$r.squared, tolerance = 1e-12)
  expect_equal(r$adj_r2, s$adj.r.squared, tolerance = 1e-12)
  expect_equal(r$f_from_r2, unname(s$fstatistic[1]), tolerance = 1e-12)
  expect_equal(r$var_resid, mean(residuals(f1)^2), tolerance = 1e-12)
  f2 <- lm(y ~ x + x2)
  a <- anova(f1, f2)
  fc <- morie_f_change(sum(residuals(f1)^2), sum(residuals(f2)^2), k_full = 3, k_restricted = 2, n = 7)
  expect_equal(fc$f_ss, a$F[2], tolerance = 1e-12)
  fr <- morie_f_change(r2_full = summary(f2)$r.squared, r2_restricted = s$r.squared, k_full = 2, k_restricted = 1, n = 7)
  expect_equal(fr$f_r2, a$F[2], tolerance = 1e-12)
  expect_error(morie_f_change(1, 1, k_full = 1, k_restricted = 1, n = 5))
})

test_that("standardised coefficients, VIF, logit link", {
  expect_equal(morie_std_coef(0.4, 2.5, 5), 0.2)
  expect_equal(morie_std_coef(0.4, 2.5), 1)
  expect_equal(morie_std_coef(0.4, 2.5, gelman = TRUE), 2)
  expect_error(morie_std_coef(1, 0))
  v <- morie_vif_tolerance(0.75)
  expect_equal(c(v$tolerance, v$vif), c(0.25, 4))
  expect_error(morie_vif_tolerance(1))
  l <- morie_logit_link(0.2, xb = 0.7, b = -0.3)
  expect_equal(l$logit, qlogis(0.2), tolerance = 1e-12)
  expect_equal(l$p_from_xb, plogis(0.7), tolerance = 1e-12)
  expect_equal(l$odds_ratio, exp(-0.3), tolerance = 1e-12)
  expect_error(morie_logit_link(1))
})

test_that("logistic effects agree with a fitted glm", {
  x <- c(-2, -1.3, -0.4, 0.2, 0.9, 1.4, 2.2, -0.8, 0.5, 1.8)
  y <- c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1)
  g <- glm(y ~ x, family = binomial())
  b <- unname(coef(g)[2])
  se <- summary(g)$coefficients[2, 2]
  pred <- sum((fitted(g) > 0.5) == (y == 1))
  r <- morie_logistic_effects(mean(y), b, se, pred, 10, g$null.deviance, g$deviance, n = 10)
  expect_equal(r$model_chi2, g$null.deviance - g$deviance, tolerance = 1e-12)
  expect_equal(r$z, unname(summary(g)$coefficients[2, 3]), tolerance = 1e-12)
  expect_equal(r$dm, unname(0.25 * b), tolerance = 1e-12)
  expect_equal(r$cox_snell_r2, 1 - exp(-(g$null.deviance - g$deviance) / 10), tolerance = 1e-12)
  expect_equal(morie_logistic_effects(0.5, 1, 1, 1, 2, 10, 8, neg2ll_reduced = 9)$lr_chi2, 1)
  expect_error(morie_logistic_effects(1, 1, 1, 1, 2, 10, 8))
})

test_that("multinomial and ordinal logit pieces", {
  xb <- c(0.2, -1.1, 0.8)
  m <- morie_mlogit_probs(xb)
  expect_equal(m$probs, exp(xb) / sum(exp(xb)), tolerance = 1e-12)
  expect_equal(m$or_matrix[1, 3], exp(0.2 - 0.8), tolerance = 1e-12)
  o <- morie_ordinal_logit_ca(c(0.2, 0.3, 0.5), 2, tau_m = 0.4, xb = 0.1)
  expect_equal(o$cum_prob, 0.5)
  expect_equal(o$cum_logit, 0)
  expect_equal(c(o$logit_plus, o$logit_minus), c(0.5, 0.3))
  expect_error(morie_ordinal_logit_ca(c(0.2, 0.3, 0.5), 3))
})

test_that("count GLM predictions, quasi-Poisson dispersion and negative binomial variance", {
  x <- c(0, 1, 2, 3, 4, 5, 6, 7)
  y <- c(1, 0, 3, 2, 6, 4, 9, 7)
  ctl <- glm.control(epsilon = 1e-14, maxit = 100)
  g <- glm(y ~ x, family = poisson(), control = ctl)
  b <- coef(g)
  r <- morie_count_glm(b[1], b[2], x1 = 3, exposure = 2, y = y, yhat = fitted(g), k = 1,
    se = summary(g)$coefficients[2, 2], mu = 4, alpha = 0.3)
  expect_equal(unname(r$predict), unname(exp(b[1] + 3 * b[2])), tolerance = 1e-12)
  expect_equal(unname(r$predict_offset), unname(2 * exp(b[1] + 3 * b[2])), tolerance = 1e-12)
  # Pearson dispersion, the quasi-Poisson scale (McCullagh and Nelder 1989)
  th <- sum((y - fitted(g))^2 / fitted(g)) / 6
  expect_equal(r$theta, th, tolerance = 1e-12)
  expect_equal(unname(r$se_quasi), summary(g)$coefficients[2, 2] * sqrt(th), tolerance = 1e-12)
  # quasipoisson's own scale is computed from the last IRLS weights: 1e-6 agreement
  expect_equal(r$theta, summary(glm(y ~ x, family = quasipoisson()))$dispersion, tolerance = 1e-6)
  expect_equal(r$negbin_var, 4 + 16 * 0.3)
  expect_error(morie_count_glm(exposure = 0))
})

test_that("HLM variance components and ICC", {
  h <- morie_hlm_components(ms_between = 12, ms_within = 2, n_per_cluster = 5, ll_null = -110, ll_full = -104)
  expect_equal(h$sigma2_u, 2)
  expect_equal(h$icc, 0.5)
  expect_equal(h$lr_chi2, 12)
  expect_true(is.na(morie_hlm_components(1, 2, 5)$icc))
  expect_error(morie_hlm_components(1, 2, 0))
})

test_that("power approximations follow the textbook normal approximation", {
  r <- morie_power_ttest_crim(d = 0.5, n1 = 40, n2 = 40, t_cv = qt(0.975, 78), df = 78,
    f = 0.25, n_total = 60, r = 0.3, n = 50)
  dd <- 0.5 * sqrt(1600 / 80)
  tc <- qt(0.975, 78)
  expect_equal(r$delta_d, dd, tolerance = 1e-12)
  expect_equal(r$power, 1 - pnorm((tc * (1 - 1 / 312) - dd) / sqrt(1 + tc^2 / 156)), tolerance = 1e-12)
  # the approximation is close to the exact noncentral-t power
  expect_equal(r$power, power.t.test(n = 40, delta = 0.5)$power, tolerance = 1e-2)
  expect_equal(r$lambda, 60 * 0.0625)
  expect_equal(r$delta_r, 0.3 * sqrt(48) / sqrt(0.91), tolerance = 1e-12)
  expect_length(morie_power_ttest_crim(), 0L)
})

test_that("RCT tests: regression adjustment, t, chi-square, paired t", {
  g1 <- c(5.1, 6.2, 4.8, 7.0, 5.5)
  g2 <- c(4.0, 4.4, 5.2, 3.9, 4.6, 4.1)
  r <- morie_rct_tests(m1 = mean(g1), m2 = mean(g2), s1 = sd(g1), s2 = sd(g2), n1 = 5, n2 = 6,
    a = 12, b = 8, c = 5, d = 15, differences = g1 - g2[1:5])
  tt <- t.test(g1, g2, var.equal = TRUE)
  expect_equal(r$t, unname(tt$statistic), tolerance = 1e-12)
  expect_equal(r$df, 9)
  ct <- chisq.test(matrix(c(12, 5, 8, 15), 2), correct = FALSE)
  expect_equal(r$chi2, unname(ct$statistic), tolerance = 1e-12)
  pt <- t.test(g1 - g2[1:5])
  expect_equal(r$t_paired, unname(pt$statistic), tolerance = 1e-12)
  yv <- c(3, 5, 4, 8, 6, 9)
  tr <- c(0, 0, 0, 1, 1, 1)
  xv <- c(1, 3, 2, 3, 2, 4)
  b <- coef(lm(yv ~ tr + xv))[2]
  rr <- morie_rct_tests(r_yt = cor(yv, tr), r_yx = cor(yv, xv), r_tx = cor(tr, xv), s_y = sd(yv), s_t = sd(tr))
  expect_equal(rr$b_t, unname(b), tolerance = 1e-12)
  expect_equal(rr$b_t_random, unname(coef(lm(yv ~ tr))[2]), tolerance = 1e-12)
  expect_error(morie_rct_tests(a = 0, b = 0, c = 1, d = 1))
})

test_that("experimental ANOVA: one-way and randomised blocks", {
  gs <- list(c(4, 5, 6, 5), c(7, 8, 6), c(3, 2, 4, 3, 3))
  r <- morie_experiment_anova(groups = gs)
  a <- anova(lm(unlist(gs) ~ factor(rep(1:3, lengths(gs)))))
  expect_equal(r$f, a$`F value`[1], tolerance = 1e-12)
  expect_equal(c(r$df1, r$df2), c(2, 9))
  y <- c(10, 12, 11, 14, 15, 13, 9, 11, 12)
  trt <- rep(c("a", "b", "c"), 3)
  blk <- rep(1:3, each = 3)
  rb <- morie_experiment_anova(y = y, treatment = trt, block = blk)
  a2 <- anova(lm(y ~ factor(trt) + factor(blk)))
  expect_equal(rb$f_treatment, a2$`F value`[1], tolerance = 1e-12)
  expect_equal(rb$ss_block, a2$`Sum Sq`[2], tolerance = 1e-12)
  expect_error(morie_experiment_anova(groups = list(1:3)))
})

test_that("PSM standardised bias and meta-analytic effect sizes", {
  expect_equal(morie_psm_balance(5, 4, 2, 1), 100 / sqrt(2.5), tolerance = 1e-12)
  expect_error(morie_psm_balance(1, 1, 0, 0))
  e <- morie_meta_effect_sizes(m1 = 10, m2 = 8, s1 = 3, s2 = 2.5, n1 = 20, n2 = 25, t_value = 2.4,
    a = 15, b = 5, c = 8, d = 12, r = 0.35)
  sp <- sqrt((19 * 9 + 24 * 6.25) / 43)
  expect_equal(e$d, 2 / sp, tolerance = 1e-12)
  expect_equal(e$g, (1 - 3 / (4 * 45 - 9)) * 2 / sp, tolerance = 1e-12)
  expect_equal(e$or, 15 * 12 / 40, tolerance = 1e-12)
  expect_equal(e$rr, (15 / 20) / (8 / 20), tolerance = 1e-12)
  expect_equal(e$fisher_z, atanh(0.35), tolerance = 1e-12)
  expect_equal(e$se_fisher_z, 1 / sqrt(17), tolerance = 1e-12)
  expect_equal(e$d_from_t, 2.4 * sqrt(45 / 500), tolerance = 1e-12)
})

test_that("meta-analytic conversions (Borenstein/Lipsey-Wilson formulas)", {
  c1 <- morie_meta_convert(ln_or = 0.9, se_ln_or = 0.3, p1 = 0.4, p2 = 0.25, n1 = 50, n2 = 60,
    d = 0.5, se_d = 0.2, rr = 1.5, or_value = 2, r = 0.3, se_r = 0.05)
  expect_equal(c1$d_logit, 0.9 * sqrt(3) / pi, tolerance = 1e-12)
  expect_equal(c1$d_probit, qnorm(0.4) - qnorm(0.25), tolerance = 1e-12)
  h <- 110^2 / 3000
  expect_equal(c1$r_from_d, 0.5 / sqrt(0.25 + h), tolerance = 1e-12)
  expect_equal(c1$rr_from_or, 2 / (0.75 + 0.5), tolerance = 1e-12)
  expect_equal(c1$d_from_r, 0.6 / sqrt(0.91), tolerance = 1e-12)
  # the RR -> OR and OR -> RR conversions invert each other at fixed p2
  back <- morie_meta_convert(or_value = c1$or_from_rr, p2 = 0.25)$rr_from_or
  expect_equal(back, 1.5, tolerance = 1e-12)
  expect_equal(morie_meta_convert(d = 0.5)$r_from_d, 0.5 / sqrt(4.25), tolerance = 1e-12)
})

test_that("fixed- and random-effects pooling with subgroup Q", {
  ys <- c(0.3, 0.5, 0.1, 0.45, 0.25)
  ses <- c(0.1, 0.15, 0.12, 0.2, 0.08)
  w <- 1 / ses^2
  m <- sum(w * ys) / sum(w)
  q <- sum(w * (ys - m)^2)
  r <- morie_meta_pool(ys, ses, groups = c(1, 1, 2, 2, 2))
  expect_equal(r$mean, m, tolerance = 1e-12)
  expect_equal(r$q, q, tolerance = 1e-12)
  tau2 <- max(0, (q - 4) / (sum(w) - sum(w^2) / sum(w)))
  expect_equal(r$tau2, tau2, tolerance = 1e-12)
  qw <- sum(vapply(list(1:2, 3:5), function(i) sum(w[i] * (ys[i] - sum(w[i] * ys[i]) / sum(w[i]))^2), 0))
  expect_equal(r$q_within, qw, tolerance = 1e-12)
  expect_equal(r$q_between, q - qw, tolerance = 1e-12)
  expect_error(morie_meta_pool(ys, c(ses[-1], 0)))
})

test_that("spatial lag model reduced form y = (I - rho W)^-1 (xb + e)", {
  W <- rbind(c(0, 0.5, 0.5), c(1, 0, 0), c(0.5, 0.5, 0))
  xb <- c(1, 2, 0.5)
  e <- c(0.1, -0.2, 0.05)
  y <- morie_sar_lag(0.4, W, xb, e)
  expect_equal(y - 0.4 * as.numeric(W %*% y), xb + e, tolerance = 1e-12)
  expect_error(morie_sar_lag(1, diag(3)[c(2, 3, 1), ], xb, e))
})
