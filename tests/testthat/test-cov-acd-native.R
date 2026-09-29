# Coverage for the Bilder & Loughin (2025) categorical-data surface. Each
# quantity is recomputed with base R: prop.test's Wilson interval,
# chisq.test, glm(binomial), dbinom / dmultinom / dbeta, and the closed
# forms the book prints.

test_that("binomial inference: MLE variance, Wilson interval and true coverage", {
  z <- stats::qnorm(0.975)
  r <- morie_binomial_inference(7, 25, p = 0.3, z = z)
  expect_equal(r$pmf, stats::dbinom(7, 25, 0.3), tolerance = 1e-12)
  expect_equal(r$var_mle, (7 / 25) * (18 / 25) / 25, tolerance = 1e-12)
  pt <- stats::prop.test(7, 25, correct = FALSE, conf.level = 0.95)
  expect_equal(unname(r$wilson[c("lower", "upper")]), as.numeric(pt$conf.int), tolerance = 1e-12)
  wald <- function(w, n) {
    ph <- w / n
    ph + c(-1, 1) * z * sqrt(ph * (1 - ph) / n)
  }
  tl <- morie_binomial_inference(7, 25, p = 0.3, z = z, interval_fn = wald)$true_level
  cover <- vapply(0:25, function(w) {
    ci <- wald(w, 25)
    ci[1] <= 0.3 && 0.3 <= ci[2]
  }, TRUE)
  expect_equal(tl, sum(stats::dbinom(0:25, 25, 0.3)[cover]), tolerance = 1e-12)
  expect_null(morie_binomial_inference(7, 25)$pmf)
  expect_error(morie_binomial_inference(30, 25))
})

test_that("two-group binomial: Pearson X2 and LRT against chisq.test / glm, OR interval", {
  r <- morie_two_group_binomial(18, 40, 9, 45, z = 1.96)
  tab <- matrix(c(18, 22, 9, 36), 2, byrow = TRUE)
  expect_equal(r$x2, unname(stats::chisq.test(tab, correct = FALSE)$statistic), tolerance = 1e-12)
  g1 <- stats::glm(cbind(c(18, 9), c(22, 36)) ~ factor(1:2), family = stats::binomial())
  expect_equal(r$lrt, g1$null.deviance - g1$deviance, tolerance = 1e-9)
  or <- (18 / 22) / (9 / 36)
  se <- sqrt(1 / 18 + 1 / 22 + 1 / 9 + 1 / 36)
  expect_equal(c(r$or_hat, r$or_lower, r$or_upper), or * exp(c(0, -1.96, 1.96) * se), tolerance = 1e-12)
  expect_null(morie_two_group_binomial(0, 10, 5, 10)$or_hat)
  expect_error(morie_two_group_binomial(0, 10, 0, 10))
})

test_that("logistic regression by Newton-Raphson equals glm(binomial)", {
  x <- cbind(1, c(0.5, 1.2, -0.3, 2.1, 0.8, -1, 1.7, 0.2, -0.6, 1.1))
  y <- c(1, 1, 0, 1, 0, 0, 1, 0, 1, 1)
  r <- morie_logistic_fit(x, y)
  # glm's vcov uses the working weights of its last IRLS pass, so run it
  # to full convergence before comparing covariances
  g <- stats::glm(y ~ x[, 2], family = stats::binomial(), control = stats::glm.control(epsilon = 1e-15, maxit = 100))
  expect_equal(r$beta, unname(stats::coef(g)), tolerance = 1e-9)
  expect_equal(unname(r$cov), unname(stats::vcov(g)), tolerance = 1e-8)
  expect_equal(r$loglik, as.numeric(stats::logLik(g)), tolerance = 1e-9)
  expect_equal(r$deviance, stats::deviance(g), tolerance = 1e-9)
  r2 <- morie_logistic_fit(x, y, b = c(0.1, 0.5))
  p2 <- 1 / (1 + exp(-(x %*% c(0.1, 0.5))))
  expect_equal(r2$loglik, sum(y * log(p2) + (1 - y) * log(1 - p2)), tolerance = 1e-12)
  w <- morie_logistic_wald(b1 = 0.8, var_b1 = 0.09, c = 2, z = 1.96, xs = c(1, 0.5), cov = diag(c(0.04, 0.09)), xb = 0.3)
  expect_equal(c(w$or, w$or_lower, w$or_upper), exp(1.6 + c(0, -1, 1) * 2 * 1.96 * 0.3), tolerance = 1e-12)
  expect_equal(w$var_xb, 0.04 + 0.25 * 0.09, tolerance = 1e-12)
  expect_equal(c(w$pi, w$pi_lower, w$pi_upper), stats::plogis(0.3 + c(0, -1, 1) * 1.96 * sqrt(0.0625)), tolerance = 1e-12)
})

test_that("multinomial pmf, baseline and cumulative logits", {
  expect_equal(morie_multinomial_pmf(c(2, 1, 3), c(0.2, 0.3, 0.5)), factorial(6) / (2 * 1 * 6) * 0.2^2 * 0.3 * 0.5^3, tolerance = 1e-12)
  cm <- rbind(c(1, 2), c(3, 0))
  pm <- rbind(c(0.4, 0.6), c(0.7, 0.3))
  expect_equal(morie_multinomial_pmf(cm, pm, product = TRUE), (3 * 0.4 * 0.36) * (0.7^3), tolerance = 1e-12)
  m <- morie_multicategory_logit(bj0 = 0.5, bjs = c(1, -2), xs = c(0.3, 0.1), logits_2_to_j = c(0.4, -1),
                                 cum_probs = c(0.2, 0.7), j = 2, pi_hat = 0.3, var_pi = 0.0016)
  expect_equal(m$logit, 0.5 + 0.3 - 0.2, tolerance = 1e-12)
  expect_equal(m$probs, exp(c(0, 0.4, -1)) / sum(exp(c(0, 0.4, -1))), tolerance = 1e-12)
  expect_equal(m$pi_j, 0.5, tolerance = 1e-12)
  expect_equal(unname(m$wald), 0.3 + c(-1, 1) * 1.96 * 0.04, tolerance = 1e-12)
  expect_equal(morie_multicategory_logit(bj0 = 0.5, bjs = c(1, -2), xs = c(0.3, 0.1), polr = TRUE)$logit, 0.5 - 0.1, tolerance = 1e-12)
  expect_error(morie_multicategory_logit(cum_probs = c(0.7, 0.2), j = 1))
})

test_that("Poisson score interval and log-linear quantities", {
  p <- morie_poisson_loglinear(mu_hat = 3.2, n = 20, z = 1.96, b0 = 0.1, bs = c(0.5, -0.2), xs = c(1, 2), exposure = 4,
                               beta_x_i = 0.3, beta_z_j = -0.4, beta_xz_ij = 0.2, bxz = c(0.5, 0.1, 0.2, -0.3),
                               beta_z_jp = 0.1, beta_xz_i = 0.25, s_j = 3, s_jp = 1)
  expect_equal(unname(p$score_ci), 3.2 + 1.96^2 / 40 + c(-1, 1) * 1.96 * sqrt((3.2 + 1.96^2 / 80) / 20), tolerance = 1e-12)
  expect_equal(p$mu, exp(0.1 + 0.5 - 0.4), tolerance = 1e-12)
  expect_equal(p$mu_rate, 4 * exp(0.2), tolerance = 1e-12)
  expect_equal(p$mu_cell, exp(0.1 + 0.3 - 0.4 + 0.2), tolerance = 1e-12)
  expect_equal(p$or_loglinear, exp(0.5 + 0.1 - 0.2 + 0.3), tolerance = 1e-12)
  expect_equal(p$mean_ratio, exp((-0.4 - 0.1) + 0.25 * 2), tolerance = 1e-12)
  expect_error(morie_poisson_loglinear(bxz = 1:3))
})

test_that("BIC model averaging, diagnostic prevalence and group testing", {
  b <- morie_bic_model_average(c(102, 100, 105), thetas = c(1, 1.4, 0.8), variances = c(0.1, 0.2, 0.05))
  tau <- exp(-c(2, 0, 5) / 2) / sum(exp(-c(2, 0, 5) / 2))
  expect_equal(b$taus, tau, tolerance = 1e-12)
  th <- sum(tau * c(1, 1.4, 0.8))
  expect_equal(b$var_ma, sum(tau * ((c(1, 1.4, 0.8) - th)^2 + c(0.1, 0.2, 0.05))), tolerance = 1e-12)
  d <- morie_diagnostic_prevalence(pi = 0.2, se = 0.9, sp = 0.95, i_size = 5, pi_tilde = 0.05, b0 = -2, bs = 0.5, xs = 1.2)
  expect_equal(d$prevalence, (0.2 + 0.95 - 1) / (0.9 + 0.95 - 1), tolerance = 1e-12)
  expect_equal(d$expected_tests, 1 + 5 * (0.9 + (1 - 0.9 - 0.95) * 0.95^5), tolerance = 1e-12)
  expect_equal(d$pi_group, stats::plogis(-2 + 0.6), tolerance = 1e-12)
  expect_error(morie_diagnostic_prevalence(pi = 0.2, se = 0.4, sp = 0.5))
})

test_that("exact conditional distribution, survey estimators and Kott-Carr interval", {
  x <- cbind(1, c(0, 1, 1, 0))
  e <- morie_exact_conditional(b = c(0.2, -0.5), x = x, y = c(1, 0, 1, 1), t_values = 0:3, counts = c(1, 3, 3, 1), beta = 0.7, t_obs = 2)
  xb <- as.numeric(x %*% c(0.2, -0.5))
  expect_equal(e$loglik, sum(c(1, 0, 1, 1) * xb - log(1 + exp(xb))), tolerance = 1e-12)
  pr <- c(1, 3, 3, 1) * exp(0.7 * 0:3)
  expect_equal(e$probs, pr / sum(pr), tolerance = 1e-12)
  expect_equal(e$p_at_t, pr[3] / sum(pr), tolerance = 1e-12)
  s <- morie_survey_categorical(weights = c(2, 3, 1, 4), ys = c("a", "b", "a", "a"), category = "a",
                                replicate_estimates = c(0.31, 0.29, 0.35, 0.3), full_estimate = 0.31,
                                var_ni = 4, var_n = 9, cov_ni_n = 3, pi_hat = 0.4, n_hat = 50,
                                var_pi = 0.002, t_crit = 2.1)
  expect_equal(s$n_hat_i, 7)
  expect_equal(s$jackknife_var, 3 / 4 * sum((c(0.31, 0.29, 0.35, 0.3) - 0.31)^2), tolerance = 1e-12)
  expect_equal(s$var_pi_delta, (4 + 0.16 * 9 - 2 * 0.4 * 3) / 2500, tolerance = 1e-12)
  ne <- 0.24 / 0.002
  k <- s$kott_carr
  expect_equal(unname(k["n_effective"]), ne, tolerance = 1e-12)
  lohi <- (2 * ne * 0.4 + 2.1^2 + c(-1, 1) * 2.1 * sqrt(2.1^2 + 4 * ne * 0.24)) / (2 * (ne + 2.1^2))
  expect_equal(unname(k[c("lower", "upper")]), lohi, tolerance = 1e-12)
})

test_that("GLMM means, Bayes rules and spline logits", {
  g <- morie_mrcv_glmm(b0 = 0.2, beta_w_a = 0.1, beta_y_b = -0.3, beta_z_c = 0.4, b1 = 0.5, x = 2, random_intercept = -0.7)
  expect_equal(g$mu, exp(0.4), tolerance = 1e-12)
  expect_equal(g$eta_glmm, 0.2 + 1 - 0.7, tolerance = 1e-12)
  expect_equal(morie_mrcv_glmm(b0 = 0.2, random_intercept = 0.1)$eta_glmm, 0.3, tolerance = 1e-12)
  b <- morie_bayes_binomial(p_a_given_b = 0.9, p_b = 0.01, p_a_given_notb = 0.05, pi = 0.3, w = 4, n = 12, a = 1, b = 2,
                            logliks = c(-3, -1, -2), log_priors = log(c(0.2, 0.5, 0.3)))
  expect_equal(b$posterior_prob, 0.009 / (0.009 + 0.05 * 0.99), tolerance = 1e-12)
  expect_equal(b$posterior_density, stats::dbeta(0.3, 5, 10), tolerance = 1e-12)
  expect_equal(b$bayes_estimate, 5 / 15, tolerance = 1e-12)
  wgt <- exp(c(-3, -1, -2)) * c(0.2, 0.5, 0.3)
  expect_equal(b$grid_weights, wgt / sum(wgt), tolerance = 1e-12)
  bb <- c(0.1, 0.5, -0.2, 0.03, 0.4, -0.1)
  kk <- c(1, 2.5)
  tps <- function(v) bb[1] + bb[2] * v + bb[3] * v^2 + bb[4] * v^3 + bb[5] * pmax(v - 1, 0)^3 + bb[6] * pmax(v - 2.5, 0)^3
  s <- morie_spline_logit(x = 3, knot = 2, coef_left = c(1, 0.5, 0, 0), coef_right = c(0, 1, 0.1, 0), betas = bb, knots = kk, a = 3, b_pt = 0.5)
  expect_equal(s$piecewise, 0 + 3 + 0.9, tolerance = 1e-12)
  expect_equal(s$spline, tps(3), tolerance = 1e-12)
  expect_equal(s$spline_or, exp(tps(3) - tps(0.5)), tolerance = 1e-12)
  expect_equal(morie_spline_logit(x = 1.5, knot = 2, coef_left = c(1, 0.5, 0, 0), coef_right = 0)$piecewise, 1.75, tolerance = 1e-12)
})
