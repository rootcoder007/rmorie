# Coverage for the Brus (2022) spatial-sampling formulas. Each branch is
# recomputed from the book's equation, with lm.wfit / lm.fit for the
# regression estimators, stats::dbeta / pbeta for the Bayesian sample
# size, and brute-force sums for the variance and allocation formulas.

test_that("stratified estimators, variance and cost", {
  r <- morie_stsi_estimators(stratum_means = c(3, 5, 9), stratum_weights = c(0.2, 0.5, 0.3),
                             stratum_variances = c(0.4, 0.9, 1.6), c0 = 100, stratum_costs = c(5, 7, 9), stratum_sizes = c(4, 6, 3))
  expect_equal(r$mean, 0.6 + 2.5 + 2.7, tolerance = 1e-12)
  expect_equal(r$variance, 0.04 * 0.4 + 0.25 * 0.9 + 0.09 * 1.6, tolerance = 1e-12)
  expect_equal(r$cost, 100 + 20 + 42 + 27)
  expect_error(morie_stsi_estimators(stratum_means = 1:2, stratum_weights = c(0.5, 0.6)))
})

test_that("cluster and two-stage estimators, and the optimal two-stage design", {
  r <- morie_cluster_twostage(cluster_totals = c(12, 30, 21), cluster_sizes = c(4, 10, 7), m_population = 200,
                              n_clusters_population = 25, primary_unit_means = c(2.9, 3.4, 3.1, 2.7),
                              s2_between = 0.8, s2_within = 2.4, n = 5, m = 4)
  expect_equal(r$total_pps, 200 / 3 * (3 + 3 + 3), tolerance = 1e-12)
  expect_equal(r$mean_from_total, r$total_pps / 200, tolerance = 1e-12)
  expect_equal(r$total_si, 25 / 3 * 63, tolerance = 1e-12)
  pm <- c(2.9, 3.4, 3.1, 2.7)
  expect_equal(c(r$ts_mean, r$s2_psu, r$ts_variance), c(mean(pm), stats::var(pm), stats::var(pm) / 4), tolerance = 1e-12)
  expect_equal(r$true_variance, 0.8 / 5 + 2.4 / 20, tolerance = 1e-12)
  d <- morie_twostage_design(s_w = 2, s_b = 0.5, c1 = 30, c2 = 4, v_max = 0.05, c_max = 1000)
  m <- 2 / 0.5 * sqrt(30 / 4)
  expect_equal(d$m_opt, m, tolerance = 1e-12)
  # with m at its optimum, V = S_b^2 / n + S_w^2 / (n m) and C = n c1 + n m c2
  expect_equal(0.25 / d$n_for_variance + 4 / (d$n_for_variance * m), 0.05, tolerance = 1e-12)
  expect_equal(d$n_for_budget * 30 + d$n_for_budget * m * 4, 1000, tolerance = 1e-12)
  expect_error(morie_twostage_design(0, 1, 1, 1))
})

test_that("model-assisted difference, GLS, regression and ratio estimators", {
  set.seed(4)
  N <- 50
  xa <- cbind(1, stats::runif(N))
  s <- c(3, 8, 15, 22, 31, 40, 47)
  pis <- rep(7 / N, 7)
  z <- 2 + 3 * xa[s, 2] + stats::rnorm(7, sd = 0.2)
  ma <- 2 + 3 * xa[, 2]
  r <- morie_model_assisted(m_all = ma, z_sample = z, m_sample = ma[s], pi_sample = pis, n_population = N)
  expect_equal(r$difference, mean(ma) + sum((z - ma[s]) / pis) / N, tolerance = 1e-12)
  sg <- c(1, 2, 1, 3, 1, 2, 1)
  g <- morie_model_assisted(x = xa[s, ], z = z, sigma2 = sg, pi = pis)
  expect_equal(g$gls_b, unname(stats::lm.wfit(xa[s, ], z, 1 / (sg * pis))$coefficients), tolerance = 1e-10)
  b <- g$gls_b
  rg <- morie_model_assisted(x_all = xa, b_hat = b, x_sample = xa[s, ], z_sample = z, pi_sample = pis, n_population = N)
  expect_equal(rg$regr_general, mean(xa %*% b) + sum((z - xa[s, ] %*% b) / pis) / N, tolerance = 1e-12)
  sl <- morie_model_assisted(zbar_pi = 3.1, b_hats = c(2, -1), xbar_true = c(0.5, 1.2), xbar_pi = c(0.45, 1.3))
  expect_equal(sl$regr_slopes, 3.1 + 2 * 0.05 - 1 * (-0.1), tolerance = 1e-12)
  ra <- morie_model_assisted(t_pi_z = 120, t_pi_x = 40, t_x_true = 44)
  expect_equal(c(ra$ratio, ra$ratio_g), c(132, 1.1), tolerance = 1e-12)
})

test_that("GREG variances, g-weights, calibration and poststratification", {
  e <- c(0.3, -0.5, 0.1, 0.4, -0.2)
  pis <- c(0.1, 0.2, 0.1, 0.25, 0.2)
  gw <- c(1.1, 0.9, 1, 1.2, 0.8)
  v <- morie_greg_variance(e = e, n = 5, n_population = 60, g = gw, pi = pis, ratio = TRUE)
  s2 <- sum(e^2) / 4
  expect_equal(c(v$s2_e, v$variance, v$ratio_variance), c(s2, (1 - 5 / 60) * s2 / 5, 3600 * s2 / 5), tolerance = 1e-12)
  expect_equal(v$g_variance, (1 - 5 / 60) * sum(gw^2 * e^2) / 20, tolerance = 1e-12)
  th <- sum(e / pis)
  expect_equal(v$mc_variance, sum((5 * e / pis - th)^2) / 20 / 3600, tolerance = 1e-12)
  expect_equal(morie_greg_variance(x_k = 4, xbar_true = 3.5, xbar_sample = 3, s2_x = 2)$g_weight, 1 + 0.5 * 1 / 2, tolerance = 1e-12)
  expect_error(morie_greg_variance(e = e, n = 4))
  c1 <- morie_calibration(group_means_sample = c(4, 6), group_weights = c(0.3, 0.7))
  expect_equal(c1$poststratified, 1.2 + 4.2, tolerance = 1e-12)
  c2 <- morie_calibration(zbar_pi = 5, a_hat = 0.4, pi_sample = pis, m_all_mean = 3, m_ht_mean = 2.8, b_hat = 1.5, n_population = 60)
  expect_equal(c2$calibrated, 5 + 0.4 * (1 - sum(1 / pis) / 60) + 1.5 * 0.2, tolerance = 1e-12)
  c3 <- morie_calibration(pi_sample = pis, b_hat = 0.7, n_population = 60, z_sample = c(1, 2, 3, 4, 5), b_si = 0.9, m_all_mean = 3.3, m_sample_mean = 3)
  expect_equal(c3$intercept, 0.3 * sum(1:5 / pis) / 60, tolerance = 1e-12)
  expect_equal(c3$si_shortcut, 3 + 0.9 * 0.3, tolerance = 1e-12)
})

test_that("balanced sampling and two-phase variances", {
  e <- c(0.3, -0.5, 0.1, 0.4, -0.2)
  pis <- c(0.1, 0.2, 0.1, 0.25, 0.2)
  ck <- c(0.9, 0.8, 0.9, 0.75, 0.8)
  lm_ <- c(2, -1, 1.5, 1, -0.5)
  b <- morie_balanced_twophase(t_pi_z = 500, t_x_true = 210, t_pi_x = 200, b_hat = 2.5, e = e, pi = pis, c_k = ck,
                               n_population = 60, p = 2, e_local_mean = lm_, n = 5)
  expect_equal(b$regression_total, 525)
  expect_equal(b$balanced_variance, sum(ck * (e / pis)^2) * 5 / 3 / 3600, tolerance = 1e-12)
  expect_equal(b$local_mean_variance, 5 / 3 * 2 / 3 * sum((1 - pis) * (e / pis - lm_)^2), tolerance = 1e-12)
  expect_equal(morie_balanced_twophase(e = e, n = 5)$s2_resid, sum(e^2) / 4, tolerance = 1e-12)
  t2 <- morie_balanced_twophase(n1h = c(30, 20), n1 = 50, s2_2h = c(1.2, 0.8), n2h = c(6, 4), zbar_2h = c(3, 5), zbar_hat = 3.8,
                                s2_z = 2, s2_e = 0.5, n_population = 1000, n2 = 10)
  expect_equal(t2$twophase_strat, (0.6^2 * 1.2 / 6 + 0.4^2 * 0.8 / 4) + (0.6 * 0.64 + 0.4 * 1.44) / 50, tolerance = 1e-12)
  expect_equal(t2$twophase_regr, (1 - 50 / 1000) * 2 / 50 + (1 - 10 / 50) * 0.5 / 10, tolerance = 1e-12)
})

test_that("sample sizes, including the Bayesian beta-binomial criteria", {
  s <- morie_sample_size(p_star = 0.3, se_max = 0.05, u_crit = 1.96, s_star = 2, l_max = 0.5, cv_star = 0.4, r_max = 0.1,
                         design_effect = 1.44, n_si = 50)
  expect_equal(s$n_prop_se, 0.21 / 0.0025 + 1, tolerance = 1e-12)
  expect_equal(s$n_mean_length, (1.96 * 2 / 0.25)^2, tolerance = 1e-12)
  expect_equal(s$n_cv, (1.96 * 4)^2, tolerance = 1e-12)
  expect_equal(s$n_prop_length, (1.96 * sqrt(0.21) / 0.25)^2 + 1, tolerance = 1e-12)
  expect_equal(s$n_design_effect, 1.2 * 50, tolerance = 1e-12)
  bb <- morie_sample_size(p = 0.3, z = 6, n = 20, c = 1, d = 2, v = 0.2, l = 0.2, lengths = c(0.2, 0.3), probs = c(0.6, 0.4),
                          coverages = c(0.93, 0.97), alpha = 0.05, l_max = 0.25)
  expect_equal(bb$beta_pdf, stats::dbeta(0.3, 7, 16), tolerance = 1e-12)
  expect_equal(bb$interval_prob, stats::pbeta(0.4, 7, 16) - stats::pbeta(0.2, 7, 16), tolerance = 1e-12)
  expect_equal(bb$expected_length, 0.24, tolerance = 1e-12)
  expect_true(bb$alc_satisfied)
  expect_equal(bb$expected_coverage, 0.946, tolerance = 1e-12)
  expect_false(bb$acc_satisfied)
})

test_that("Ospats allocation and objective", {
  o <- morie_ospats(gamma_bar_h = c(1.5, 2.5), weights = c(0.4, 0.6), n_h = c(3, 5), n = 8, s_h = c(1.2, 1.6), c_h = c(4, 9),
                    zhat_i = 5, zhat_j = 3.5, r2 = 0.8, s2_i = 0.2, s2_j = 0.3, s2_ij = 0.1, d2_upper_sum = 36, n_h_units = 12,
                    per_stratum_sums = c(16, 25), n_population = 90)
  expect_equal(o$stsi_variance, 0.16 * 1.5 / 3 + 0.36 * 2.5 / 5, tolerance = 1e-12)
  expect_equal(o$equal_area_variance, 4 / 64, tolerance = 1e-12)
  ws <- c(0.4, 0.6) * c(1.2, 1.6)
  expect_equal(o$alloc_variance, sum(ws * c(2, 3)) * sum(ws / c(2, 3)) / 8, tolerance = 1e-12)
  expect_equal(o$objective_o, sum(ws)^2, tolerance = 1e-12)
  expect_equal(o$d2, 1.5^2 / 0.8 + 0.3, tolerance = 1e-12)
  expect_equal(o$stratum_variance, 0.25, tolerance = 1e-12)
  expect_equal(o$ospats_objective, 9 / 90, tolerance = 1e-12)
})

test_that("survey variance components, GLS, OLS prediction and trend weights", {
  v <- morie_survey_variances(sigma2 = 4, n = 25, rho_bar = 0.05, s2 = 3, n_population = 400, xbar_d = c(1, 2.5),
                              beta_hat = c(0.3, 1.2), v_d = 0.4, times = c(1, 2, 4, 7), beta0 = 1, beta1 = 2, x_val = 3)
  expect_equal(c(v$v_iid, v$v_autocorrelated, v$n_effective), c(0.16, 0.16 * 2.2, 25 / 2.2), tolerance = 1e-12)
  expect_equal(v$v_fpc, (1 - 25 / 400) * 3 / 25, tolerance = 1e-12)
  expect_equal(v$small_area, 0.3 + 3 + 0.4, tolerance = 1e-12)
  tt <- c(1, 2, 4, 7)
  expect_equal(v$trend_weights, (tt - 3.5) / sum((tt - 3.5)^2), tolerance = 1e-12)
  expect_equal(sum(v$trend_weights * (5 + 2 * tt)), 2, tolerance = 1e-12)
  expect_equal(v$linear_model, 7)
  X <- cbind(1, c(0.2, 0.9, 1.5, 2.2, 3))
  C <- 0.5^abs(outer(1:5, 1:5, "-"))
  zz <- c(1.1, 2.3, 2.9, 4.2, 5.1)
  g <- morie_survey_variances(x = X, c_mat = C, zhat = zz)
  L <- t(chol(C))
  expect_equal(g$gls, unname(stats::lm.fit(forwardsolve(L, X), forwardsolve(L, zz))$coefficients), tolerance = 1e-10)
  o <- morie_survey_variances(x_design = X, z_obs = zz, sigma2_eps = 0.3, x0 = c(1, 1.8))
  expect_equal(o$ols_beta, unname(stats::lm.fit(X, zz)$coefficients), tolerance = 1e-10)
  expect_equal(o$ols_pred_var, 0.3 * (1 + as.numeric(c(1, 1.8) %*% solve(crossprod(X), c(1, 1.8)))), tolerance = 1e-12)
  expect_identical(morie_survey_variances(c_hat = 2L, c_true = 2L, u = 2L)$class_indicator, 1)
  expect_identical(morie_survey_variances(c_hat = 2L, c_true = 1L, u = 2L)$class_indicator, 0)
})

test_that("variogram-design Fisher information, kriging-variance terms and EAC", {
  A <- matrix(c(2, 0.5, 0.5, 1.5), 2)
  dA <- list(diag(2), matrix(c(0, 1, 1, 0), 2))
  vd <- morie_variogram_design(mu = 1, a_i = 0.2, b_ij = -0.1, c_ijk = 0.05, eps = 0.3, a = A, da_list = dA,
                               cov_theta = diag(c(0.1, 0.2)), dv_dtheta = c(0.5, -0.3), v_ok = 0.8, e_tau2 = 0.05)
  expect_equal(vd$nested, 1.45, tolerance = 1e-12)
  Ai <- solve(A)
  fi <- outer(1:2, 1:2, Vectorize(function(i, j) 0.5 * sum(diag(Ai %*% dA[[i]] %*% Ai %*% dA[[j]]))))
  expect_equal(vd$fisher_info, fi, tolerance = 1e-12)
  expect_equal(vd$vkv, 0.1 * 0.25 + 0.2 * 0.09, tolerance = 1e-12)
  expect_equal(vd$akv, 0.85, tolerance = 1e-12)
  et <- morie_variogram_design(cov_theta = matrix(c(0.1, 0.02, 0.02, 0.2), 2), dlam_dtheta = list(c(1, -1), c(0.5, 0.5)), a_mat = A)
  ct <- matrix(c(0.1, 0.02, 0.02, 0.2), 2)
  dl <- list(c(1, -1), c(0.5, 0.5))
  ref <- sum(outer(1:2, 1:2, Vectorize(function(i, j) ct[i, j] * as.numeric(dl[[i]] %*% A %*% dl[[j]]))))
  expect_equal(et$e_tau2, ref, tolerance = 1e-12)
  expect_equal(morie_variogram_design(akv = 0.85, vkv = 0.043, v_ok = 0.8)$eac, 0.85 + 0.043 / 1.6, tolerance = 1e-12)
})
