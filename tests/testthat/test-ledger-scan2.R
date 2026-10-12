# The second library-scan batch (2026-10-12): every number recomputed from the theorem it cites.

test_that("an NB matches the ZIP zero share and mean; the zero share lies strictly inside (e^-mu, 1)", {
  r <- morie_nb_matches_zip(pi = 0.3, lambda = 2)
  expect_equal(attr(r, "theorem"), "Research.P7.ZeroInflation.nb_matches_zip")
  expect_equal(r$mean, 0.7 * 2)
  expect_equal(r$zip_zero, 0.3 + 0.7 * exp(-2))
  expect_gt(r$zip_zero, exp(-r$mean))  # zip_zero_gt_poisson
  expect_lt(r$zip_zero, 1)             # zip_zero_lt_one
  expect_equal(r$nb_zero, r$zip_zero, tolerance = 1e-9)
  expect_equal(stats::dnbinom(0, size = r$theta, mu = r$mean), r$zip_zero, tolerance = 1e-9)
  expect_error(morie_nb_matches_zip(1, 2), "in \\(0, 1\\)")
  expect_error(morie_nb_matches_zip(0.3, 0), "positive")
})

test_that("Gamma bound: worst mean monotone, Gamma* kills the evidence, table only", {
  r <- morie_gamma_bound(T = 70, n = 100, Gamma = c(1, 1.5, 2, 7 / 3, 3))
  expect_equal(attr(r, "theorem"), "Research.P5.Gamma.gamma_star_kills")
  expect_equal(r$gamma_star, 70 / 30)
  expect_true(all(diff(r$worst$worst_mean) >= 0))                       # worst_mean_mono
  expect_equal(r$worst$worst_mean[4], 70)                                # worst_mean_at_gamma_star
  expect_equal(r$worst$survives, c(TRUE, TRUE, TRUE, FALSE, FALSE))      # below / kills
  expect_equal(morie_gamma_bound(70, 100)$gamma_star, r$gamma_star)      # table only
  expect_error(morie_gamma_bound(100, 100), "0 <= T < n")
})

test_that("blocking crossover is positive and classifies rho correctly", {
  r <- morie_blocking_crossover(delta = 0.5, sigma = 1, n = 10, rho = c(0.02, 0.3, 0.9))
  expect_equal(attr(r, "theorem"), "Research.P3.Blocking.paired_better_iff")
  expect_gt(r$c_paired, r$c_unpaired)
  expect_gt(r$crossover, 0)                                               # crossover_pos
  expect_equal(r$crossover, 1 - 1 / (1 + (r$c_paired - r$c_unpaired) / r$k)^2)
  expect_equal(r$paired_better, c(0.02, 0.3, 0.9) > r$crossover)          # paired_better_iff
  expect_equal(r$k, 0.5 * sqrt(5))
  expect_error(morie_blocking_crossover(0, 1, 10), "positive")
  expect_error(morie_blocking_crossover(1, 1, 10, rho = 1), "below 1")
})

test_that("adjustment variances: Lin never worse, balanced never hurts, b0 = 0 hurts iff p > 2/3", {
  r <- morie_adjustment_variances(b1 = 1, b0 = 0, v1 = 1, v0 = 1, p = 0.8)
  expect_equal(attr(r, "theorem"), "Research.P3.Adjustment.adjusted_minus_unadjusted")
  expect_true(r$adjustment_hurts)                                         # adjusted_worse
  expect_equal(r$adjusted_minus_unadjusted, (0.2^2 - 1) / 0.8 + (0.8^2 - 0) / 0.2)
  expect_equal(r$adjusted_minus_unadjusted, 1 * (3 * 0.8 - 2) / (1 - 0.8))
  expect_false(morie_adjustment_variances(1, 0, 1, 1, 0.6)$adjustment_hurts)
  for (p in c(0.3, 0.5, 0.7)) {
    s <- morie_adjustment_variances(1.2, -0.4, 0.5, 2, p)
    expect_lte(s$lin, s$unadjusted)                                       # lin_le_unadjusted
    expect_lte(s$lin, s$adjusted)                                         # lin_le_adjusted
  }
  expect_lte(morie_adjustment_variances(1.2, -0.4, 0.5, 2, 0.5)$adjusted,
             morie_adjustment_variances(1.2, -0.4, 0.5, 2, 0.5)$unadjusted)  # balanced
  expect_error(morie_adjustment_variances(1, 0, 1, 1, 1), "in \\(0, 1\\)")
})

test_that("budget optimum is interior for concave responses that beat both corners", {
  r <- morie_budget_optimum(g = sqrt, d = function(s) 2 * log1p(s), C = 10)
  expect_equal(attr(r, "theorem"), "Research.P8.Budget.interior_optimum")
  expect_true(r$interior)
  expect_gt(r$value, r$corner_values[["stick"]])
  expect_gt(r$value, r$corner_values[["carrot"]])
  # concave_sum: midpoint inequality along the grid
  f <- r$grid$objective
  expect_true(all(f[-c(1, length(f))] >= (f[-c(length(f) - 1, length(f))] + f[-(1:2)]) / 2 - 1e-12))
  # a linear deterrence response dominating: corner optimum
  c0 <- morie_budget_optimum(g = function(t) 0.01 * t, d = function(s) s, C = 1)
  expect_false(c0$interior)
  expect_error(morie_budget_optimum(sqrt, sqrt, 0), "positive")
})

test_that("outcome test: infra-marginality and the uniform case", {
  r <- morie_outcome_test(list(A = list(q = c(0.3, 0.9), w = c(1, 1), t = 0.5), B = list(q = 0.9, w = 1, t = 0.2)))
  expect_equal(attr(r, "theorem"), "Research.P2.OutcomeTest.infra_marginality")
  expect_equal(r$hit_rate, c(0.9, 0.9))
  expect_equal(r$stop_rate, c(0.5, 1))
  expect_false(r$threshold[1] == r$threshold[2])
  s <- morie_outcome_test(list(A = list(q = c(0.3, 0.9), w = c(1, 1), t = 0.5), B2 = list(q = c(0.6, 0.9), w = c(1, 1), t = 0.5)))
  expect_false(s$hit_rate[1] == s$hit_rate[2])                             # equal_thresholds_unequal_hits
  # uniform_hit_rate: E[q | q > t] = (1 + t)/2 on a fine uniform grid
  q <- seq(0.0005, 0.9995, by = 0.001)
  u <- morie_outcome_test(list(U = list(q = q, w = rep(1, length(q)), t = 0.4)))
  expect_equal(u$hit_rate, (1 + 0.4) / 2, tolerance = 1e-3)
  expect_error(morie_outcome_test(list(list(q = 0.5, w = 1, t = 0.1))), "named list")
  expect_error(morie_outcome_test(list(A = list(q = 2, w = 1, t = 0.1))), "q in")
})

test_that("spatial omission: lag covariance, GLS beats OLS, FWER", {
  r <- morie_spatial_omission(rho = 0.4, x = c(1, 2), Sigma = matrix(c(1, 0.5, 0.5, 1), 2), alpha = 0.05, m = 50)
  expect_equal(attr(r, "theorem"), "Research.P3.Spatial.lag_regressor_correlated")
  expect_equal(r$lag_covariance, 0.4 / (1 - 0.16))
  # lag_regressor_correlated: average of y2 u1 over the four draws
  y2 <- function(u1, u2) (u2 + 0.4 * u1) / (1 - 0.16)
  expect_equal((y2(1, 1) * 1 + y2(1, -1) * 1 + y2(-1, 1) * (-1) + y2(-1, -1) * (-1)) / 4, r$lag_covariance)
  expect_equal(r$var_ols, 7 / 25)                                          # gls_more_precise
  expect_equal(r$var_gls, 1 / 4)
  expect_true(r$gls_more_precise)
  expect_equal(r$fwer, 1 - 0.95^50)                                        # fwer_tendsto_one
  expect_gt(morie_spatial_omission(0.1, c(1, 2), diag(2), 0.05, 5000)$fwer, 0.999)
  expect_error(morie_spatial_omission(1, c(1, 2), diag(2)), "differ from 1")
})

test_that("natural IV bounds: ends attained, width P(A=0|Z=1)+P(A=1|Z=0)", {
  r <- morie_iv_natural_bounds(p11 = 0.8, p10 = 0.3, m1 = 0.6, m0 = 0.4)
  expect_equal(attr(r, "theorem"), "Research.P8.NaturalBounds.width")
  expect_equal(r$width, 0.2 + 0.3)
  ate <- function(q1, q0) (0.6 * 0.8 + q1 * 0.2) - (0.4 * 0.7 + q0 * 0.3)
  expect_equal(ate(0, 1), r$lower)                                         # bounds_attained
  expect_equal(ate(1, 0), r$upper)
  for (q1 in c(0, 0.5, 1)) for (q0 in c(0, 0.5, 1)) {
    expect_gte(ate(q1, q0), r$lower - 1e-12); expect_lte(ate(q1, q0), r$upper + 1e-12)   # ate_mem
  }
  pc <- morie_iv_natural_bounds(1, 0, 0.6, 0.4)
  expect_equal(pc$width, 0); expect_equal(pc$upper, 0.2)                   # perfect_compliance_point
  expect_equal(morie_iv_natural_bounds(0.5, 0.5, 0.6, 0.4)$width, 1)       # half_noncompliance_width
  expect_error(morie_iv_natural_bounds(1.2, 0, 0, 0), "lie in")
})

test_that("design sensitivity: unique root and the 0/1 limiting power", {
  r <- morie_design_sensitivity(mu = 0.7)
  expect_equal(attr(r, "theorem"), "Research.P5.DesignSensitivity.design_sensitivity_exists_unique")
  expect_equal(r$gamma_tilde, 0.7 / 0.3, tolerance = 1e-8)
  expect_equal(r$limiting_power$power, c(1, 1, 0))                     # eventually_reject / accept
  expect_error(morie_design_sensitivity(0.4), "no effect")
  expect_error(morie_design_sensitivity(0.7, upper = 2), "too small")
})

test_that("Moran's I: trend witness and residual trace ratio", {
  W <- matrix(0, 4, 4); W[cbind(1:3, 2:4)] <- 1; W <- W + t(W)
  r <- morie_moran_trend(W, y = 1:4)
  expect_equal(attr(r, "theorem"), "Research.P7.MoranTrend.residual_moran_ratio")
  expect_equal(r$I, 1 / 3)                                                 # trend_witness
  expect_equal(r$null_expectation, -1 / 3)
  set.seed(5)
  n <- 30
  Wn <- matrix(0, n, n); Wn[cbind(1:(n - 1), 2:n)] <- 1; Wn <- Wn + t(Wn)
  X <- cbind(1, seq_len(n))
  y <- 2 + 0.5 * seq_len(n) + stats::rnorm(n)
  rr <- morie_moran_trend(Wn, y, X)
  M <- diag(n) - X %*% solve(crossprod(X)) %*% t(X)
  expect_equal(rr$residual_expectation, (n / sum(Wn)) * sum(diag(Wn %*% M)) / (n - 2))
  # residual_moran_ratio: averaged over draws, numerator and denominator sums match c tr(WM), c tr(M)
  E <- matrix(stats::rnorm(n * 4000), n)
  num <- mean(apply(E, 2, function(e) { me <- M %*% e; as.numeric(t(me) %*% Wn %*% me) }))
  den <- mean(apply(E, 2, function(e) { me <- M %*% e; sum(me^2) }))
  expect_equal(num, sum(diag(Wn %*% M)), tolerance = 0.1)
  expect_equal(den, n - 2, tolerance = 0.05)
  expect_error(morie_moran_trend(W, 1:5), "n x n")
})

test_that("exposure positivity and permutation exactness", {
  e <- morie_exposure_positivity(p = 0.5, degrees = c(1, 5, 10, 30))
  expect_equal(attr(e, "theorem"), "Research.P3.Exposure.exposure_prob_tendsto_zero")
  expect_equal(e$prob_all_treated, 0.5^c(1, 5, 10, 30))
  expect_equal(e$ht_weight, 2^c(1, 5, 10, 30))
  expect_true(all(diff(e$prob_all_treated) < 0))
  expect_error(morie_exposure_positivity(1, 1), "in \\(0, 1\\)")
  set.seed(2)
  stats <- stats::rnorm(200)
  pv <- morie_permutation_valid(stats, alpha = c(0.01, 0.05, 0.1, 0.5))
  expect_equal(attr(pv, "theorem"), "Research.P3.Exposure.permutation_valid")
  expect_true(all(pv$levels$valid))                                        # permutation_valid
  expect_equal(pv$pvalues[which.max(stats)], 1 / 200)
  tied <- morie_permutation_valid(c(1, 1, 1, 2), alpha = 0.5)
  expect_equal(tied$levels$rejection_share, 0.25)
  expect_true(tied$levels$valid)
  expect_error(morie_permutation_valid(numeric(0)), "non-empty")
})

test_that("thinning: pgf identity, non-identification, Chernoff bound", {
  t <- morie_thinning_identification(lambda = 5, gamma = 0.3)
  expect_equal(attr(t, "theorem"), "Research.P1.Thinning.not_identified")
  expect_equal(t$recorded_rate, 1.5)
  expect_true(t$pgf_check)                                                 # thinned_pgf
  expect_equal(unname(t$alternative[["lambda"]] * t$alternative[["gamma"]]), 1.5)
  expect_false(t$alternative[["lambda"]] == 5)
  # joint_pgf_factorises
  s <- 0.4; u <- 0.7
  expect_equal(exp(5 * ((0.3 * s + 0.7 * u) - 1)), exp(1.5 * (s - 1)) * exp(3.5 * (u - 1)))
  expect_error(morie_thinning_identification(5, 1), "in \\(0, 1\\)")
  c <- morie_poisson_chernoff(lambda = 20, eps = c(0.25, 0.5, 1))
  expect_equal(attr(c, "theorem"), "Research.P7.Chernoff.poisson_chernoff")
  expect_true(all(c$rate > 0))                                             # rate_pos
  expect_true(all(c$exact <= c$bound))                                     # poisson_chernoff
  expect_equal(c$bound, exp(-20 * ((1 + c$eps) * log(1 + c$eps) - c$eps)))
  expect_error(morie_poisson_chernoff(0, 1), "positive")
})

test_that("random-walk exposure: degree law stationary on undirected graphs, not on a directed one", {
  adj <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0), 4)
  r <- morie_random_walk_exposure(adj)
  expect_equal(attr(r, "theorem"), "Research.P3.RandomWalk.stationary")
  expect_true(r$undirected)
  expect_true(r$stationary)                                                # stationary
  expect_equal(r$pi, rowSums(adj) / sum(adj))                              # pi = d / (2|E|)
  expect_equal(sum(r$pi), 1)                                               # stationary_sum_one
  dir <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0), 3, byrow = TRUE)
  d <- morie_random_walk_exposure(dir)
  expect_false(d$undirected)
  expect_false(d$stationary)                                               # directed_fails
  expect_error(morie_random_walk_exposure(matrix(0, 2, 2)), "out-edge")
})

test_that("extremal index: cluster survival tends to exp(-tau/m), theta = 1/m", {
  r <- morie_extremal_index_clusters(m = 4, tau = 2, n = c(10, 100, 1e5))
  expect_equal(attr(r, "theorem"), "Research.P10.ExtremalIndex.extremal_index")
  expect_equal(r$theta, 1 / 4)
  expect_equal(r$limit_cluster^4, r$limit_iid)                             # extremal_index
  expect_equal(r$table$cluster[3], exp(-0.5), tolerance = 1e-4)           # cluster_tendsto
  expect_equal(r$table$iid[3], exp(-2), tolerance = 1e-4)                 # iid_tendsto
  expect_error(morie_extremal_index_clusters(0, 1), "at least 1")
})

test_that("tail ratios: geometric to zero, power constant, NB ratio under its bound and to zero", {
  r <- morie_tail_ratio(k = c(5, 10, 20, 40), q = 0.7, alpha = 1.5, r = 3)
  expect_equal(attr(r, "theorem"), "Research.P7.TailShape.nb_pmf_ratio_tendsto_zero")
  expect_equal(r$geometric, 0.7^c(5, 10, 20, 40))                         # geometric_ratio_tendsto_zero
  expect_equal(r$power, rep(2^(-1.5), 4))                                  # power_ratio_const
  expect_true(all(r$nb_pmf_ratio <= r$nb_bound))                           # nb_pmf_ratio_le
  expect_true(all(diff(r$nb_pmf_ratio) < 0)); expect_lt(r$nb_pmf_ratio[4], 1e-3)
  x <- c(rep(1, 50), rep(3, 30), rep(8, 15), 20, 25, 30, 40, 60)
  e <- morie_tail_ratio(k = c(2, 5), counts = x)
  expect_equal(e$empirical[1], mean(x > 4) / mean(x > 2))
  expect_error(morie_tail_ratio(0), "at least 1")
})

test_that("calibration flip incompatibility and the Bayes-risk data-processing inequality", {
  r <- morie_calibration_flip(fA = 0.05, fB = 0.2, r = c(0.3, 0.5, 0.8))
  expect_equal(attr(r, "theorem"), "Research.P5.DataProcessing.calibration_incompatible")
  expect_equal(0.05 + 0.9 * r$true_A, r$r)                                 # calibrated_to_D_true_rate
  expect_equal(r$incompatible, c(TRUE, FALSE, TRUE))
  expect_false(any(r$true_A[c(1, 3)] == r$true_B[c(1, 3)]))                # calibration_incompatible
  expect_false(any(r$recorded_A[c(1, 3)] == r$recorded_B[c(1, 3)]))        # calibration_to_W_incompatible
  expect_equal(r$true_A[2], r$true_B[2])
  expect_error(morie_calibration_flip(0.5, 0.1, 0.3), "no information")
  mu <- matrix(c(0.3, 0.1, 0.1, 0.2, 0.05, 0.25), 2)
  d <- morie_bayes_risk_dpi(mu, w = c(0, 1), h = c(1, 1, 2))
  expect_equal(attr(d, "theorem"), "Research.P5.DataProcessing.bayes_risk_data_processing")
  expect_true(d$dpi_holds)
  expect_lte(d$risk_D, d$risk_R)
  expect_equal(morie_bayes_risk_dpi(mu, c(0, 1), c(1, 2, 3))$risk_R, d$risk_D)  # identity post-processing
  expect_error(morie_bayes_risk_dpi(mu, c(0, 1), c(1, 1)), "indexes")
})

test_that("Bellman: contraction ratio within gamma, fixed point, and the myopic example", {
  r <- matrix(c(1, 0.5, 1, 0, 0.5, 1), nrow = 3)
  p <- array(0, c(3, 2, 3)); p[1, 1, 2] <- 1; p[1, 2, 3] <- 1; p[2, , 2] <- 1; p[3, , 3] <- 1
  b <- morie_bellman_value(r, p, gamma = 0.9)
  expect_equal(attr(b, "theorem"), "Research.P4.Bellman.bellman_contracting")
  expect_true(b$converged)
  expect_lte(b$contraction_ratio, 0.9 + 1e-6)                              # bellman_lipschitz
  expect_equal(b$value[2], 0.5 / 0.1, tolerance = 1e-6)
  expect_equal(b$value[3], 1 / 0.1, tolerance = 1e-6)
  expect_equal(b$value[1], 0.9 * 10, tolerance = 1e-6)                     # patient action optimal at 0.9
  expect_equal(b$policy[1], 2L)
  # optimal_value_exists_unique: the value is a fixed point of the operator
  q <- r + 0.9 * apply(p, c(1, 2), function(pr) sum(pr * b$value))
  expect_equal(apply(q, 1, max), b$value, tolerance = 1e-6)
  m <- morie_myopic_example(gamma = c(0.5, 2 / 3, 0.9))
  expect_equal(attr(m, "theorem"), "Research.P4.Bellman.myopic_suboptimal")
  expect_equal(m$patient_better, c(FALSE, FALSE, TRUE))                    # myopic_suboptimal
  expect_equal(m$greedy, 1 + c(0.5, 2 / 3, 0.9) * 0.5 / (1 - c(0.5, 2 / 3, 0.9)))
  expect_error(morie_bellman_value(r, p, 1), "lie in")
  expect_error(morie_bellman_value(r, p * 2, 0.5), "probability vectors")
})

test_that("budget rule: threshold optimal over fractional rules, stochastic needed at the tie", {
  r <- morie_budget_rule(m = c(1, 1, 1), tau = c(0.5, 0.2, -0.1), kappa = 1.5)
  expect_equal(attr(r, "theorem"), "Research.P5.BudgetRule.threshold_optimal")
  expect_equal(r$rule, c(1, 0.5, 0))
  expect_equal(r$value, 0.5 + 0.1)
  expect_equal(r$threshold, 0.2)
  expect_true(r$stochastic_needed)                                         # stochastic_needed
  expect_equal(r$best_deterministic, 0.5)
  # threshold_optimal: random feasible fractional rules never beat it
  set.seed(9)
  for (i in 1:200) {
    d <- stats::runif(3); if (sum(d) > 1.5) d <- d * 1.5 / sum(d)
    expect_lte(sum(d * c(0.5, 0.2, -0.1)), r$value + 1e-12)
  }
  nt <- morie_budget_rule(m = c(2, 1), tau = c(0.3, 0.4), kappa = 3)
  expect_equal(nt$rule, c(1, 1)); expect_false(nt$stochastic_needed)
  expect_error(morie_budget_rule(c(0, 1), c(1, 1), 1), "positive")
})

test_that("run-length bound: partial sum at least J/2", {
  r <- morie_run_length_bound(alpha = 0.01)
  expect_equal(attr(r, "theorem"), "Research.P10.Cusum.mean_run_length_ge")
  expect_equal(r$J, 100)
  expect_equal(r$partial_sum, sum(1 - (0:99) * 0.01))                     # partial_sum_ge_half
  expect_gte(r$partial_sum, r$bound)
  expect_equal(r$bound, 50)
  # stop_prob_le: restarted copies with P(tau < inf) <= alpha give P(N <= n) <= (n + 1) alpha
  set.seed(4)
  alpha <- 0.05; reps <- 4000
  N <- replicate(reps, { k <- 0; repeat { if (stats::runif(1) < alpha) break; k <- k + 1 }; k })
  for (n in c(0, 3, 10)) expect_lte(mean(N <= n), (n + 1) * alpha + 0.03)
  expect_error(morie_run_length_bound(0), "in \\(0, 1\\]")
  expect_error(morie_run_length_bound(0.5, J = 3), "must not exceed 1")
})
