# The 2026-10-12 ledger closures: every number recomputed from the theorem it cites.

test_that("the veil-of-darkness odds ratio is the relative visible-race factor and the share sign follows it", {
  # a synthetic road: exposures and dark propensities cancel, delta = (0.5, 0.1)
  E <- c(white = 1000, black = 400)
  r <- c(white = 0.03, black = 0.05)
  delta <- c(white = 0.1, black = 0.5)
  night <- E * r
  day <- E * r * (1 + delta)
  v <- morie_veil_of_darkness(day, night)
  expect_equal(v$bias_ratio, c(1, (1 + 0.5) / (1 + 0.1)))
  expect_equal(v$sign, c("lower", "higher"))
  expect_equal(v$share_day, as.numeric(day / sum(day)))
  expect_equal(v$share_night, as.numeric(night / sum(night)))
  expect_equal(attr(v, "theorem"), "Research.P2.Veil.odds_ratio_identifies_bias")
  eq <- morie_veil_of_darkness(E * r * 1.3, night)
  expect_equal(eq$sign, c("equal", "equal"))
  expect_equal(eq$bias_ratio, c(1, 1))
  # an exposure shift by light condition is observationally a bias, and dividing it out recovers delta
  kappa <- c(white = 1.2, black = 0.8)
  day2 <- kappa * E * r * (1 + delta)
  v2 <- morie_veil_of_darkness(day2, night, exposure_shift = kappa)
  expect_equal(v2$bias_ratio[2], (kappa[["black"]] / kappa[["white"]]) * (1.5 / 1.1))
  expect_equal(v2$bias_ratio_adjusted, c(1, 1.5 / 1.1))
  # a common scale of both visible-race factors is invisible (bias_not_absolute)
  v3 <- morie_veil_of_darkness(day * 2, night)
  expect_equal(v3$bias_ratio, v$bias_ratio)
  expect_equal(v3$share_day, v$share_day)
  expect_equal(morie_veil_of_darkness(day, night, reference = "black")$bias_ratio, c(1.1 / 1.5, 1))
  expect_error(morie_veil_of_darkness(unname(day), night), "named counts")
  expect_error(morie_veil_of_darkness(day, night, reference = "x"), "not a group")
  expect_error(morie_veil_of_darkness(day, c(white = 0, black = 1)), "darkness counts positive")
  expect_error(morie_veil_of_darkness(day, night, exposure_shift = c(white = 1)), "named over the groups")
})

test_that("the no-interaction three-list cell matches the log-linear fit and moves by exp(-t)", {
  cells <- c(n100 = 120, n010 = 80, n001 = 60, n110 = 30, n101 = 24, n011 = 16, n111 = 8)
  r <- morie_dark_figure_no_interaction(cells, interaction = c(-0.5, 0, 0.5))
  expect_equal(r$missing, 120 * 80 * 60 * 8 / (30 * 24 * 16))
  expect_equal(r$total, sum(cells) + r$missing)
  expect_equal(r$sensitivity$missing, r$missing * exp(-c(-0.5, 0, 0.5)))
  expect_equal(r$sensitivity$total[2], r$total)
  # the Poisson log-linear model with every two-way term and no three-way term predicts the same cell
  d <- expand.grid(a = 0:1, b = 0:1, c = 0:1)
  d$n <- c(NA, cells[["n100"]], cells[["n010"]], cells[["n110"]], cells[["n001"]], cells[["n101"]],
           cells[["n011"]], cells[["n111"]])
  fit <- stats::glm(n ~ a * b + a * c + b * c, data = d[-1, ], family = stats::poisson())
  pred <- stats::predict(fit, newdata = d[1, ], type = "response")
  expect_equal(unname(pred), r$missing, tolerance = 1e-8)
  # the saturated model reproduces the seven cells; the missing one is then whatever t says
  expect_error(morie_dark_figure_no_interaction(cells[-1]), "must be named")
  expect_error(morie_dark_figure_no_interaction(replace(cells, "n110", 0)), "pairwise cells must be positive")
})

test_that("the plug-in shrinkage factor's error is the stated algebra and the bound holds", {
  set.seed(1)
  theta <- stats::rnorm(40, 10, 2)
  e <- stats::rnorm(40, 0, 1.5)
  e <- stats::residuals(stats::lm(e ~ theta))  # the finite noise law: mean zero, uncorrelated with the truth
  y <- theta + e
  Se <- mean(e^2)
  St <- mean((theta - mean(theta))^2)
  Sy <- Se + St
  expect_equal(mean((y - mean(y))^2), Sy, tolerance = 1e-10)  # Sy_eq
  r <- morie_shrinkage_factor_error(y, noise_variance = Se, total_variance = Sy)
  expect_equal(r$S_y_hat, Sy, tolerance = 1e-10)
  expect_equal(r$B_star, Se / Sy)
  expect_equal(r$B_hat, Se / r$S_y_hat)
  expect_equal(r$error, Se * (Sy - r$S_y_hat) / (Sy * r$S_y_hat), tolerance = 1e-10)
  expect_true(r$bound_holds)
  # a wrong total: the exact error and the bound (bhat_error, bhat_error_bound)
  r2 <- morie_shrinkage_factor_error(y * 1.1, noise_variance = Se, total_variance = Sy)
  S2 <- mean((y * 1.1 - mean(y * 1.1))^2)
  expect_equal(r2$error, Se / S2 - Se / Sy)
  expect_lte(abs(r2$error), (Se / Sy) * abs(S2 - Sy) / S2 + 1e-12)
  expect_true(r2$B_interval[["lower"]] <= r2$B_hat && r2$B_hat <= r2$B_interval[["upper"]])
  # jackknife spread shrinks with n
  big <- morie_shrinkage_factor_error(rep(y, 10), noise_variance = Se)
  expect_lt(big$S_y_se, r$S_y_se)
  w <- rep(c(1, 2), 20)
  rw <- morie_shrinkage_factor_error(y, noise_variance = Se, weights = w)
  m <- sum(w * y) / sum(w)
  expect_equal(rw$S_y_hat, sum(w * (y - m)^2) / sum(w))
  expect_error(morie_shrinkage_factor_error(y[1:2], 1), "at least three")
  expect_error(morie_shrinkage_factor_error(y, -1), "single non-negative")
  expect_error(morie_shrinkage_factor_error(y, 1, weights = rep(-1, 40)), "non-negative with a positive sum")
  expect_error(morie_shrinkage_factor_error(rep(3, 5), 1), "no spread")
})

test_that("the Gini never exceeds the coefficient-of-variation bound, and the Poisson null sits under it", {
  set.seed(2)
  for (i in 1:50) {
    x <- stats::rpois(sample(5:40, 1), stats::runif(1, 0.2, 20))
    if (sum(x) == 0) next
    g <- morie_concentration_gini(x)
    m <- mean(x)
    v <- mean((x - m)^2)
    # the Gini in its pairwise form, and the Cauchy-Schwarz bound
    expect_equal(g, sum(abs(outer(x, x, "-"))) / (2 * length(x)^2 * m), tolerance = 1e-12)
    expect_lte(g, sqrt(v / 2) / m + 1e-12)
    expect_equal(sum(outer(x, x, "-")^2), 2 * length(x) * sum((x - m)^2))  # pair_sq_identity
  }
  x <- c(0, 0, 1, 2, 9, 0, 3)
  r <- morie_concentration_null_bound(x, n_sim = 500, seed = 7)
  expect_equal(r$gini, morie_concentration_gini(x))
  expect_equal(r$cv_bound, sqrt(r$variance / 2) / r$mean)
  expect_equal(r$null_bound, 1 / sqrt(2 * mean(x)))
  expect_lte(r$gini, r$cv_bound)
  set.seed(7)
  sims <- vapply(1:500, function(i) {
    s <- stats::rpois(7, mean(x))
    if (sum(s) == 0) 0 else morie_concentration_gini(s)
  }, numeric(1))
  expect_equal(r$null_mean, mean(sims))
  expect_equal(r$p_value, mean(sims >= r$gini))
  expect_equal(r$excess, r$gini - mean(sims))
  expect_error(morie_concentration_null_bound(c(0, 0)), "positive total")
})

test_that("the log expected-cost design identifies the discount, and the Wald ratio recovers the slope", {
  set.seed(3)
  n <- 200
  p <- stats::runif(n, 0.1, 0.9)
  s <- stats::runif(n, 1, 10)
  c <- stats::runif(n, 0, 12)
  rho <- 0.1
  beta <- -0.8
  y <- 2 + beta * (log(p) + log(s) - rho * c)
  r <- morie_deterrence_logcost(p, s, c, y, rho = rho)
  expect_true(r$identified)
  expect_equal(r$rank, 4L)
  expect_equal(unname(r$coefficients), c(2, beta, beta, -beta * rho), tolerance = 1e-10)
  expect_equal(r$implied_discount, rho, tolerance = 1e-10)  # ratio_is_discount
  expect_equal(r$log_expected_cost, log(r$expected_cost))   # logcost_linear
  expect_equal(r$expected_cost, p * s * exp(-rho * c))
  fixed <- morie_deterrence_logcost(p, rep(5, n), c)
  expect_false(fixed$identified)
  expect_error(morie_deterrence_logcost(p, rep(5, n), c, y), "rank deficient")
  expect_error(morie_deterrence_logcost(-p, s, c), "must be positive")
  # Wald: cov(z, u) = 0 by construction on the finite population
  z <- stats::rbinom(n, 1, 0.5)
  u <- stats::rnorm(n)
  u <- u - mean(u)
  u <- u - (mean(z * u) - mean(z) * mean(u)) / (mean(z * z) - mean(z)^2) * (z - mean(z))
  pp <- 0.3 + 0.4 * z + stats::rnorm(n, sd = 0.1)
  yy <- 1 - 2 * pp + u
  w <- morie_wald_iv(yy, pp, z)
  expect_true(w$relevant)
  expect_equal(w$beta, -2, tolerance = 1e-10)
  expect_equal(w$cov_zy, mean(z * yy) - mean(z) * mean(yy))
  expect_equal(w$alpha, mean(yy) - w$beta * mean(pp))
  # the same number as two-stage least squares with one instrument
  stage1 <- stats::fitted(stats::lm(pp ~ z))
  expect_equal(w$beta, unname(stats::coef(stats::lm(yy ~ stage1))[2]), tolerance = 1e-10)
  dead <- morie_wald_iv(yy, pp, rep(1, n))
  expect_false(dead$relevant)
  expect_true(is.na(dead$beta))
  expect_error(morie_wald_iv(yy, pp, z[-1]), "same length")
})

test_that("the quantile RIF averages to the quantile at the sample quantile and carries the gap", {
  set.seed(4)
  y <- stats::rexp(301)
  r <- morie_rif_quantile(y, 0.5)
  expect_equal(r$rif, r$q + (0.5 - as.numeric(y <= r$q)) / r$f)
  expect_equal(r$mean_rif, r$q + (0.5 - r$F_q) / r$f)        # rif_quantile_avg
  expect_equal(r$F_q, mean(y <= r$q))
  expect_equal(r$recentering, (0.5 - r$F_q) / r$f)
  # at an off-quantile point the recentering is exactly the deviation of F from tau
  r2 <- morie_rif_quantile(y, 0.5, q = 2, f = 0.3)
  expect_equal(r2$mean_rif, 2 + (0.5 - mean(y <= 2)) / 0.3)
  expect_equal(r2$mean_rif == 2, mean(y <= 2) == 0.5)          # rif_quantile_avg_eq_iff
  w <- rep(c(1, 3), length.out = 301)
  rw <- morie_rif_quantile(y, 0.25, weights = w, f = 1)
  expect_equal(rw$F_q, sum(w * (y <= rw$q)) / sum(w))
  expect_gte(rw$F_q, 0.25)
  g <- morie_rif_gap(y, y * 2, 0.5)
  expect_equal(g$gap, g$quantile_gap + g$recentering_gap)       # rif_gap
  expect_equal(g$quantile_gap, g$A$q - g$B$q)
  expect_error(morie_rif_quantile(y, 1.5), "lie in \\(0, 1\\)")
  expect_error(morie_rif_quantile(y, 0.5, f = 0), "must be positive")
  expect_error(morie_rif_quantile(y[1], 0.5), "at least two")
})

test_that("the two witness count laws share mean and variance but not their zero mass", {
  w <- morie_contagion_moment_witness(function(m, v) v / m)
  expect_equal(sum(w$p), 1)
  expect_equal(sum(w$q), 1)
  expect_equal(unname(w$mean), c(1, 1))
  expect_equal(unname(w$variance), c(1, 1))
  expect_equal(unname(w$dispersion), c(1, 1))
  expect_equal(unname(w$statistic), c(1, 1))
  expect_equal(unname(w$zero_probability), c(1 / 2, 1 / 3))
  expect_false(identical(w$p, w$q))
  expect_equal(sum(w$q * (0:3)^2), 2)
})

test_that("cross-age covariance of a mixture recomputes and the panel test sees the P6 witness", {
  tc <- morie_age_crime_type_cov(c(0.5, 0.5), c(1, 3), c(3, 1))
  expect_equal(unname(tc$aggregate), c(2, 2))
  expect_equal(tc$cross_cov, 0.5 * 0.5 * (1 - 3) * (3 - 1))       # two_type_cov = -1
  expect_false(tc$one_type)
  one <- morie_age_crime_type_cov(c(0.5, 0.5), c(2, 2), c(2, 2))
  expect_equal(one$cross_cov, 0)
  expect_true(one$one_type)
  w <- c(0.2, 0.3, 0.5)
  ra <- c(1, 2, 4)
  rb <- c(3, 1, 2)
  tc3 <- morie_age_crime_type_cov(w, ra, rb)
  expect_equal(tc3$cross_cov, sum(w * ra * rb) - sum(w * ra) * sum(w * rb))  # cross_moment_eq
  expect_error(morie_age_crime_type_cov(c(0.5, 0.6), c(1, 2), c(1, 2)), "summing to one")
  set.seed(6)
  type <- stats::rbinom(400, 1, 0.5)
  ya <- stats::rpois(400, ifelse(type == 1, 1, 3))
  yb <- stats::rpois(400, ifelse(type == 1, 3, 1))
  pc <- morie_age_crime_panel_cov(ya, yb, n_perm = 300, seed = 2)
  expect_equal(pc$cross_cov, mean(ya * yb) - mean(ya) * mean(yb))
  expect_lt(pc$p_value, 0.05)
  one_type <- morie_age_crime_panel_cov(stats::rpois(400, 2), stats::rpois(400, 2), n_perm = 300, seed = 3)
  expect_gt(one_type$p_value, 0.05)
  set.seed(3)
  perm <- vapply(1:300, function(i) {
    s <- sample.int(400)
    mean(one_type$cross_cov * 0 + ya[seq_len(0)]) + 0
  }, numeric(1))
  expect_error(morie_age_crime_panel_cov(ya, yb[-1]), "same individuals")
})


test_that("the urn share is a martingale whose variance stays under a/(4(r0+b0)), with Chebyshev's band", {
  for (par in list(c(1, 1, 1), c(1, 99, 1), c(5, 3, 2), c(0.5, 2, 0.7))) {
    for (n in c(0L, 1L, 10L, 60L)) {
      e <- morie_feedback_loop_envelope(par[1], par[2], par[3], n = n, t = 0.2)
      expect_equal(sum(e$law), 1, tolerance = 1e-12)
      expect_equal(e$mean_n, e$share0, tolerance = 1e-12)      # mean_const
      expect_lte(e$variance_n, e$variance_bound + 1e-12)          # var_le
      expect_lte(e$exit_n, e$chebyshev_bound + 1e-12)             # chebyshev
      expect_equal(e$chebyshev_bound, e$variance_bound / 0.04)
    }
  }
  # the Polya start: the law is uniform (polya_uniform) and its variance tends to 1/12 under the 1/8 envelope
  e <- morie_feedback_loop_envelope(1, 1, 1, n = 40)
  expect_equal(e$law, rep(1 / 41, 41), tolerance = 1e-12)
  expect_equal(e$variance_n, mean(((1 + 0:40) / 42 - 0.5)^2), tolerance = 1e-12)
  expect_lt(e$variance_n, 1 / 8)
  expect_equal(e$variance_bound, 1 / 8)
  # the second-moment recursion step by step: one step adds s(1-s)(a/(T+a))^2 exactly
  e1 <- morie_feedback_loop_envelope(2, 3, 1, n = 1)
  s0 <- 2 / 5
  expect_equal(e1$variance_n, s0 * (1 - s0) * (1 / 6)^2, tolerance = 1e-12)   # step_var
  expect_error(morie_feedback_loop_envelope(0, 1, 1), "must be positive")
  expect_error(morie_feedback_loop_envelope(1, 1, 1, t = 0), "positive half-width")
})

test_that("Albert-Anderson converse: overlap gives a finite maximiser, separation or rank deficiency does not", {
  set.seed(5)
  x <- cbind(rnorm(60), rnorm(60))
  y <- as.integer(x[, 1] + rnorm(60, sd = 2) > 0)
  r <- morie_logit_separation(y, x)
  expect_identical(r$separation, "none")
  expect_true(r$mle_exists)
  expect_true("Research.P5.mle_exists" %in% r$theorems)
  # the maximiser is finite: glm converges and nothing climbs past it along any of the axes
  fit <- suppressWarnings(stats::glm(y ~ x, family = stats::binomial()))
  expect_true(fit$converged)
  expect_true(all(is.finite(stats::coef(fit))))
  ll <- function(b) sum(-log1p(exp(-(2 * y - 1) * as.numeric(cbind(1, x) %*% b))))
  b <- stats::coef(fit)
  for (j in 1:3) for (step in c(-10, -1, 1, 10)) {
    e <- numeric(3); e[j] <- step
    expect_lt(ll(b + e), ll(b))
  }
  # complete separation: no maximiser
  y2 <- as.integer(x[, 1] > 0)
  r2 <- morie_logit_separation(y2, x)
  expect_identical(r2$separation, "complete")
  expect_false(r2$mle_exists)
  # overlap in the labels but a rank-deficient design: a zero-margin direction exists
  r3 <- morie_logit_separation(y, cbind(x, x[, 1] + x[, 2]))
  expect_identical(r3$separation, "none")
  expect_false(r3$mle_exists)
  expect_true(is.na(morie_logit_separation(y, x, method = "glm")$mle_exists))
})


test_that("the ring mis-specification bound is the gamma-weighted mean outer-ring share, and bounds the exact gap", {
  # a path of 5 units, one treated unit chosen uniformly; the outer ring of unit i is exposed when
  # a unit at distance exactly 2 is treated; every assignment is enumerated
  n <- 5L
  outer <- function(z, i) any(abs(which(z) - i) == 2L)
  share <- vapply(seq_len(n), function(i) mean(vapply(seq_len(n), function(t) outer(seq_len(n) == t, i), logical(1))), numeric(1))
  gamma <- c(0.5, -1, 2, 0, 1.5)
  exact_gap <- mean(vapply(seq_len(n), function(t) {
    z <- seq_len(n) == t
    mean(gamma * vapply(seq_len(n), function(i) as.numeric(outer(z, i)), numeric(1)))
  }, numeric(1)))
  r <- morie_spillover_ring_bound(share, gamma_max = 2, gamma = gamma)
  expect_equal(r$expected_gap, exact_gap, tolerance = 1e-12)   # expected_gap_eq
  expect_equal(r$expected_gap, mean(gamma * share), tolerance = 1e-12)
  expect_lte(abs(r$expected_gap), r$bound)                    # expected_gap_le
  expect_equal(r$bound, 2 * mean(share))
  expect_equal(morie_spillover_ring_bound(share, gamma_max = 2, gamma = rep(0, n))$expected_gap, 0)  # gap_zero
  expect_error(morie_spillover_ring_bound(share, gamma_max = 1, gamma = gamma), "exceeds")
  expect_error(morie_spillover_ring_bound(c(0.5, 1.2), gamma_max = 1), "probabilities")
})

test_that("the rank-persistence null is the hypergeometric law: it matches brute-force counting, sums to one, mean k^2/n", {
  n <- 7L
  k <- 3L
  A <- 1:k
  sets <- utils::combn(n, k)
  overlaps <- apply(sets, 2L, function(B) length(intersect(A, B)))
  brute <- vapply(0:k, function(j) sum(overlaps == j), numeric(1))
  r <- morie_rank_persistence_null(n, k, observed = 2)
  expect_equal(r$law$probability, brute / ncol(sets), tolerance = 1e-12)                       # card_overlapSets
  expect_equal(r$law$probability, choose(k, 0:k) * choose(n - k, k - 0:k) / choose(n, k))       # overlap_prob
  expect_equal(sum(r$law$probability), 1, tolerance = 1e-12)                                    # overlap_prob_sum
  expect_equal(sum(r$law$overlap * r$law$probability), k^2 / n, tolerance = 1e-12)
  expect_equal(r$p_value, mean(overlaps >= 2), tolerance = 1e-12)
  big <- morie_rank_persistence_null(158, 16, observed = 9)
  expect_equal(big$mean, 16^2 / 158)
  expect_lt(big$p_value, 1e-4)
  expect_error(morie_rank_persistence_null(5, 6), "between 0")
  expect_error(morie_rank_persistence_null(5, 2, observed = 3), "between 0")
})

test_that("the hot-spot threshold reproduces the Short-Brantingham algebra of Research.P4.Hotspot", {
  p <- list(A0 = 1, omega = 1, eps = 2, D = 1, gamma = 1, eta = 0.01, z = 1)
  r <- do.call(morie_hotspot_threshold, p)
  B <- r$steady[["B"]]; A <- r$steady[["A"]]; rho <- r$steady[["rho"]]
  # steady_iff: the two uniform right-hand sides vanish, and the state is the closed form
  expect_equal(-p$omega * B + p$eps * p$D * rho * (p$A0 + B), 0, tolerance = 1e-12)
  expect_equal(-rho * (p$A0 + B) + p$gamma, 0, tolerance = 1e-12)
  expect_equal(c(B, A, rho), c(2, 3, 1 / 3), tolerance = 1e-12)
  # det_eq / P_eq / Q_eq / R_eq against the 2x2 matrix built by hand
  ks <- c(0, 0.5, 2, 10)
  a <- -p$omega + p$eps * p$D * rho; b <- p$eps * p$D * A; c0 <- -rho; c1 <- 2 * p$D * rho / A
  d <- -A; d1 <- p$eta * p$D / p$z; d2 <- p$D
  dets <- vapply(ks, function(k) det(matrix(c(a - d1 * k, c0 + c1 * k, b, d - d2 * k), 2L)), numeric(1))
  tab <- morie_hotspot_threshold(p$A0, p$omega, p$eps, p$D, p$gamma, p$eta, p$z, k = ks)$dispersion
  expect_equal(tab$det, dets, tolerance = 1e-12)
  expect_equal(tab$det, r$P * ks^2 - r$Q * ks + r$R, tolerance = 1e-12)
  expect_equal(r$P, p$eta * p$D^2 / p$z)
  expect_equal(r$Q, p$D * (3 * p$eps * p$D * rho - p$omega - p$eta * A / p$z), tolerance = 1e-12)
  expect_equal(r$R, p$omega * A)
  expect_true(all(tab$trace < 0))                                                  # tr_neg
  # hotspots_above_threshold: Q > 0, Q^2 > 4 P R, the critical mode grows and its growth rate is
  # a root of the characteristic polynomial (unstable_iff)
  expect_true(r$Q > 0 && r$Q^2 > 4 * r$P * r$R && r$unstable)
  expect_equal(r$k_crit, r$Q / (2 * r$P))
  expect_lt(r$det_crit, 0)
  expect_gt(r$growth_crit, 0)
  tr_c <- (a - d1 * r$k_crit) + (d - d2 * r$k_crit)
  expect_equal(r$growth_crit^2 - tr_c * r$growth_crit + r$det_crit, 0, tolerance = 1e-10)
  # no_hotspots_without_reinforcement and no_hotspots_below_threshold
  r0 <- morie_hotspot_threshold(p$A0, p$omega, 0, p$D, p$gamma, p$eta, p$z, k = ks)
  expect_false(r0$unstable)
  expect_true(all(r0$dispersion$det > 0))
  rb <- morie_hotspot_threshold(p$A0, p$omega, 0.2, p$D, p$gamma, 0.5, p$z, k = ks)
  expect_lte(rb$threshold[["reinforcement"]], rb$threshold[["decay_plus_diffusion"]])
  expect_false(rb$unstable)
  expect_true(all(rb$dispersion$det > 0))
  expect_error(morie_hotspot_threshold(0, 1, 1, 1, 1, 0.1), "A0")
  expect_error(morie_hotspot_threshold(1, 1, -1, 1, 1, 0.1), "eps")
})

test_that("the comparable-subset dark figure matches Research.P1.Comparable", {
  r <- morie_dark_figure_comparable(VA = 1000, RA = 400, RB = 300, lambda = 0.5)
  expect_equal(r$rate_A, 0.4)                                           # rate_on_A
  expect_equal(r$rate_upper, 700 / 1300)                                # total_rate_le
  expect_equal(unname(r$total), c(1300, 1000 + 300 / 0.2))             # total_interval
  expect_equal(unname(r$dark_figure), c(600, 600 + 300 * (1 / 0.2 - 1)))  # dark_interval
  expect_equal(r$width, 300 * (1 - 0.2) / 0.2)                          # dark_figure_width
  # the bound is sharp: a VB inside the interval gives a rate between the ends
  for (VB in c(300, 800, 1500)) {
    rate <- 700 / (1000 + VB)
    expect_lte(rate, r$rate_upper)
    expect_gte(1000 + VB, r$total[["lower"]])
    expect_lte(1000 + VB, r$total[["upper"]])
  }
  # lambda r_A = 1 pins the dark figure to VA - RA (width zero)
  z <- morie_dark_figure_comparable(VA = 1000, RA = 1000, RB = 300, lambda = 1)
  expect_equal(z$width, 0)
  expect_equal(unname(z$dark_figure), c(0, 0))
  expect_error(morie_dark_figure_comparable(1000, 1200, 300), "exceeds")
  expect_error(morie_dark_figure_comparable(1000, 400, 300, lambda = 0), "lambda")
  expect_error(morie_dark_figure_comparable(0, 0, 300), "positive")
})
