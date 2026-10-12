# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research ledger, the library-scan closures of 2026-10-12 (second batch): twenty results with
# their R side. Every statement is a machine-checked theorem (Lean 4 + Mathlib, 0 sorry,
# standard axioms only) in research/lean/Researchproofs/:
#
#   P7ZeroInflation.lean     Research.P7.ZeroInflation.nb_matches_zip                 (scan A6)
#   P5GammaBound.lean        Research.P5.Gamma.worst_mean_mono, gamma_star_kills,
#                            gamma_star_table_only                                    (scan A7)
#   P3Blocking.lean          Research.P3.Blocking.paired_better_iff, crossover_pos    (scan A8)
#   P3Adjustment.lean        Research.P3.Adjustment.unadjusted_unbiased,
#                            adjusted_minus_unadjusted, adjusted_worse                (scan A9)
#   P8Budget.lean            Research.P8.Budget.concave_sum, interior_optimum        (scan A19)
#   P2OutcomeTest.lean       Research.P2.OutcomeTest.infra_marginality,
#                            uniform_equal_hits_equal_thresholds                     (scan A24)
#   P3Spatial.lean           Research.P3.Spatial.lag_regressor_correlated,
#                            gls_more_precise, fwer_tendsto_one                      (scan A25)
#   P8NaturalBounds.lean     Research.P8.NaturalBounds.width                         (scan B11)
#   P5DesignSensitivity.lean Research.P5.DesignSensitivity.eventually_reject,
#                            design_sensitivity_exists_unique                        (scan B17)
#   P7MoranTrend.lean        Research.P7.MoranTrend.trend_witness,
#                            residual_moran_ratio                                    (scan B19)
#   P3Exposure.lean          Research.P3.Exposure.exposure_prob_tendsto_zero,
#                            permutation_valid                                       (scan B21)
#   P1Thinning.lean          Research.P1.Thinning.thinned_pgf, not_identified         (scan C9)
#   P7Chernoff.lean          Research.P7.Chernoff.poisson_chernoff, rate_pos         (scan C11)
#   P3RandomWalk.lean        Research.P3.RandomWalk.stationary, directed_fails       (scan C15)
#   P10ExtremalIndex.lean    Research.P10.ExtremalIndex.cluster_tendsto,
#                            extremal_index                                          (scan C17)
#   P7TailShape.lean         Research.P7.TailShape.nb_pmf_ratio_tendsto_zero         (scan C18)
#   P5DataProcessing.lean    Research.P5.DataProcessing.bayes_risk_data_processing,
#                            calibration_incompatible                                (scan C20)
#   P4Bellman.lean           Research.P4.Bellman.bellman_contracting,
#                            myopic_suboptimal                                       (scan C21)
#   P5BudgetRule.lean        Research.P5.BudgetRule.threshold_optimal                (scan C22)
#   P10Cusum.lean            Research.P10.Cusum.stop_prob_le, mean_run_length_ge     (scan C23)

#' A negative binomial with the zero share and mean of any zero-inflated Poisson
#'
#' For ZIP(\code{pi}, \code{lambda}) the mean is \eqn{(1-\pi)\lambda} and the zero
#' probability \eqn{\pi + (1-\pi)e^{-\lambda}}, strictly between \eqn{e^{-\mu}}
#' and 1. The negative binomial zero probability \eqn{(1+\mu/\theta)^{-\theta}}
#' runs continuously from 1 to \eqn{e^{-\mu}}, so some \eqn{\theta} matches
#' (\code{Research.P7.ZeroInflation.nb_matches_zip}): the zero share with the
#' mean cannot tell inflation from heterogeneity.
#'
#' @param pi Inflation weight in (0, 1).
#'
#' @param lambda Poisson rate of the uninflated part, positive.
#'
#' @return A list: \code{mean}, \code{zip_zero}, \code{theta} (the matching
#'   dispersion), \code{nb_zero} (equal to \code{zip_zero}) and \code{size}
#'   (\code{theta}, the \code{dnbinom} parameterisation).
#'
#' @references Weisburd D, Britt C (2014). Statistics in Criminal Justice, 4th ed.,
#'   ch. 6. Springer.
#' @examples
#' morie_nb_matches_zip(pi = 0.3, lambda = 2)
#' @export
morie_nb_matches_zip <- function(pi, lambda) {
  if (pi <= 0 || pi >= 1) stop("`pi` must be in (0, 1)", call. = FALSE)
  if (lambda <= 0) stop("`lambda` must be positive", call. = FALSE)
  mu <- (1 - pi) * lambda
  p0 <- pi + (1 - pi) * exp(-lambda)
  f <- function(theta) (1 + mu / theta)^(-theta) - p0
  theta <- stats::uniroot(f, c(1e-8, 1e8), tol = 1e-12)$root
  structure(list(mean = mu, zip_zero = p0, theta = theta, nb_zero = (1 + mu / theta)^(-theta), size = theta),
            theorem = "Research.P7.ZeroInflation.nb_matches_zip")
}

#' A Rosenbaum Gamma bound is not a test of ignorability
#'
#' For a matched-pairs sign test with \code{n} discordant pairs and \code{T}
#' treated-case pairs, the worst-case expectation of \eqn{T} at sensitivity
#' \eqn{\Gamma} is \eqn{n\Gamma/(1+\Gamma)}, non-decreasing in \eqn{\Gamma}
#' (\code{Research.P5.Gamma.worst_mean_mono}); \eqn{\Gamma^* = T/(n-T)} is the
#' value where it reaches \eqn{T} (\code{gamma_star_kills}) and depends on the
#' table alone (\code{gamma_star_table_only}).
#'
#' @param T Number of discordant pairs in which the treated unit is the case.
#'
#' @param n Number of discordant pairs.
#'
#' @param Gamma Optional sensitivity values at which to evaluate the worst case.
#'
#' @return A list: \code{gamma_star} and, with \code{Gamma}, a data frame
#'   \code{worst} with \code{Gamma}, \code{worst_mean} and \code{survives}
#'   (\code{T} above the worst-case mean).
#'
#' @references Rosenbaum PR (2002). Observational Studies, 2nd ed. Springer.
#' @examples
#' morie_gamma_bound(T = 70, n = 100, Gamma = c(1, 1.5, 2, 2.5))
#' @export
morie_gamma_bound <- function(T, n, Gamma = NULL) {
  if (T < 0 || T >= n) stop("`T` must satisfy 0 <= T < n", call. = FALSE)
  out <- list(gamma_star = T / (n - T))
  if (!is.null(Gamma)) {
    if (any(Gamma < 0)) stop("`Gamma` must be non-negative", call. = FALSE)
    wm <- n * Gamma / (1 + Gamma)
    out$worst <- data.frame(Gamma = Gamma, worst_mean = wm, survives = wm < T)
  }
  structure(out, theorem = "Research.P5.Gamma.gamma_star_kills")
}

#' Blocking does not always increase power: the within-pair correlation crossover
#'
#' With \code{n} pairs, effect \code{delta}, unit standard deviation \code{sigma}
#' and t critical values \eqn{c_p} (paired, \eqn{n-1} df) and \eqn{c_u}
#' (two-sample, \eqn{2n-2} df), the paired design has more approximate power
#' iff the within-pair correlation exceeds
#' \eqn{\rho^* = 1 - (1 + (c_p-c_u)/k)^{-2}} with \eqn{k = \delta\sqrt{n/2}/\sigma}
#' (\code{Research.P3.Blocking.paired_better_iff}); \eqn{\rho^* > 0}
#' (\code{crossover_pos}).
#'
#' @param delta,sigma Effect size and unit standard deviation.
#'
#' @param n Number of pairs (at least 3).
#'
#' @param alpha One-sided level for the critical values.
#'
#' @param rho Optional within-pair correlations to classify.
#'
#' @return A list: \code{k}, \code{c_paired}, \code{c_unpaired}, \code{crossover}
#'   and, with \code{rho}, \code{paired_better} and the two power deviates.
#'
#' @references Weisburd D, Britt C (2014). Statistics in Criminal Justice, 4th ed.,
#'   ch. 9. Springer.
#' @examples
#' morie_blocking_crossover(delta = 0.5, sigma = 1, n = 10, rho = c(0.05, 0.3))
#' @export
morie_blocking_crossover <- function(delta, sigma, n, alpha = 0.05, rho = NULL) {
  if (delta <= 0 || sigma <= 0 || n < 3) stop("`delta`, `sigma` positive and `n` at least 3", call. = FALSE)
  k <- delta * sqrt(n / 2) / sigma
  cp <- stats::qt(1 - alpha, df = n - 1)
  cu <- stats::qt(1 - alpha, df = 2 * n - 2)
  out <- list(k = k, c_paired = cp, c_unpaired = cu, crossover = 1 - 1 / (1 + (cp - cu) / k)^2)
  if (!is.null(rho)) {
    if (any(rho >= 1)) stop("`rho` must be below 1", call. = FALSE)
    out$z_paired <- k / sqrt(1 - rho) - cp
    out$z_unpaired <- k - cu
    out$paired_better <- out$z_paired > out$z_unpaired
  }
  structure(out, theorem = "Research.P3.Blocking.paired_better_iff")
}

#' Covariate adjustment can cost precision under unequal allocation
#'
#' Superpopulation asymptotic variances (times \eqn{n}) with arm slopes
#' \code{b1}, \code{b0}, noise variances \code{v1}, \code{v0} and treated share
#' \code{p}: unadjusted, pooled-slope ANCOVA and Lin's interaction estimator
#' (\code{Research.P3.Adjustment.adjusted_minus_unadjusted},
#' \code{lin_le_unadjusted}, \code{lin_le_adjusted}); with \code{b0 = 0} the
#' pooled adjustment is worse than none iff \eqn{p > 2/3}
#' (\code{adjusted_worse}), never at \eqn{p = 1/2}
#' (\code{adjusted_le_unadjusted_balanced}).
#'
#' @param b1,b0 Slopes of the potential outcomes on the centred unit-variance
#'   covariate.
#'
#' @param v1,v0 Noise variances.
#'
#' @param p Treated share in (0, 1).
#'
#' @return A list: \code{unadjusted}, \code{adjusted}, \code{lin},
#'   \code{adjusted_minus_unadjusted} and \code{adjustment_hurts}.
#'
#' @references Freedman DA (2008). On regression adjustments to experimental data.
#'   Advances in Applied Mathematics 40(2). Lin W (2013). Agnostic notes on regression
#'   adjustments to experimental data. Annals of Applied Statistics 7(1).
#' @examples
#' morie_adjustment_variances(b1 = 1, b0 = 0, v1 = 1, v0 = 1, p = 0.8)
#' @export
morie_adjustment_variances <- function(b1, b0, v1, v0, p) {
  if (p <= 0 || p >= 1) stop("`p` must be in (0, 1)", call. = FALSE)
  if (v1 < 0 || v0 < 0) stop("noise variances must be non-negative", call. = FALSE)
  d <- b1 - b0
  un <- (b1^2 + v1) / p + (b0^2 + v0) / (1 - p)
  ad <- ((1 - p)^2 * d^2 + v1) / p + (p^2 * d^2 + v0) / (1 - p)
  lin <- v1 / p + v0 / (1 - p)
  structure(list(unadjusted = un, adjusted = ad, lin = lin, adjusted_minus_unadjusted = ad - un,
                 adjustment_hurts = ad > un),
            theorem = "Research.P3.Adjustment.adjusted_minus_unadjusted")
}

#' Stick versus carrot under a fixed budget: the interior optimum
#'
#' Maximises \eqn{g(\theta) + d(C - \theta)} over \eqn{\theta \in [0, C]} on a
#' grid; with concave \code{g} and \code{d} the objective is concave
#' (\code{Research.P8.Budget.concave_sum}) and the maximiser is interior as
#' soon as some interior point beats both corners (\code{interior_optimum}).
#'
#' @param g,d Rehabilitation and deterrence response functions of the spend.
#'
#' @param C Budget.
#'
#' @param grid Number of grid points.
#'
#' @return A list: \code{theta} (maximiser), \code{value}, \code{interior},
#'   \code{corner_values} and the grid data frame.
#'
#' @references D'Orsogna MR, Perc M (2015). Statistical physics of crime: a review.
#'   Physics of Life Reviews 12.
#' @examples
#' morie_budget_optimum(g = sqrt, d = function(s) 2 * log1p(s), C = 10)
#' @export
morie_budget_optimum <- function(g, d, C, grid = 201L) {
  if (C <= 0) stop("`C` must be positive", call. = FALSE)
  theta <- seq(0, C, length.out = grid)
  f <- g(theta) + d(C - theta)
  i <- which.max(f)
  structure(list(theta = theta[i], value = f[i], interior = i > 1 && i < grid,
                 corner_values = c(stick = f[1], carrot = f[grid]),
                 grid = data.frame(theta = theta, objective = f)),
            theorem = "Research.P8.Budget.interior_optimum")
}

#' The outcome test: equal hit rates do not certify equal thresholds
#'
#' Each group is a list with guilt probabilities \code{q}, weights \code{w} and a
#' threshold \code{t}; a person is stopped when \eqn{q > t}. The stop rate and
#' the hit rate (mean guilt among the stopped) are returned. Different signal
#' laws and thresholds can give equal hit rates
#' (\code{Research.P2.OutcomeTest.infra_marginality}) and equal thresholds can
#' give unequal ones (\code{equal_thresholds_unequal_hits}); only under one
#' signal law with a strictly increasing conditional mean do equal hit rates
#' pin equal thresholds (\code{uniform_equal_hits_equal_thresholds}).
#'
#' @param groups A named list of lists, each with \code{q}, \code{w} and \code{t}.
#'
#' @return A data frame with one row per group: \code{threshold},
#'   \code{stop_rate}, \code{hit_rate}.
#'
#' @references Bowling B, Phillips C (2007). Disproportionate and discriminatory.
#'   Modern Law Review 70(6).
#' @examples
#' morie_outcome_test(list(A = list(q = c(0.3, 0.9), w = c(1, 1), t = 0.5),
#'                         B = list(q = 0.9, w = 1, t = 0.2)))
#' @export
morie_outcome_test <- function(groups) {
  if (!is.list(groups) || is.null(names(groups))) stop("`groups` must be a named list", call. = FALSE)
  rows <- lapply(groups, function(g) {
    if (any(g$q < 0 | g$q > 1) || any(g$w < 0) || sum(g$w) <= 0) stop("each group needs q in [0, 1] and positive weights", call. = FALSE)
    s <- g$q > g$t
    stop_rate <- sum(g$w[s]) / sum(g$w)
    hit <- if (stop_rate > 0) sum(g$w[s] * g$q[s]) / sum(g$w[s]) else NA_real_
    data.frame(threshold = g$t, stop_rate = stop_rate, hit_rate = hit)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- names(groups)
  structure(out, theorem = "Research.P2.OutcomeTest.infra_marginality")
}

#' Spatial-lag omission is bias, spatial-error omission is inefficiency
#'
#' For the two-unit swap model \eqn{y = \rho W y + u} the lag regressor's
#' covariance with the error is \eqn{\rho/(1-\rho^2)}
#' (\code{Research.P3.Spatial.lag_regressor_correlated}); for a regressor
#' \code{x} with error covariance \code{Sigma} the slope variances are
#' \eqn{x^T\Sigma x/(x^Tx)^2} (OLS) and \eqn{1/(x^T\Sigma^{-1}x)} (GLS)
#' (\code{gls_more_precise}); \code{m} uncorrected local tests at level
#' \code{alpha} reject somewhere with probability \eqn{1-(1-\alpha)^m}
#' (\code{fwer_tendsto_one}).
#'
#' @param rho Spatial-lag coefficient with \eqn{\rho^2 \ne 1}.
#'
#' @param x Regressor vector.
#'
#' @param Sigma Error covariance matrix.
#'
#' @param alpha,m Level and number of local tests.
#'
#' @return A list: \code{lag_covariance}, \code{var_ols}, \code{var_gls},
#'   \code{gls_more_precise}, \code{fwer}.
#'
#' @references Kikuchi G (2010). Neighborhood Structures and Crime. LFB Scholarly.
#' @examples
#' morie_spatial_omission(rho = 0.4, x = c(1, 2), Sigma = matrix(c(1, 0.5, 0.5, 1), 2), m = 50)
#' @export
morie_spatial_omission <- function(rho, x, Sigma, alpha = 0.05, m = 1L) {
  if (rho^2 == 1) stop("`rho^2` must differ from 1", call. = FALSE)
  Sigma <- as.matrix(Sigma)
  if (length(x) != nrow(Sigma) || nrow(Sigma) != ncol(Sigma)) stop("`Sigma` must be square with `length(x)` rows", call. = FALSE)
  vo <- as.numeric(t(x) %*% Sigma %*% x) / sum(x^2)^2
  vg <- 1 / as.numeric(t(x) %*% solve(Sigma) %*% x)
  structure(list(lag_covariance = rho / (1 - rho^2), var_ols = vo, var_gls = vg,
                 gls_more_precise = vg <= vo + 1e-12, fwer = 1 - (1 - alpha)^m),
            theorem = "Research.P3.Spatial.lag_regressor_correlated")
}

#' Instrumental-variable natural bounds
#'
#' With \eqn{p_{11} = P(A=1|Z=1)}, \eqn{p_{10} = P(A=1|Z=0)},
#' \eqn{m_1 = E[Y|A=1,Z=1]}, \eqn{m_0 = E[Y|A=0,Z=0]} and a bounded outcome,
#' the average effect lies in an interval whose ends are attained and whose
#' width is \eqn{P(A=0|Z=1) + P(A=1|Z=0)}
#' (\code{Research.P8.NaturalBounds.width}).
#'
#' @param p11,p10 Treatment probabilities under the two instrument values.
#'
#' @param m1,m0 Observed outcome means among the treated under \eqn{Z=1} and the
#'   untreated under \eqn{Z=0}, in \eqn{[0, 1]}.
#'
#' @return A list: \code{lower}, \code{upper}, \code{width}.
#'
#' @references Pearl J (2009). Causality, 2nd ed., section 8.2. Cambridge.
#' @examples
#' morie_iv_natural_bounds(p11 = 0.8, p10 = 0.3, m1 = 0.6, m0 = 0.4)
#' @export
morie_iv_natural_bounds <- function(p11, p10, m1, m0) {
  if (any(c(p11, p10, m1, m0) < 0 | c(p11, p10, m1, m0) > 1)) stop("all inputs must lie in \\[0, 1\\]", call. = FALSE)
  lower <- m1 * p11 - m0 * (1 - p10) - p10
  upper <- m1 * p11 + (1 - p11) - m0 * (1 - p10)
  structure(list(lower = lower, upper = upper, width = upper - lower),
            theorem = "Research.P8.NaturalBounds.width")
}

#' Design sensitivity: the Gamma at which a sensitivity analysis's limiting power turns
#'
#' Along a sequence of studies with \eqn{T_n/n \to \mu} and worst-case critical
#' values \eqn{c_n/n \to \mu_\Gamma}, the test rejects for all large \eqn{n}
#' when \eqn{\mu > \mu_\Gamma} and never when \eqn{\mu < \mu_\Gamma}
#' (\code{Research.P5.DesignSensitivity.eventually_reject},
#' \code{eventually_accept}); the design sensitivity is the unique root of
#' \eqn{\mu_\Gamma = \mu} when \eqn{\Gamma \mapsto \mu_\Gamma} is continuous and
#' strictly increasing (\code{design_sensitivity_exists_unique}).
#'
#' @param mu Limiting standardised statistic under the alternative.
#'
#' @param mu_gamma Function of \eqn{\Gamma} giving the worst-case limit; the
#'   default is the sign-score form \eqn{\Gamma/(1+\Gamma)}.
#'
#' @param upper Upper end of the search interval.
#'
#' @return A list: \code{gamma_tilde} and the limiting power at a few \eqn{\Gamma}.
#'
#' @references Rosenbaum PR (2004). Design sensitivity in observational studies.
#'   Biometrika 91(1).
#' @examples
#' morie_design_sensitivity(mu = 0.7)
#' @export
morie_design_sensitivity <- function(mu, mu_gamma = function(G) G / (1 + G), upper = 1e4) {
  if (mu_gamma(1) >= mu) stop("`mu` must exceed `mu_gamma(1)`: no effect to detect", call. = FALSE)
  if (mu_gamma(upper) <= mu) stop("`upper` too small: `mu_gamma(upper)` must exceed `mu`", call. = FALSE)
  gt <- stats::uniroot(function(G) mu_gamma(G) - mu, c(1, upper), tol = 1e-10)$root
  G <- c(1, gt / 2 + 1 / 2, 2 * gt)
  structure(list(gamma_tilde = gt,
                 limiting_power = data.frame(Gamma = G, power = as.numeric(mu_gamma(G) < mu))),
            theorem = "Research.P5.DesignSensitivity.design_sensitivity_exists_unique")
}

#' Moran's I under a deterministic trend, and on regression residuals
#'
#' Computes Moran's I of \code{y} for weights \code{W}, the null expectation
#' \eqn{-1/(n-1)}, and, with a design matrix \code{X}, the ratio-of-expectations
#' value for OLS residuals \eqn{(n/S_0)\,\mathrm{tr}(WM)/(n-p)}
#' (\code{Research.P7.MoranTrend.residual_moran_ratio}). A pure trend with no
#' noise already moves I away from the null (\code{trend_witness}).
#'
#' @param W Weight matrix.
#'
#' @param y Values.
#'
#' @param X Optional design matrix (with intercept) for the residual form.
#'
#' @return A list: \code{I}, \code{null_expectation} and, with \code{X},
#'   \code{I_residual} and \code{residual_expectation}.
#'
#' @references Schabenberger O, Gotway CA (2005). Statistical Methods for Spatial Data
#'   Analysis, section 1.3.2.2. CRC.
#' @examples
#' W <- matrix(0, 4, 4); W[cbind(1:3, 2:4)] <- 1; W <- W + t(W)
#' morie_moran_trend(W, y = 1:4)
#' @export
morie_moran_trend <- function(W, y, X = NULL) {
  W <- as.matrix(W)
  n <- length(y)
  if (nrow(W) != n || ncol(W) != n) stop("`W` must be n x n", call. = FALSE)
  S0 <- sum(W)
  z <- y - mean(y)
  I <- (n / S0) * as.numeric(t(z) %*% W %*% z) / sum(z^2)
  out <- list(I = I, null_expectation = -1 / (n - 1))
  if (!is.null(X)) {
    X <- as.matrix(X)
    M <- diag(n) - X %*% solve(crossprod(X)) %*% t(X)
    e <- as.numeric(M %*% y)
    out$I_residual <- (n / S0) * as.numeric(t(e) %*% W %*% e) / sum(e^2)
    out$residual_expectation <- (n / S0) * sum(diag(W %*% M)) / (n - ncol(X))
  }
  structure(out, theorem = "Research.P7.MoranTrend.residual_moran_ratio")
}

#' Exposure positivity under Bernoulli assignment
#'
#' The probability that all \code{d} neighbours are treated is \eqn{p^d \to 0}
#' and the Horvitz-Thompson weight \eqn{1/p^d \to \infty}
#' (\code{Research.P3.Exposure.exposure_prob_tendsto_zero},
#' \code{ht_weight_unbounded}).
#'
#' @param p Treatment probability in (0, 1).
#'
#' @param degrees Neighbourhood sizes.
#'
#' @return A data frame: \code{degree}, \code{prob_all_treated}, \code{ht_weight}.
#'
#' @references Wager S (2024). Causal Inference: A Statistical Learning Approach,
#'   ch. 11-12.
#' @examples
#' morie_exposure_positivity(p = 0.5, degrees = c(1, 5, 10, 30))
#' @export
morie_exposure_positivity <- function(p, degrees) {
  if (p <= 0 || p >= 1) stop("`p` must be in (0, 1)", call. = FALSE)
  if (any(degrees < 0)) stop("`degrees` must be non-negative", call. = FALSE)
  structure(data.frame(degree = degrees, prob_all_treated = p^degrees, ht_weight = p^(-degrees)),
            theorem = "Research.P3.Exposure.exposure_prob_tendsto_zero")
}

#' Exactness of the permutation p-value over a finite design
#'
#' For statistic values over every assignment of a finite design, the
#' permutation p-value of an assignment is the share of assignments with a
#' statistic at least as large; the share of assignments with p-value at most
#' \code{alpha} never exceeds \code{alpha}
#' (\code{Research.P3.Exposure.permutation_valid}). Under interference this
#' holds whenever the statistic is imputable under the null on the focal units.
#'
#' @param stats Statistic values, one per assignment in the design.
#'
#' @param alpha Levels to check.
#'
#' @return A list: \code{pvalues} (one per assignment) and a data frame
#'   \code{levels} with \code{alpha}, \code{rejection_share} and \code{valid}.
#'
#' @references Wager S (2024). Causal Inference: A Statistical Learning Approach,
#'   ch. 11.
#' @examples
#' morie_permutation_valid(stats = c(3, 1, 4, 1, 5, 9, 2, 6), alpha = c(0.1, 0.25, 0.5))
#' @export
morie_permutation_valid <- function(stats, alpha = c(0.01, 0.05, 0.1)) {
  if (length(stats) < 1L) stop("`stats` must be non-empty", call. = FALSE)
  if (any(alpha < 0 | alpha > 1)) stop("`alpha` must lie in \\[0, 1\\]", call. = FALSE)
  pv <- vapply(stats, function(t) mean(stats >= t), numeric(1))
  share <- vapply(alpha, function(a) mean(pv <= a), numeric(1))
  structure(list(pvalues = pv, levels = data.frame(alpha = alpha, rejection_share = share, valid = share <= alpha + 1e-12)),
            theorem = "Research.P3.Exposure.permutation_valid")
}

#' Thinning a Poisson count: the recording rate is not identified
#'
#' Recording each event independently with probability \code{gamma} turns a
#' Poisson(\code{lambda}) count into a Poisson(\code{lambda * gamma}) count
#' (\code{Research.P1.Thinning.thinned_pgf}), independent of the unrecorded
#' remainder (\code{joint_pgf_factorises}); the pair is identified only through
#' the product (\code{not_identified}).
#'
#' @param lambda True rate.
#'
#' @param gamma Recording probability in (0, 1).
#'
#' @param s Points at which to compare generating functions.
#'
#' @return A list: \code{recorded_rate}, \code{alternative} (a different
#'   \code{lambda}, \code{gamma} pair with the same recorded law), and
#'   \code{pgf_check} (the thinned and the Poisson generating functions agree).
#'
#' @references Grimmett G, Stirzaker D (2020). Probability and Random Processes,
#'   4th ed., Theorem 6.13.14. Oxford.
#' @examples
#' morie_thinning_identification(lambda = 5, gamma = 0.3)
#' @export
morie_thinning_identification <- function(lambda, gamma, s = c(0, 0.5, 0.9, 1)) {
  if (lambda <= 0) stop("`lambda` must be positive", call. = FALSE)
  if (gamma <= 0 || gamma >= 1) stop("`gamma` must be in (0, 1)", call. = FALSE)
  thinned <- exp(lambda * ((1 - gamma + gamma * s) - 1))
  direct <- exp(lambda * gamma * (s - 1))
  structure(list(recorded_rate = lambda * gamma,
                 alternative = c(lambda = lambda * gamma, gamma = 1),
                 pgf_check = all(abs(thinned - direct) < 1e-12)),
            theorem = "Research.P1.Thinning.not_identified")
}

#' Chernoff tail bound for a Poisson count
#'
#' \eqn{P(S \ge (1+\epsilon)\lambda) \le \exp(-\lambda[(1+\epsilon)\log(1+\epsilon)-\epsilon])}
#' (\code{Research.P7.Chernoff.poisson_chernoff}), with a positive rate
#' (\code{rate_pos}); the exact Poisson tail is returned beside it.
#'
#' @param lambda Mean.
#'
#' @param eps Relative excess, positive.
#'
#' @return A data frame: \code{eps}, \code{bound}, \code{exact}, \code{rate}.
#'
#' @references Grimmett G, Stirzaker D (2020). Probability and Random Processes,
#'   4th ed., Theorem 5.11.4. Oxford.
#' @examples
#' morie_poisson_chernoff(lambda = 20, eps = c(0.25, 0.5, 1))
#' @export
morie_poisson_chernoff <- function(lambda, eps) {
  if (lambda <= 0 || any(eps <= 0)) stop("`lambda` and `eps` must be positive", call. = FALSE)
  rate <- (1 + eps) * log1p(eps) - eps
  structure(data.frame(eps = eps, bound = exp(-lambda * rate),
                       exact = stats::ppois(ceiling((1 + eps) * lambda) - 1, lambda, lower.tail = FALSE),
                       rate = rate),
            theorem = "Research.P7.Chernoff.poisson_chernoff")
}

#' Random-walk exposure on a graph is proportional to degree
#'
#' For an undirected adjacency matrix without isolated vertices the simple random
#' walk has stationary law \eqn{d_v/(2|E|)}
#' (\code{Research.P3.RandomWalk.stationary}); for a directed one the
#' out-degree law is checked and in general fails (\code{directed_fails}).
#'
#' @param adj Adjacency matrix (0/1); symmetric for an undirected graph.
#'
#' @return A list: \code{pi} (the degree law), \code{stationary} (whether
#'   \eqn{\pi P = \pi}), \code{undirected}.
#'
#' @references Grimmett G, Stirzaker D (2020). Probability and Random Processes,
#'   4th ed., Exercise 6.4.6. Oxford.
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3)
#' morie_random_walk_exposure(adj)
#' @export
morie_random_walk_exposure <- function(adj) {
  adj <- as.matrix(adj)
  if (nrow(adj) != ncol(adj) || any(adj < 0)) stop("`adj` must be a square non-negative matrix", call. = FALSE)
  d <- rowSums(adj)
  if (any(d == 0)) stop("every vertex needs an out-edge", call. = FALSE)
  P <- adj / d
  pi <- d / sum(d)
  structure(list(pi = pi, stationary = all(abs(as.numeric(pi %*% P) - pi) < 1e-10),
                 undirected = isTRUE(all.equal(adj, t(adj)))),
            theorem = "Research.P3.RandomWalk.stationary")
}

#' The extremal index of a block-cluster process is the reciprocal cluster size
#'
#' With \code{n} blocks of \code{m} days and cluster-start probability
#' \eqn{\tau/(nm)}, the no-exceedance probability is \eqn{(1-\tau/(nm))^n \to e^{-\tau/m}}
#' while the independent comparison gives \eqn{e^{-\tau}}
#' (\code{Research.P10.ExtremalIndex.cluster_tendsto}, \code{iid_tendsto}), so
#' \eqn{\theta = 1/m} (\code{extremal_index}).
#'
#' @param m Cluster (block) length.
#'
#' @param tau Expected number of cluster starts.
#'
#' @param n Numbers of blocks at which to evaluate.
#'
#' @return A list: \code{theta}, \code{limit_cluster}, \code{limit_iid} and a
#'   data frame of finite-\code{n} survival probabilities.
#'
#' @references Coles S (2001). An Introduction to Statistical Modeling of Extreme
#'   Values, Theorem 5.2. Springer.
#' @examples
#' morie_extremal_index_clusters(m = 4, tau = 2, n = c(10, 100, 1000))
#' @export
morie_extremal_index_clusters <- function(m, tau, n = c(10, 100, 1000)) {
  if (m < 1 || tau <= 0 || any(n < 1)) stop("`m`, `n` at least 1 and `tau` positive", call. = FALSE)
  structure(list(theta = 1 / m, limit_cluster = exp(-tau / m), limit_iid = exp(-tau),
                 table = data.frame(n = n, cluster = (1 - tau / (n * m))^n, iid = (1 - tau / (n * m))^(n * m))),
            theorem = "Research.P10.ExtremalIndex.extremal_index")
}

#' Tail ratios that separate light from power-law count tails
#'
#' The ratio \eqn{S(2k)/S(k)} is \eqn{q^k \to 0} for a geometric tail, the
#' constant \eqn{2^{-\alpha}} for a power tail
#' (\code{Research.P7.TailShape.geometric_ratio_tendsto_zero},
#' \code{power_ratio_const}), and the negative-binomial pmf ratio
#' \eqn{p(2k)/p(k)} is at most \eqn{(2k+r)^{r-1}q^k \to 0}
#' (\code{nb_pmf_ratio_le}, \code{nb_pmf_ratio_tendsto_zero}); the empirical
#' survival ratio of \code{counts} is returned beside them.
#'
#' @param k Lags at which to evaluate.
#'
#' @param q Geometric / negative-binomial success parameter in (0, 1).
#'
#' @param alpha Power-law exponent.
#'
#' @param r Negative-binomial integer shape.
#'
#' @param counts Optional observed counts for the empirical ratio.
#'
#' @return A data frame with one row per \code{k}.
#'
#' @references Coles S (2001). An Introduction to Statistical Modeling of Extreme
#'   Values, Theorem 4.1. Springer.
#' @examples
#' morie_tail_ratio(k = c(5, 10, 20), q = 0.7, alpha = 1.5, r = 3)
#' @export
morie_tail_ratio <- function(k, q = 0.5, alpha = 1, r = 2L, counts = NULL) {
  if (any(k < 1) || q <= 0 || q >= 1 || alpha <= 0 || r < 1) stop("`k`, `r` at least 1, `q` in (0, 1), `alpha` positive", call. = FALSE)
  nb <- function(j) choose(j + r - 1, j) * (1 - q)^r * q^j
  out <- data.frame(k = k, geometric = q^k, power = 2^(-alpha), nb_pmf_ratio = nb(2 * k) / nb(k),
                    nb_bound = (2 * k + r)^(r - 1) * q^k)
  if (!is.null(counts)) {
    S <- function(j) mean(counts > j)
    out$empirical <- vapply(k, function(j) if (S(j) > 0) S(2 * j) / S(j) else NA_real_, numeric(1))
  }
  structure(out, theorem = "Research.P7.TailShape.nb_pmf_ratio_tendsto_zero")
}

#' Calibration to the recorded label and to the true label cannot both hold across groups
#'
#' With symmetric recording flip rates \code{fA}, \code{fB}, the recorded rate
#' is \eqn{f + (1-2f)\,p_W}. Calibration to the record in both groups at score
#' \code{r} forces true rates \eqn{(r-f_g)/(1-2f_g)} that differ
#' (\code{Research.P5.DataProcessing.calibration_incompatible}); calibration to
#' the truth forces recorded rates that differ
#' (\code{calibration_to_W_incompatible}).
#'
#' @param fA,fB Flip rates, not 1/2.
#'
#' @param r Score values.
#'
#' @return A data frame: \code{r}, true rates under record calibration
#'   (\code{true_A}, \code{true_B}), recorded rates under true calibration
#'   (\code{recorded_A}, \code{recorded_B}) and \code{incompatible}.
#'
#' @references MacKay DJC (2003). Information Theory, Inference, and Learning
#'   Algorithms, Exercise 8.9. Cambridge.
#' @examples
#' morie_calibration_flip(fA = 0.05, fB = 0.2, r = c(0.3, 0.5, 0.8))
#' @export
morie_calibration_flip <- function(fA, fB, r) {
  if (fA == 0.5 || fB == 0.5) stop("flip rates of 1/2 carry no information", call. = FALSE)
  structure(data.frame(r = r, true_A = (r - fA) / (1 - 2 * fA), true_B = (r - fB) / (1 - 2 * fB),
                       recorded_A = fA + (1 - 2 * fA) * r, recorded_B = fB + (1 - 2 * fB) * r,
                       incompatible = fA != fB & r != 0.5),
            theorem = "Research.P5.DataProcessing.calibration_incompatible")
}

#' Post-processing cannot lower the best squared-error risk
#'
#' For a finite joint weight \code{mu} of the target \code{w} and the recorded
#' label \code{D}, and a post-processing \code{h} mapping \code{D} to a score,
#' the best risk from the score is at least the best from \code{D}
#' (\code{Research.P5.DataProcessing.bayes_risk_data_processing}).
#'
#' @param mu Matrix of joint weights, rows indexed by target values, columns by
#'   \code{D} values.
#'
#' @param w Target value per row of \code{mu}.
#'
#' @param h Integer vector mapping each column of \code{mu} to a score level.
#'
#' @return A list: \code{risk_D}, \code{risk_R}, \code{dpi_holds}.
#'
#' @references MacKay DJC (2003). Information Theory, Inference, and Learning
#'   Algorithms, Exercise 8.9. Cambridge.
#' @examples
#' mu <- matrix(c(0.3, 0.1, 0.1, 0.2, 0.05, 0.25), 2)
#' morie_bayes_risk_dpi(mu, w = c(0, 1), h = c(1, 1, 2))
#' @export
morie_bayes_risk_dpi <- function(mu, w, h) {
  mu <- as.matrix(mu)
  if (length(w) != nrow(mu) || length(h) != ncol(mu) || any(mu < 0)) stop("`w` indexes rows, `h` columns, weights non-negative", call. = FALSE)
  best <- function(groups) {
    risk <- 0
    for (g in unique(groups)) {
      cols <- which(groups == g)
      mass <- rowSums(mu[, cols, drop = FALSE])
      if (sum(mass) > 0) {
        pred <- sum(mass * w) / sum(mass)
        risk <- risk + sum(mass * (w - pred)^2)
      }
    }
    risk
  }
  rd <- best(seq_len(ncol(mu)))
  rr <- best(h)
  structure(list(risk_D = rd, risk_R = rr, dpi_holds = rd <= rr + 1e-12),
            theorem = "Research.P5.DataProcessing.bayes_risk_data_processing")
}

#' Value iteration with the Bellman contraction audited
#'
#' For rewards \code{r} (states by actions), transitions \code{p} (an array
#' states by actions by states) and discount \code{gamma}, the Bellman operator
#' contracts the sup norm by \code{gamma}
#' (\code{Research.P4.Bellman.bellman_lipschitz}) and has a unique fixed point
#' (\code{optimal_value_exists_unique}), found by iteration. The greedy
#' three-state example is returned by \code{morie_myopic_example}.
#'
#' @param r Reward matrix.
#'
#' @param p Transition array.
#'
#' @param gamma Discount in [0, 1).
#'
#' @param iter,tol Iteration cap and tolerance.
#'
#' @return A list: \code{value}, \code{policy}, \code{iterations},
#'   \code{contraction_ratio} (largest observed step ratio, at most
#'   \code{gamma}) and \code{converged}.
#'
#' @references Puterman ML (1994). Markov Decision Processes, Theorem 6.2.3. Wiley.
#' @examples
#' r <- matrix(c(1, 0.5, 1, 0, 0.5, 1), nrow = 3)
#' p <- array(0, c(3, 2, 3)); p[1, 1, 2] <- 1; p[1, 2, 3] <- 1
#' p[2, , 2] <- 1; p[3, , 3] <- 1
#' morie_bellman_value(r, p, gamma = 0.9)
#' @export
morie_bellman_value <- function(r, p, gamma, iter = 1000L, tol = 1e-10) {
  r <- as.matrix(r)
  if (gamma < 0 || gamma >= 1) stop("`gamma` must lie in [0, 1)", call. = FALSE)
  if (length(dim(p)) != 3L || any(dim(p)[1:2] != dim(r)) || dim(p)[3] != nrow(r)) stop("`p` must be states x actions x states", call. = FALSE)
  if (any(p < 0) || any(abs(apply(p, c(1, 2), sum) - 1) > 1e-10)) stop("`p` rows must be probability vectors", call. = FALSE)
  nS <- nrow(r)
  v <- rep(0, nS)
  bellman <- function(v) {
    q <- r + gamma * apply(p, c(1, 2), function(pr) sum(pr * v))
    list(v = apply(q, 1, max), a = apply(q, 1, which.max))
  }
  ratio <- 0
  prev_step <- NA_real_
  for (i in seq_len(iter)) {
    b <- bellman(v)
    step <- max(abs(b$v - v))
    if (!is.na(prev_step) && prev_step > 1e-6) ratio <- max(ratio, step / prev_step)
    v <- b$v
    if (step < tol) break
    prev_step <- step
  }
  structure(list(value = v, policy = b$a, iterations = i, contraction_ratio = ratio, converged = step < tol),
            theorem = "Research.P4.Bellman.bellman_contracting")
}

#' The greedy allocation loses to the patient one when the discount is high
#'
#' Greedy: reward 1 now then 1/2 forever; patient: 0 now then 1 forever. Values
#' \eqn{1 + \gamma/(2(1-\gamma))} and \eqn{\gamma/(1-\gamma)}; the patient action
#' wins iff \eqn{\gamma > 2/3} (\code{Research.P4.Bellman.myopic_suboptimal}).
#'
#' @param gamma Discount values in [0, 1).
#'
#' @return A data frame: \code{gamma}, \code{greedy}, \code{patient},
#'   \code{patient_better}.
#'
#' @references Puterman ML (1994). Markov Decision Processes, ch. 6. Wiley.
#' @examples
#' morie_myopic_example(gamma = c(0.5, 2 / 3, 0.9))
#' @export
morie_myopic_example <- function(gamma) {
  if (any(gamma < 0 | gamma >= 1)) stop("`gamma` must lie in [0, 1)", call. = FALSE)
  g <- 1 + gamma * (1 / 2) / (1 - gamma)
  pt <- gamma / (1 - gamma)
  structure(data.frame(gamma = gamma, greedy = g, patient = pt, patient_better = pt > g),
            theorem = "Research.P4.Bellman.myopic_suboptimal")
}

#' The budget-constrained optimal rule is a threshold on the conditional effect
#'
#' Covariate cells with masses \code{m} and effects \code{tau}, budget
#' \code{kappa}: treat cells above the threshold, none below, and a fraction of
#' the tied cell so the budget binds; this maximises the total effect over all
#' feasible fractional rules (\code{Research.P5.BudgetRule.threshold_optimal}).
#' When the tied cell is only partly treated no deterministic rule attains the
#' optimum (\code{stochastic_needed}).
#'
#' @param m Cell masses, positive.
#'
#' @param tau Cell effects.
#'
#' @param kappa Budget.
#'
#' @return A list: \code{rule} (treated share per cell), \code{threshold},
#'   \code{value}, \code{best_deterministic} (the best over all-or-nothing rules
#'   when the number of cells is at most 15) and \code{stochastic_needed}.
#'
#' @references Luedtke AR, van der Laan MJ (2016). Optimal individualized treatments
#'   in resource-limited settings. International Journal of Biostatistics 12(1).
#' @examples
#' morie_budget_rule(m = c(1, 1, 1), tau = c(0.5, 0.2, -0.1), kappa = 1.5)
#' @export
morie_budget_rule <- function(m, tau, kappa) {
  if (length(m) != length(tau) || any(m <= 0) || kappa < 0) stop("`m` positive, same length as `tau`, `kappa` non-negative", call. = FALSE)
  o <- order(tau, decreasing = TRUE)
  d <- numeric(length(m))
  left <- kappa
  thr <- 0
  for (i in o) {
    if (tau[i] <= 0) break
    take <- min(1, left / m[i])
    d[i] <- take
    left <- left - take * m[i]
    thr <- tau[i]
    if (take < 1) break
  }
  value <- sum(m * d * tau)
  out <- list(rule = d, threshold = thr, value = value)
  if (length(m) <= 15L) {
    grid <- as.matrix(expand.grid(rep(list(c(0, 1)), length(m))))
    feas <- grid %*% m <= kappa + 1e-12
    vals <- grid %*% (m * tau)
    out$best_deterministic <- max(vals[feas])
    out$stochastic_needed <- out$best_deterministic < value - 1e-12
  }
  structure(out, theorem = "Research.P5.BudgetRule.threshold_optimal")
}

#' Mean run length of a restarted detector with false-alarm probability alpha
#'
#' With \eqn{P(\tau < \infty) \le \alpha} for each restarted copy,
#' \eqn{P(N \le n) \le (n+1)\alpha} (\code{Research.P10.Cusum.stop_prob_le}) and
#' \eqn{E N \ge \sum_{n<J}(1-n\alpha) \ge J/2} for any \eqn{J\alpha \le 1}
#' (\code{mean_run_length_ge}).
#'
#' @param alpha False-alarm probability in (0, 1].
#'
#' @param J Horizon; default \eqn{\lfloor 1/\alpha \rfloor}.
#'
#' @return A list: \code{J}, \code{partial_sum}, \code{bound} (\code{J / 2}).
#'
#' @references Lai TL (1995). Sequential changepoint detection in quality control and
#'   dynamical systems. JRSS-B 57(4).
#' @examples
#' morie_run_length_bound(alpha = 0.01)
#' @export
morie_run_length_bound <- function(alpha, J = floor(1 / alpha)) {
  if (alpha <= 0 || alpha > 1) stop("`alpha` must be in (0, 1]", call. = FALSE)
  if (J * alpha > 1) stop("`J * alpha` must not exceed 1", call. = FALSE)
  n <- seq_len(J) - 1
  structure(list(J = J, partial_sum = sum(1 - n * alpha), bound = J / 2),
            theorem = "Research.P10.Cusum.mean_run_length_ge")
}
