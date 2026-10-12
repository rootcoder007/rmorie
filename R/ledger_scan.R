# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research ledger, the library-scan closures of 2026-10-12: twelve results with their R side.
# Every statement is a machine-checked theorem (Lean 4 + Mathlib, 0 sorry, standard axioms
# only) in research/lean/Researchproofs/:
#
#   P5Attenuation.lean       Research.P5.Attenuation.single_attenuation, two_regressor_coeffs,
#                            error_free_biased_iff, bias_sign                        (scan A4)
#   P4Friedman.lean          Research.P4.Friedman.friedman_step, difference_step, meanDiff_succ,
#                            difference_mean_tendsto, mean_share_tendsto_half        (scan C2)
#   P4Ruin.lean              Research.P4.Ruin.deMoivre_step, ruin_boundary, ruin_harmonic,
#                            optional_stopping_identity, ruin_symmetric              (scan C4)
#   P7Cox.lean               Research.P7.Cox.total_variance, variance_ge_mean,
#                            equality_iff_constant, zip_zero_eq_poisson, llr_zero (scan C12, B24)
#   P1Nonresponse.lean       Research.P1.Nonresponse.region_lower, region_upper, region_sharp,
#                            region_width                                           (scan B10)
#   P8IVInequality.lean      Research.P8.IVInequality.instrumental_inequality         (scan B12)
#   P2Misclassification.lean Research.P2.Misclassification.binary_attenuation, binary_sign,
#                            ordinal_witness, ordinal_reversal                       (scan B14)
#   P11Amplification.lean    Research.P11.Amplification.symmetric, ge_one, le_min, mono_left,
#                            level_curve                                             (scan B18)
#   P13Design.lean           Research.P13.Design.quad_form, cluster_level, within_balanced,
#                            quad_indicator, icc_lower_bound                   (scan A10, B9)
#   P3Basu.lean              Research.P3.Basu.ht_mean, ht_variance, elephant_unbounded,
#                            ratio_bounded                                           (scan B8)
#   P4Contraction.lean       Research.P4.Contraction.sup_contraction, spreading_conserves,
#                            update_total, decay, feedback_gain                      (scan C7)
#   P8Inspection.lean        Research.P8.Inspection.payoff_zero_sum, interior_fixed_point,
#                            conserved, simplex_invariant, only_interior_rest  (scan A18, C24)

#' Measurement error attenuates only in the one-regressor case
#'
#' With a true regressor of variance \code{s11} measured with classical error of
#' variance \code{su2}, the one-regressor population slope is
#' \eqn{\beta_1 s_{11}/(s_{11}+s_{u}^2)}: attenuation by the reliability ratio
#' (\code{Research.P5.Attenuation.single_attenuation}). With a second,
#' error-free regressor of variance \code{s22} and covariance \code{s12}, the
#' population coefficients are \eqn{b_1 = \beta_1 (s_{11}s_{22}-s_{12}^2)/D^*}
#' and \eqn{b_2 = \beta_2 + \beta_1 s_u^2 s_{12}/D^*} with
#' \eqn{D^* = (s_{11}+s_u^2)s_{22}-s_{12}^2} (\code{two_regressor_coeffs}): the
#' error-free coefficient is biased unless \eqn{\beta_1}, \eqn{s_u^2} or
#' \eqn{s_{12}} is zero (\code{error_free_biased_iff}), and the bias has the sign
#' of \eqn{\beta_1 s_{12}} (\code{bias_sign}), away from zero as often as toward it.
#'
#' @param beta1,beta2 True coefficients of the error-prone and the error-free
#'   regressor.
#'
#' @param s11,s22 Variances of the two true regressors.
#'
#' @param s12 Their covariance; must satisfy \code{s12^2 < s11 * s22}.
#'
#' @param su2 Variance of the classical measurement error on the first regressor.
#'
#' @return A list: \code{slope_single} (the one-regressor slope on the observed
#'   regressor), \code{reliability} (\code{s11 / (s11 + su2)}), \code{b1},
#'   \code{b2} (the two-regressor population coefficients), \code{bias_b2}
#'   (\code{b2 - beta2}), \code{bias_b2_sign} and \code{dstar}.
#'
#' @references Weisburd D, Britt C (2014). Statistics in Criminal Justice, 4th ed.,
#'   ch. 2. Springer.
#' @examples
#' morie_attenuation_two_regressor(beta1 = 1, beta2 = 0.5, s11 = 1, s22 = 1, s12 = 0.6, su2 = 0.5)
#' @export
morie_attenuation_two_regressor <- function(beta1, beta2, s11, s22, s12 = 0, su2 = 0) {
  if (s11 <= 0 || s22 <= 0) stop("`s11` and `s22` must be positive variances", call. = FALSE)
  if (su2 < 0) stop("`su2` must be non-negative", call. = FALSE)
  if (s12^2 >= s11 * s22) stop("`s12` must satisfy s12^2 < s11 * s22", call. = FALSE)
  dstar <- (s11 + su2) * s22 - s12^2
  b1 <- beta1 * (s11 * s22 - s12^2) / dstar
  b2 <- beta2 + beta1 * su2 * s12 / dstar
  bias <- b2 - beta2
  structure(list(
    slope_single = beta1 * s11 / (s11 + su2),
    reliability = s11 / (s11 + su2),
    b1 = b1, b2 = b2, bias_b2 = bias,
    bias_b2_sign = if (bias > 0) "positive" else if (bias < 0) "negative" else "zero",
    dstar = dstar
  ), theorem = "Research.P5.Attenuation.two_regressor_coeffs")
}

#' The Friedman urn: cross-colour reinforcement pulls the share to one half
#'
#' A ball is drawn and one of the other colour is added. With \code{b0} blue and
#' \code{r0} red balls (total \eqn{T_0}), the expected difference after \eqn{n}
#' steps is \eqn{D_0 (T_0-1)/(T_0+n-1)}, which solves the one-step recursion
#' \eqn{E[D_{n+1}] = E[D_n](1 - 1/T_n)} (\code{Research.P4.Friedman.meanDiff_succ})
#' and tends to zero (\code{difference_mean_tendsto}); the mean share
#' \eqn{1/2 + E[D_n]/(2T_n)} tends to one half (\code{mean_share_tendsto_half}).
#' The quantity \eqn{(B-R)(B+R-1)} is a martingale (\code{friedman_step}). Same-colour
#' reinforcement (\code{\link{morie_feedback_loop_urn_law}}) gives a random limit;
#' the cross-colour rule gives a deterministic one.
#'
#' @param b0,r0 Starting counts of the two colours; \code{b0 + r0 > 1}.
#'
#' @param n Number of steps to tabulate.
#'
#' @return A data frame with one row per step: \code{step}, \code{total},
#'   \code{mean_diff} and \code{mean_share}.
#'
#' @references Grimmett G, Stirzaker D (2020). Probability and Random Processes,
#'   4th ed., Exercise 12.3.6. Oxford.
#' @examples
#' tail(morie_friedman_urn(b0 = 9, r0 = 1, n = 50), 3)
#' @export
morie_friedman_urn <- function(b0, r0, n = 100L) {
  if (b0 < 0 || r0 < 0 || b0 + r0 <= 1) stop("`b0 + r0` must exceed 1", call. = FALSE)
  n <- as.integer(n)
  if (n < 0L) stop("`n` must be non-negative", call. = FALSE)
  T0 <- b0 + r0
  D0 <- b0 - r0
  step <- 0:n
  mean_diff <- D0 * (T0 - 1) / (T0 + step - 1)
  structure(data.frame(
    step = step, total = T0 + step, mean_diff = mean_diff,
    mean_share = 1 / 2 + mean_diff / (2 * (T0 + step))
  ), theorem = "Research.P4.Friedman.meanDiff_succ")
}

#' Absorption probability of a finite-capacity allocation
#'
#' A resource moves one unit at a time toward the first of two regions with
#' probability \code{p}, and stops when a region holds nothing or everything
#' (\code{N}). With \eqn{\rho = (1-p)/p}, the probability of ending at zero from
#' \eqn{k} is \eqn{(\rho^k - \rho^N)/(1 - \rho^N)}, which solves the harmonic
#' equation \eqn{p_k = p\,p_{k+1} + (1-p)\,p_{k-1}} with \eqn{p_0 = 1},
#' \eqn{p_N = 0} (\code{Research.P4.Ruin.ruin_harmonic}, \code{ruin_boundary}) by
#' the de Moivre martingale \eqn{\rho^{S_n}} (\code{deMoivre_step}). At
#' \code{p = 1/2} the formula is 0/0 and the answer is \eqn{1 - k/N}
#' (\code{ruin_symmetric}).
#'
#' @param p Probability of a step toward the first region, in (0, 1).
#'
#' @param N Capacity (absorbing upper state).
#'
#' @param k Starting states; default all of \code{0:N}.
#'
#' @return A numeric vector of absorption-at-zero probabilities, one per \code{k},
#'   with \code{rho} as an attribute.
#'
#' @references Grimmett G, Stirzaker D (2020). Probability and Random Processes,
#'   4th ed., Example 12.5.8, Theorems 12.5.1 and 12.5.9. Oxford.
#' @examples
#' morie_ruin_probability(p = 0.4, N = 10)
#' @export
morie_ruin_probability <- function(p, N, k = 0:N) {
  if (p <= 0 || p >= 1) stop("`p` must be in (0, 1)", call. = FALSE)
  if (N < 1 || any(k < 0) || any(k > N)) stop("`k` must lie in 0..N", call. = FALSE)
  rho <- (1 - p) / p
  out <- if (abs(p - 0.5) < 1e-12) 1 - k / N else (rho^k - rho^N) / (1 - rho^N)
  structure(out, rho = rho, theorem = "Research.P4.Ruin.ruin_harmonic")
}

#' Any intensity heterogeneity forces overdispersion
#'
#' A count whose intensity is drawn from a finite mixture (weights \code{w},
#' intensities \code{lam}) has \eqn{Var\,N = E\,N + Var\,\Lambda}
#' (\code{Research.P7.Cox.total_variance}), so the variance is at least the mean
#' (\code{variance_ge_mean}) with equality iff the intensity is constant on the
#' support (\code{equality_iff_constant}). A Gini or zero share above the Poisson
#' null is therefore implied by any random intensity and is not by itself
#' evidence of contagion.
#'
#' @param w Mixture weights (normalised to sum to one).
#'
#' @param lam Non-negative intensities, one per weight.
#'
#' @return A list: \code{mean}, \code{variance}, \code{intensity_var},
#'   \code{dispersion} (variance over mean) and \code{constant_intensity}.
#'
#' @references Grimmett G, Stirzaker D (2020). Probability and Random Processes,
#'   4th ed., Problem 6.15.22. Oxford.
#' @examples
#' morie_cox_overdispersion(w = c(0.7, 0.3), lam = c(1, 5))
#' @export
morie_cox_overdispersion <- function(w, lam) {
  if (length(w) != length(lam) || any(w < 0) || sum(w) <= 0) stop("`w` must be non-negative weights", call. = FALSE)
  if (any(lam < 0)) stop("`lam` must be non-negative", call. = FALSE)
  w <- w / sum(w)
  m <- sum(w * lam)
  iv <- sum(w * (lam - m)^2)
  v <- sum(w * (lam + lam^2)) - m^2
  structure(list(
    mean = m, variance = v, intensity_var = iv,
    dispersion = if (m > 0) v / m else NA_real_,
    constant_intensity = all(abs(lam[w > 0] - m) < 1e-12)
  ), theorem = "Research.P7.Cox.total_variance")
}

#' Vuong's test of zero-inflated against plain Poisson at the boundary
#'
#' At \eqn{\pi = 0} the zero-inflated Poisson pmf is the Poisson pmf at every
#' count (\code{Research.P7.Cox.zip_zero_eq_poisson}), so every pointwise
#' log-likelihood ratio is zero (\code{llr_zero}), their sample standard deviation
#' is zero and the Vuong statistic is 0/0, not standard normal.
#'
#' @param counts Observed counts.
#'
#' @param lam Poisson rate; default the sample mean.
#'
#' @return A list: \code{llr} (all zero), \code{sd} (zero) and \code{statistic}
#'   (\code{NA}).
#'
#' @references Friendly M, Meyer D (2016). Discrete Data Analysis with R, section 11.2.
#'   CRC.
#' @examples
#' morie_zip_vuong_boundary(c(0, 0, 1, 2, 0, 3))
#' @export
morie_zip_vuong_boundary <- function(counts, lam = mean(counts)) {
  if (any(counts < 0) || any(counts != round(counts))) stop("`counts` must be non-negative integers", call. = FALSE)
  pois <- stats::dpois(counts, lam)
  zip <- 0 + (1 - 0) * pois
  llr <- log(zip) - log(pois)
  structure(list(llr = llr, sd = stats::sd(llr), statistic = NA_real_),
            theorem = "Research.P7.Cox.llr_zero")
}

#' The identification region of a proportion under nonresponse
#'
#' With reporting (or response) rate \code{r} and observed share \code{p_obs}
#' among those who report, the population proportion lies in
#' \eqn{[r p_{obs},\; r p_{obs} + 1 - r]}, both ends attained
#' (\code{Research.P1.Nonresponse.region_sharp}), of width exactly \eqn{1 - r}
#' (\code{region_width}). With \eqn{r = 0.106} the region has width 0.894.
#'
#' @param p_obs Observed share among reporters, in \eqn{[0, 1]}.
#'
#' @param r Reporting rate, in \eqn{[0, 1]}; recycled against \code{p_obs}.
#'
#' @return A data frame: \code{p_obs}, \code{r}, \code{lower}, \code{upper},
#'   \code{width}.
#'
#' @references Manski CF (2003). Partial Identification of Probability Distributions,
#'   ch. 2 eq. 2.4. Springer. Lohr SL (2019). Sampling: Design and Analysis, 2nd ed.,
#'   section 8.1. CRC.
#' @examples
#' morie_nonresponse_region(p_obs = 0.4, r = c(0.106, 0.5, 0.9))
#' @export
morie_nonresponse_region <- function(p_obs, r) {
  if (any(p_obs < 0 | p_obs > 1) || any(r < 0 | r > 1)) stop("`p_obs` and `r` must lie in [0, 1]", call. = FALSE)
  n <- max(length(p_obs), length(r))
  p_obs <- rep_len(p_obs, n)
  r <- rep_len(r, n)
  structure(data.frame(p_obs = p_obs, r = r, lower = r * p_obs, upper = r * p_obs + 1 - r, width = 1 - r),
            theorem = "Research.P1.Nonresponse.region_width")
}

#' Pearl's instrumental inequality on a finite table
#'
#' For a finite instrument \eqn{Z}, treatment \eqn{X} and outcome \eqn{Y} with a
#' latent confounder independent of \eqn{Z}, the observed law obeys
#' \eqn{\sum_y \max_z P(y, x \mid z) \le 1} for every \eqn{x}
#' (\code{Research.P8.IVInequality.instrumental_inequality}). A table that violates
#' it refutes the instrument.
#'
#' @param p A three-way array of \eqn{P(y, x \mid z)} with dimensions
#'   (y, x, z); every z-slice sums to one.
#'
#' @param tol Tolerance for the slice sums and the test.
#'
#' @return A list: \code{per_x} (the sums \eqn{\sum_y \max_z P(y,x|z)}),
#'   \code{max} and \code{passes}.
#'
#' @references Pearl J (2009). Causality, 2nd ed., section 8.4. Cambridge.
#' @examples
#' p <- array(c(0.4, 0.1, 0.3, 0.2, 0.2, 0.3, 0.1, 0.4), dim = c(2, 2, 2))
#' morie_instrumental_inequality(p)
#' @export
morie_instrumental_inequality <- function(p, tol = 1e-8) {
  if (!is.array(p) || length(dim(p)) != 3L || any(p < 0)) stop("`p` must be a non-negative (y, x, z) array", call. = FALSE)
  slice <- apply(p, 3, sum)
  if (any(abs(slice - 1) > tol)) stop("every z-slice of `p` must sum to one", call. = FALSE)
  m <- apply(p, c(1, 2), max)
  per_x <- colSums(m)
  structure(list(per_x = per_x, max = max(per_x), passes = all(per_x <= 1 + tol)),
            theorem = "Research.P8.IVInequality.instrumental_inequality")
}

#' Nondifferential misclassification of a binary exposure
#'
#' With true risks \code{r1}, \code{r0}, prevalence \code{prevalence},
#' sensitivity and specificity, the measured risk difference is
#' \eqn{(se+sp-1)(r_1-r_0)\,\pi(1-\pi)/(d_1 d_0)} with \eqn{d_1, d_0} the measured
#' group shares (\code{Research.P2.Misclassification.binary_attenuation}), so it
#' keeps the sign of the true difference whenever \eqn{se + sp \ge 1}
#' (\code{binary_sign}). For an ordinal exposure see
#' \code{\link{morie_misclassified_contrast}}.
#'
#' @param r1,r0 True outcome risks among the exposed and the unexposed.
#'
#' @param prevalence True exposure prevalence, in (0, 1).
#'
#' @param sensitivity,specificity Of the exposure measurement, in \eqn{[0, 1]}.
#'
#' @return A list: \code{true_rd}, \code{measured_rd}, \code{factor}
#'   (\code{measured_rd / true_rd}), \code{d1}, \code{d0} and \code{sign_kept}.
#'
#' @references Hernan MA, Robins JM (2020). Causal Inference: What If, Fine Point 9.1.
#'   CRC.
#' @examples
#' morie_misclassified_rd(r1 = 0.3, r0 = 0.1, prevalence = 0.4, sensitivity = 0.8, specificity = 0.9)
#' @export
morie_misclassified_rd <- function(r1, r0, prevalence, sensitivity, specificity) {
  if (prevalence <= 0 || prevalence >= 1) stop("`prevalence` must be in (0, 1)", call. = FALSE)
  if (any(c(sensitivity, specificity) < 0 | c(sensitivity, specificity) > 1)) stop("`sensitivity` and `specificity` must be in [0, 1]", call. = FALSE)
  pi <- prevalence
  se <- sensitivity
  sp <- specificity
  d1 <- pi * se + (1 - pi) * (1 - sp)
  d0 <- pi * (1 - se) + (1 - pi) * sp
  if (d1 <= 0 || d0 <= 0) stop("both measured groups must be non-empty", call. = FALSE)
  n1 <- pi * se * r1 + (1 - pi) * (1 - sp) * r0
  n0 <- pi * (1 - se) * r1 + (1 - pi) * sp * r0
  measured <- n1 / d1 - n0 / d0
  true_rd <- r1 - r0
  structure(list(
    true_rd = true_rd, measured_rd = measured,
    factor = (se + sp - 1) * pi * (1 - pi) / (d1 * d0),
    d1 = d1, d0 = d0,
    sign_kept = sign(measured) == sign(true_rd) || measured == 0
  ), theorem = "Research.P2.Misclassification.binary_attenuation")
}

#' Misclassification of an ordinal exposure can reverse a contrast
#'
#' Each true level has a risk and a weight; the measurement sends levels to a
#' measured group. The measured contrast between two groups is the difference of
#' their weighted mean risks, which can be zero or of the opposite sign to the
#' true top-versus-bottom contrast when the map is not monotone
#' (\code{Research.P2.Misclassification.ordinal_witness},
#' \code{ordinal_reversal}). The binary attenuation theorem is a binary theorem.
#'
#' @param risks Outcome risk at each true level, ordered from lowest to highest
#'   level.
#'
#' @param group Measured group label of each level.
#'
#' @param weights Prevalence weight of each level.
#'
#' @param high,low The two measured groups to contrast.
#'
#' @return A list: \code{true_contrast} (top minus bottom level),
#'   \code{measured_contrast} (\code{high} minus \code{low}), \code{group_risk}
#'   and \code{reversed}.
#'
#' @references Hernan MA, Robins JM (2020). Causal Inference: What If, Technical
#'   Point 9.1. CRC.
#' @examples
#' morie_misclassified_contrast(risks = c(0, 0.5, 1), group = c("high", "low", "high"))
#' @export
morie_misclassified_contrast <- function(risks, group, weights = rep(1, length(risks)),
                                         high = "high", low = "low") {
  if (length(risks) < 2L || length(group) != length(risks) || length(weights) != length(risks)) {
    stop("`risks`, `group` and `weights` must have the same length, at least 2", call. = FALSE)
  }
  if (!all(c(high, low) %in% group)) stop("`high` and `low` must both appear in `group`", call. = FALSE)
  gr <- tapply(weights * risks, group, sum) / tapply(weights, group, sum)
  true_c <- risks[length(risks)] - risks[1L]
  meas <- unname(gr[[high]] - gr[[low]])
  structure(list(
    true_contrast = true_c, measured_contrast = meas, group_risk = gr,
    reversed = sign(meas) != sign(true_c)
  ), theorem = "Research.P2.Misclassification.ordinal_witness")
}

#' Amplification of a sensitivity parameter: one Gamma is a curve of (Lambda, Delta)
#'
#' \eqn{\Gamma = (\Lambda\Delta + 1)/(\Lambda + \Delta)} with \eqn{\Lambda} the
#' odds of treatment and \eqn{\Delta} the odds of a positive outcome attributable
#' to the unobserved covariate. The map is symmetric, at least one, at most
#' \eqn{\min(\Lambda, \Delta)} and non-decreasing in each argument
#' (\code{Research.P11.Amplification.symmetric}, \code{ge_one}, \code{le_min},
#' \code{mono_left}); every \eqn{\Gamma \ge 1} is a curve, parametrised by
#' \eqn{\Lambda > \Gamma} through \eqn{\Delta = (\Lambda\Gamma - 1)/(\Lambda - \Gamma)}
#' (\code{level_curve}). Give any two of the three.
#'
#' @param Lambda,Delta,Gamma Two of the three (each at least 1); the third is
#'   computed. \code{Lambda} and \code{Delta} may be vectors.
#'
#' @return A data frame with columns \code{Lambda}, \code{Delta}, \code{Gamma}.
#'
#' @references Rosenbaum PR, Silber JH (2009). Amplification of sensitivity analysis
#'   in matched observational studies. JASA 104(488).
#' @examples
#' morie_sensitivity_amplification(Lambda = c(2, 3, 5), Delta = 4)
#' morie_sensitivity_amplification(Gamma = 2, Lambda = c(3, 4, 10))
#' @export
morie_sensitivity_amplification <- function(Lambda = NULL, Delta = NULL, Gamma = NULL) {
  given <- !c(is.null(Lambda), is.null(Delta), is.null(Gamma))
  if (sum(given) != 2L) stop("give exactly two of `Lambda`, `Delta`, `Gamma`", call. = FALSE)
  if (is.null(Gamma)) {
    if (any(Lambda < 1) || any(Delta < 1)) stop("`Lambda` and `Delta` must be at least 1", call. = FALSE)
    Gamma <- (Lambda * Delta + 1) / (Lambda + Delta)
  } else if (is.null(Delta)) {
    if (any(Gamma < 1) || any(Lambda <= Gamma)) stop("`Lambda` must exceed `Gamma` (at least 1)", call. = FALSE)
    Delta <- (Lambda * Gamma - 1) / (Lambda - Gamma)
  } else {
    if (any(Gamma < 1) || any(Delta <= Gamma)) stop("`Delta` must exceed `Gamma` (at least 1)", call. = FALSE)
    Lambda <- (Delta * Gamma - 1) / (Delta - Gamma)
  }
  structure(data.frame(Lambda = Lambda, Delta = Delta, Gamma = Gamma),
            theorem = "Research.P11.Amplification.level_curve")
}

#' The design effect of clustering is direction-specific
#'
#' Errors in a cluster sample of \eqn{M} units per cluster have exchangeable
#' covariance with intraclass correlation \eqn{\rho}. The quadratic form of a
#' regressor \eqn{x} is \eqn{(1-\rho)\sum x^2 + \rho\sum_c(\sum_j x_{cj})^2}
#' (\code{Research.P13.Design.quad_form}): a cluster-level regressor inflates the
#' naive variance by \eqn{1 + (M-1)\rho} (\code{cluster_level}), a
#' within-cluster-balanced regressor deflates it by \eqn{1 - \rho}
#' (\code{within_balanced}), and non-negativity at a one-cluster indicator forces
#' \eqn{\rho \ge -1/(M-1)} (\code{icc_lower_bound}).
#'
#' @param rho Intraclass correlation, in \eqn{[-1/(M-1), 1]}.
#'
#' @param M Units per cluster (at least 2).
#'
#' @param x Optional regressor matrix with one row per cluster and \code{M}
#'   columns; its quadratic form and variance ratio are returned.
#'
#' @return A list: \code{deff_cluster_level} (\code{1 + (M - 1) * rho}),
#'   \code{deff_within_balanced} (\code{1 - rho}), \code{icc_floor} and, with
#'   \code{x}, \code{quad}, \code{naive} (\code{sum(x^2)}) and \code{ratio}.
#'
#' @references Lohr SL (2019). Sampling: Design and Analysis, 2nd ed., section 5.2.
#'   CRC.
#' @examples
#' morie_design_effect_direction(rho = 0.1, M = 20)
#' x <- matrix(c(1, 1, 1, -1, -1, -1), nrow = 2, byrow = TRUE)
#' morie_design_effect_direction(rho = 0.1, M = 3, x = x)
#' @export
morie_design_effect_direction <- function(rho, M, x = NULL) {
  if (M < 2) stop("`M` must be at least 2", call. = FALSE)
  floor_rho <- -1 / (M - 1)
  if (rho < floor_rho || rho > 1) stop("`rho` must lie in [-1/(M - 1), 1]", call. = FALSE)
  out <- list(deff_cluster_level = 1 + (M - 1) * rho, deff_within_balanced = 1 - rho, icc_floor = floor_rho)
  if (!is.null(x)) {
    x <- as.matrix(x)
    if (ncol(x) != M) stop("`x` must have `M` columns", call. = FALSE)
    out$quad <- (1 - rho) * sum(x^2) + rho * sum(rowSums(x)^2)
    out$naive <- sum(x^2)
    out$ratio <- out$quad / out$naive
  }
  structure(out, theorem = "Research.P13.Design.quad_form")
}

#' Basu's elephant: the Horvitz-Thompson total is unbiased with exploding variance
#'
#' Under independent inclusion with probabilities \code{pi}, the Horvitz-Thompson
#' total has mean \eqn{\sum t_i} (\code{Research.P3.Basu.ht_mean}) and variance
#' \eqn{\sum t_i^2(1-\pi_i)/\pi_i} (\code{ht_variance}). One unit with value
#' \eqn{t_e} and inclusion probability \eqn{\pi_e = t_e^2/(2(B + t_e^2))}
#' contributes more than any bound \eqn{B} (\code{elephant_unbounded}); the other
#' units' contribution is bounded without reference to \eqn{\pi_e}
#' (\code{ratio_bounded}).
#'
#' @param t Unit values.
#'
#' @param pi Inclusion probabilities in (0, 1].
#'
#' @param bound Optional variance bound \eqn{B}; the inclusion probability at
#'   which the largest unit alone exceeds it is returned.
#'
#' @return A list: \code{mean}, \code{variance}, \code{terms} (per unit) and,
#'   with \code{bound}, \code{elephant} (its index), \code{pi_elephant} and
#'   \code{term_elephant}.
#'
#' @references Lohr SL (2019). Sampling: Design and Analysis, 2nd ed., section 6.7.
#'   CRC.
#' @examples
#' morie_ht_elephant(t = c(1, 2, 3, 100), pi = c(0.5, 0.5, 0.5, 0.01), bound = 1e4)
#' @export
morie_ht_elephant <- function(t, pi, bound = NULL) {
  if (length(t) != length(pi) || any(pi <= 0) || any(pi > 1)) stop("`pi` must be in (0, 1], one per unit", call. = FALSE)
  terms <- t^2 * (1 - pi) / pi
  out <- list(mean = sum(t), variance = sum(terms), terms = terms)
  if (!is.null(bound)) {
    if (bound <= 0) stop("`bound` must be positive", call. = FALSE)
    e <- which.max(abs(t))
    te <- t[e]
    pe <- te^2 / (2 * (bound + te^2))
    out$elephant <- e
    out$pi_elephant <- pe
    out$term_elephant <- te^2 * (1 - pe) / pe
  }
  structure(out, theorem = "Research.P3.Basu.ht_variance")
}

#' The discrete attractiveness field contracts absent crime
#'
#' The lattice update \eqn{B' = (1-\omega)[(1-\eta)B + \eta S B]} with \eqn{S} a
#' row-stochastic spreading matrix and no events contracts the sup norm by
#' \eqn{1-\omega} at every step (\code{Research.P4.Contraction.sup_contraction},
#' \code{decay}); with \eqn{S} doubly stochastic the total is scaled by exactly
#' \eqn{1-\omega} (\code{update_total}). With event feedback \eqn{\epsilon\rho}
#' the uniform mode grows iff \eqn{\epsilon\rho > \omega/(1-\omega)}
#' (\code{feedback_gain}).
#'
#' @param S Non-negative spreading matrix with unit row sums.
#'
#' @param omega,eta Decay and spreading rates, in \eqn{[0, 1]}.
#'
#' @param B Starting field, one value per site.
#'
#' @param n Number of updates.
#'
#' @return A list: \code{trajectory} (a matrix with \code{n + 1} rows),
#'   \code{sup_norm}, \code{sup_bound} (\eqn{(1-\omega)^k} times the starting
#'   norm), \code{total}, \code{doubly_stochastic} and
#'   \code{feedback_threshold} (\eqn{\omega/(1-\omega)}).
#'
#' @references D'Orsogna MR, Perc M (2015). Statistical physics of crime: a review.
#'   Physics of Life Reviews 12, eq. 2.
#' @examples
#' S <- matrix(c(0.5, 0.5, 0, 0.5, 0, 0.5, 0, 0.5, 0.5), nrow = 3, byrow = TRUE)
#' morie_attractiveness_contraction(S, omega = 0.2, eta = 0.3, B = c(5, 1, 0), n = 3)
#' @export
morie_attractiveness_contraction <- function(S, omega, eta, B, n = 10L) {
  S <- as.matrix(S)
  if (nrow(S) != ncol(S) || any(S < 0) || any(abs(rowSums(S) - 1) > 1e-10)) stop("`S` must be square, non-negative, with unit row sums", call. = FALSE)
  if (length(B) != nrow(S)) stop("`B` must have one value per site", call. = FALSE)
  if (omega < 0 || omega > 1 || eta < 0 || eta > 1) stop("`omega` and `eta` must lie in [0, 1]", call. = FALSE)
  n <- as.integer(n)
  traj <- matrix(NA_real_, n + 1L, length(B))
  traj[1L, ] <- B
  for (k in seq_len(n)) {
    B <- (1 - omega) * ((1 - eta) * B + eta * as.numeric(S %*% B))
    traj[k + 1L, ] <- B
  }
  sup <- apply(abs(traj), 1, max)
  structure(list(
    trajectory = traj, sup_norm = sup, sup_bound = (1 - omega)^(0:n) * sup[1L],
    total = rowSums(traj),
    doubly_stochastic = all(abs(colSums(S) - 1) < 1e-10),
    feedback_threshold = if (omega < 1) omega / (1 - omega) else Inf
  ), theorem = "Research.P4.Contraction.decay")
}

#' The criminal-citizen-inspector game cycles
#'
#' Replicator dynamics for a rock-paper-scissors payoff with stake \code{a} (each
#' strategy beats the next) has \eqn{x^T A x = 0} on the simplex
#' (\code{Research.P8.Inspection.payoff_zero_sum}), the centroid as its only
#' interior rest point (\code{interior_fixed_point}, \code{only_interior_rest}),
#' and \eqn{V = x_1 x_2 x_3} as a conserved quantity (\code{conserved}): orbits are
#' closed curves around the centroid, so more inspection does not monotonically
#' reduce crime.
#'
#' @param a Stake (non-zero).
#'
#' @param x Starting point on the simplex (three non-negative shares summing to one).
#'
#' @param steps,dt Number and size of fourth-order Runge-Kutta steps.
#'
#' @return A list: \code{trajectory} (a matrix with \code{steps + 1} rows),
#'   \code{invariant} (\eqn{V} along the orbit), \code{invariant_drift}
#'   (its largest deviation from the start), \code{dV_dt} (the exact derivative at
#'   the start, zero), \code{field} (the vector field at the start) and
#'   \code{fixed_point}.
#'
#' @references D'Orsogna MR, Perc M (2015). Statistical physics of crime: a review.
#'   Physics of Life Reviews 12, section IV, eq. 6.
#' @examples
#' g <- morie_inspection_game(a = 1, x = c(0.5, 0.3, 0.2), steps = 200)
#' g$invariant_drift
#' @export
morie_inspection_game <- function(a, x, steps = 1000L, dt = 0.01) {
  if (a == 0) stop("`a` must be non-zero", call. = FALSE)
  if (length(x) != 3L || any(x < 0) || abs(sum(x) - 1) > 1e-10) stop("`x` must be three shares summing to one", call. = FALSE)
  field <- function(x) c(x[1] * a * (x[2] - x[3]), x[2] * a * (x[3] - x[1]), x[3] * a * (x[1] - x[2]))
  f0 <- field(x)
  dv <- x[2] * x[3] * f0[1] + x[1] * x[3] * f0[2] + x[1] * x[2] * f0[3]
  steps <- as.integer(steps)
  traj <- matrix(NA_real_, steps + 1L, 3L)
  traj[1L, ] <- x
  for (k in seq_len(steps)) {
    k1 <- field(x)
    k2 <- field(x + dt / 2 * k1)
    k3 <- field(x + dt / 2 * k2)
    k4 <- field(x + dt * k3)
    x <- x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    traj[k + 1L, ] <- x
  }
  v <- traj[, 1] * traj[, 2] * traj[, 3]
  structure(list(
    trajectory = traj, invariant = v, invariant_drift = max(abs(v - v[1L])),
    dV_dt = dv, field = f0, fixed_point = rep(1 / 3, 3)
  ), theorem = "Research.P8.Inspection.conserved")
}
