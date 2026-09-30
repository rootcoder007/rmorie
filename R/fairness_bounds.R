# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P5: what can be certified about the fairness of a risk score.
#
# Every identity and bound reported here is a machine-checked theorem in
# research/lean/P5Fairness.lean (Lean 4 + Mathlib, 0 sorry, standard
# axioms only):
#
#   Research.P5.Table.chouldechova     fpr = p/(1-p) * (1-ppv)/ppv * (1-fnr)
#   Research.P5.impossibility          equal ppv, equal fnr, p_A != p_B  =>  fpr_A != fpr_B
#   Research.P5.true_base_rate_bounds  with noise alpha in [0, a], beta in [0, b], a + b < 1:
#                                      (p_obs - a)/(1 - a) <= p <= p_obs/(1 - b)
#   Research.P5.lower_bound_attained / upper_bound_attained: both ends are reached
#   Research.P5.compare_decided        disjoint intervals order the true rates for every admissible noise
#   Research.P5.compare_undecided      overlapping intervals admit noise pairs in either order
#
# The theorems are arithmetic on rates; whether the noise boxes are the
# right ones for a jurisdiction is a substantive claim no proof supplies.

#' Group-wise rates from a confusion table
#'
#' @param tp,fp,fn,tn Cell counts (or masses) of the confusion table for
#'   one group; all positive.
#' @return A list with \code{p} (base rate), \code{ppv}, \code{fpr},
#'   \code{fnr} and \code{n}.
#' @examples
#' morie_fairness_rates(tp = 120, fp = 60, fn = 40, tn = 280)
#' @export
morie_fairness_rates <- function(tp, fp, fn, tn) {
  for (v in list(tp, fp, fn, tn)) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0) {
      stop("tp, fp, fn and tn must be single positive numbers", call. = FALSE)
    }
  }
  n <- tp + fp + fn + tn
  list(p = (tp + fn) / n, ppv = tp / (tp + fp), fpr = fp / (fp + tn),
       fnr = fn / (tp + fn), n = n)
}

#' The false positive rate implied by base rate, PPV and FNR
#'
#' Chouldechova's identity: \eqn{fpr = \frac{p}{1-p}\,\frac{1-ppv}{ppv}\,(1-fnr)}
#' (\code{Research.P5.Table.chouldechova}). Because it is an identity, two
#' groups that share \code{ppv} and \code{fnr} but differ in base rate
#' cannot share \code{fpr} (\code{Research.P5.impossibility}): no risk
#' score, however built, escapes it.
#'
#' @param p Base rate in (0, 1).
#' @param ppv Positive predictive value in (0, 1].
#' @param fnr False negative rate in [0, 1).
#' @return The false positive rate the three imply.
#' @examples
#' r <- morie_fairness_rates(120, 60, 40, 280)
#' morie_fairness_implied_fpr(r$p, r$ppv, r$fnr)   # equals r$fpr exactly
#' # same ppv and fnr, different base rates: the fpr must differ
#' morie_fairness_implied_fpr(0.30, 0.6, 0.25)
#' morie_fairness_implied_fpr(0.50, 0.6, 0.25)
#' @export
morie_fairness_implied_fpr <- function(p, ppv, fnr) {
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || p <= 0 || p >= 1) {
    stop("p must be a single number in (0, 1)", call. = FALSE)
  }
  if (!is.numeric(ppv) || length(ppv) != 1L || is.na(ppv) || ppv <= 0 || ppv > 1) {
    stop("ppv must be a single number in (0, 1]", call. = FALSE)
  }
  if (!is.numeric(fnr) || length(fnr) != 1L || is.na(fnr) || fnr < 0 || fnr >= 1) {
    stop("fnr must be a single number in [0, 1)", call. = FALSE)
  }
  p / (1 - p) * ((1 - ppv) / ppv) * (1 - fnr)
}

#' Sharp bounds on a true base rate from a noisy recorded one
#'
#' Rearrest is a proxy for reoffending. With false-positive noise
#' \eqn{\alpha = P(\text{recorded } 1 \mid \text{true } 0)} and
#' false-negative noise \eqn{\beta = P(\text{recorded } 0 \mid \text{true } 1)}
#' the recorded rate is \eqn{p_{obs} = p(1-\beta) + (1-p)\alpha}. When only
#' boxes \eqn{\alpha \in [0, a]}, \eqn{\beta \in [0, b]} with \eqn{a + b < 1}
#' are credible, the true rate is identified only up to
#' \deqn{\frac{p_{obs} - a}{1 - a} \le p \le \frac{p_{obs}}{1 - b},}
#' and both ends are attained (\code{Research.P5.true_base_rate_bounds},
#' \code{lower_bound_attained}, \code{upper_bound_attained}), so no
#' narrower interval follows from the boxes alone.
#'
#' @param p_obs Recorded (proxy) base rate, one or more values in [0, 1].
#' @param alpha_max Largest credible false-positive noise rate.
#' @param beta_max Largest credible false-negative noise rate;
#'   \code{alpha_max + beta_max} must be below 1.
#' @return A data frame with \code{p_obs}, \code{lower}, \code{upper}
#'   (clamped to [0, 1]) and \code{width}.
#' @examples
#' morie_fairness_base_rate_bounds(c(0.35, 0.55), alpha_max = 0.10, beta_max = 0.20)
#' # can two groups' true base rates still be ordered? only if the boxes do not overlap
#' @export
morie_fairness_base_rate_bounds <- function(p_obs, alpha_max, beta_max) {
  if (!is.numeric(p_obs) || any(is.na(p_obs)) || any(p_obs < 0) || any(p_obs > 1)) {
    stop("p_obs must be numeric in [0, 1]", call. = FALSE)
  }
  for (v in list(alpha_max, beta_max)) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 0) {
      stop("alpha_max and beta_max must be single non-negative numbers", call. = FALSE)
    }
  }
  if (alpha_max + beta_max >= 1) stop("alpha_max + beta_max must be below 1", call. = FALSE)
  lower <- pmax(0, (p_obs - alpha_max) / (1 - alpha_max))
  upper <- pmin(1, p_obs / (1 - beta_max))
  data.frame(p_obs = p_obs, lower = lower, upper = upper, width = upper - lower)
}

#' Recover a true base rate when the noise rates are known
#'
#' Inverts \eqn{p_{obs} = p(1-\beta) + (1-p)\alpha}
#' (\code{Research.P5.trueRate_observedRate}).
#'
#' @inheritParams morie_fairness_base_rate_bounds
#' @param alpha,beta Known noise rates with \code{alpha + beta < 1}.
#' @return The true base rate(s).
#' @examples
#' p <- 0.4
#' p_obs <- p * (1 - 0.2) + (1 - p) * 0.1
#' morie_fairness_true_rate(p_obs, alpha = 0.1, beta = 0.2)   # 0.4
#' @export
morie_fairness_true_rate <- function(p_obs, alpha, beta) {
  if (!is.numeric(p_obs) || any(is.na(p_obs))) stop("p_obs must be numeric", call. = FALSE)
  if (!is.numeric(alpha) || !is.numeric(beta) || length(alpha) != 1L || length(beta) != 1L ||
        is.na(alpha) || is.na(beta) || alpha < 0 || beta < 0 || alpha + beta >= 1) {
    stop("alpha and beta must be non-negative with alpha + beta < 1", call. = FALSE)
  }
  (p_obs - alpha) / (1 - alpha - beta)
}

#' Can two groups' true base rates be ordered under label noise?
#'
#' Applies the sharp intervals of \code{\link{morie_fairness_base_rate_bounds}}
#' to two recorded rates. When the intervals are disjoint the order of the
#' true rates holds for every admissible noise pair
#' (\code{Research.P5.compare_decided}); when they overlap there are
#' admissible noise pairs that put either group first, so the data cannot
#' settle the order without narrower noise boxes
#' (\code{Research.P5.compare_undecided}). The function also reports the
#' largest \code{beta_max} (holding \code{alpha_max}) at which the order
#' would become decidable, a breakdown value.
#'
#' @param p_obs_a,p_obs_b Recorded base rates of the two groups.
#' @inheritParams morie_fairness_base_rate_bounds
#' @return A list with \code{decided} (logical), \code{order} (\code{"a < b"},
#'   \code{"b < a"} or \code{"undecided"}), the two intervals, and
#'   \code{breakdown_beta_max} (the largest \code{beta_max} that would make the
#'   comparison decidable at the same \code{alpha_max}, \code{NA} if none).
#' @examples
#' morie_fairness_compare_groups(0.35, 0.55, alpha_max = 0.05, beta_max = 0.20)
#' morie_fairness_compare_groups(0.35, 0.55, alpha_max = 0.05, beta_max = 0.40)
#' @export
morie_fairness_compare_groups <- function(p_obs_a, p_obs_b, alpha_max, beta_max) {
  ba <- morie_fairness_base_rate_bounds(p_obs_a, alpha_max, beta_max)
  bb <- morie_fairness_base_rate_bounds(p_obs_b, alpha_max, beta_max)
  order <- if (ba$upper < bb$lower) "a < b" else if (bb$upper < ba$lower) "b < a" else "undecided"
  # breakdown: with alpha_max fixed, the upper bound is p_obs / (1 - beta); the
  # comparison a < b needs p_obs_a / (1 - beta) < (p_obs_b - alpha_max) / (1 - alpha_max)
  breakdown <- NA_real_
  lo_b <- (p_obs_b - alpha_max) / (1 - alpha_max)
  lo_a <- (p_obs_a - alpha_max) / (1 - alpha_max)
  if (p_obs_a < p_obs_b && lo_b > 0) {
    b <- 1 - p_obs_a / lo_b
    if (b > 0) breakdown <- min(b, 1 - alpha_max - 1e-12)
  } else if (p_obs_b < p_obs_a && lo_a > 0) {
    b <- 1 - p_obs_b / lo_a
    if (b > 0) breakdown <- min(b, 1 - alpha_max - 1e-12)
  }
  list(decided = order != "undecided", order = order,
       interval_a = c(lower = ba$lower, upper = ba$upper),
       interval_b = c(lower = bb$lower, upper = bb$upper),
       breakdown_beta_max = breakdown,
       theorem = if (order != "undecided") "Research.P5.compare_decided" else "Research.P5.compare_undecided")
}


#' Rescaling of logit coefficients across nested models
#'
#' In a latent-index model with logistic error (variance \eqn{\pi^2/3}) and an
#' omitted covariate independent of the others carrying latent variance
#' \code{omitted_var}, the coefficient a logit identifies shrinks by the
#' factor \eqn{\sqrt{(\pi^2/3)/(\pi^2/3 + v)}} when that covariate is dropped,
#' with no confounding at all (\code{Research.P5.reduced_coefficient},
#' \code{rescale_lt_one}, \code{rescale_eq_one_iff}); the apparent odds-ratio
#' change is \eqn{\exp((c-1)\beta)} (\code{ratio_is_rescaling}). Coefficients
#' or odds ratios of a risk score therefore cannot be compared across models
#' with different covariate sets without a variance normalisation
#' (Karlson, Holm & Breen 2012). The identity is exact for a probit index
#' (\code{error_var = 1}: normal plus normal is normal); for the logit the
#' logistic-plus-normal mixture is not logistic and the factor is the
#' standard approximation, accurate to a few percent in simulation.
#'
#' @param beta Identified coefficient(s) in the full model.
#' @param omitted_var Latent variance carried by the omitted independent
#'   covariate(s) (\eqn{\gamma^2\tau^2}), non-negative.
#' @param error_var Error variance of the latent index; \eqn{\pi^2/3} for the
#'   logit, 1 for the probit.
#' @return A list with \code{rescale}, \code{beta_reduced} (what the reduced
#'   model identifies), \code{odds_ratio_full}, \code{odds_ratio_reduced},
#'   \code{apparent_change} (their ratio, pure rescaling) and \code{theorems}.
#' @examples
#' morie_logit_rescale(beta = 0.8, omitted_var = 1)
#' @export
morie_logit_rescale <- function(beta, omitted_var, error_var = pi^2 / 3) {
  if (any(omitted_var < 0) || error_var <= 0) stop("omitted_var must be non-negative and error_var positive", call. = FALSE)
  c_ <- sqrt(error_var / (error_var + omitted_var))
  list(rescale = c_, beta_reduced = beta * c_,
       odds_ratio_full = exp(beta), odds_ratio_reduced = exp(beta * c_),
       apparent_change = exp((c_ - 1) * beta),
       theorems = c("Research.P5.rescale_lt_one", "Research.P5.rescale_eq_one_iff",
                    "Research.P5.reduced_coefficient", "Research.P5.ratio_is_rescaling"))
}


#' Resolution of a ranking built from noisy risk scores
#'
#' Two people with true scores \eqn{t_1 < t_2} are ranked by estimates that
#' noise can move by up to \eqn{\delta} each. Whenever the true gap is below
#' \eqn{2\delta} an admissible noise pair reverses the order
#' (\code{Research.P5.rank_reversal_exists}); whenever it exceeds
#' \eqn{2\delta} no admissible noise can (\code{rank_stable_of_gap}). The
#' resolution of a ranking is therefore twice the noise half-width; on a
#' probability scale with interval half-widths near one half no pair has an
#' identified order (\code{identified_scores_le}). Given point estimates and
#' their half-widths, the function reports which pairs are identified and the
#' share of pairs that are not (the Baldus proportionality-review situation
#' in Weisburd & Britt ch. 1, where most intervals covered nearly all of [0, 1]).
#'
#' @param estimate Estimated scores (probabilities).
#' @param half_width Half-width of the interval around each estimate (one
#'   number, recycled, or one per estimate).
#' @return A list with \code{n_pairs}, \code{identified_pairs} (a logical
#'   matrix: order identified if the estimates differ by more than the sum of
#'   the two half-widths), \code{share_unidentified}, \code{resolution}
#'   (twice the largest half-width) and \code{theorems}.
#' @examples
#' morie_ranking_resolution(c(0.2, 0.35, 0.8), half_width = c(0.1, 0.1, 0.05))
#' @export
morie_ranking_resolution <- function(estimate, half_width) {
  n <- length(estimate)
  if (n < 2) stop("need at least two scores", call. = FALSE)
  half_width <- rep_len(half_width, n)
  if (any(half_width < 0)) stop("half_width must be non-negative", call. = FALSE)
  gap <- abs(outer(estimate, estimate, "-"))
  tol <- outer(half_width, half_width, "+")
  ident <- gap > tol
  diag(ident) <- NA
  pairs <- ident[upper.tri(ident)]
  list(n_pairs = length(pairs), identified_pairs = ident,
       share_unidentified = mean(!pairs), resolution = 2 * max(half_width),
       theorems = c("Research.P5.rank_reversal_exists", "Research.P5.rank_stable_of_gap",
                    "Research.P5.identified_scores_le"))
}


#' Built-in selection in period-by-period hazard ratios
#'
#' Two risk types with per-period reconviction probabilities \code{h} (high)
#' and \code{l} (low), initial high-risk share \code{s}. Whatever the
#' programme does in period 1, the period-2 hazard among survivors is a
#' weighted average of \code{h} and \code{l} with weight the surviving
#' high-risk share (\code{Research.P5.survivor_hazard_mono}); an arm that
#' depletes the high-risk type less in period 1 has a higher period-2 hazard
#' with no individual effect in period 2 at all
#' (\code{hr2_gt_one_of_depletion}, \code{hr2_witness}). A period-specific
#' hazard ratio is therefore not a causal contrast; fixed-horizon risk
#' differences are.
#'
#' @param s Initial high-risk share in (0, 1).
#' @param h,l Per-period hazards of the high- and low-risk types, \code{l < h}.
#' @param survive_high,survive_low Period-1 survival probabilities of the two
#'   types in each arm, as length-2 vectors \code{c(control, treated)}.
#' @return A list with \code{surviving_high_share} per arm,
#'   \code{period2_hazard} per arm, \code{period2_hazard_ratio} (treated over
#'   control) and \code{theorems}.
#' @examples
#' morie_hazard_selection(s = 0.5, h = 0.5, l = 0.1,
#'                        survive_high = c(control = 0.5, treated = 0.8),
#'                        survive_low = c(control = 0.9, treated = 0.9))
#' @export
morie_hazard_selection <- function(s, h, l, survive_high, survive_low) {
  if (s <= 0 || s >= 1) stop("s must lie in (0, 1)", call. = FALSE)
  if (!(l < h) || l < 0 || h > 1) stop("need 0 <= l < h <= 1", call. = FALSE)
  if (length(survive_high) != 2L || length(survive_low) != 2L || any(c(survive_high, survive_low) <= 0)) stop("survival probabilities must be length-2 positive vectors", call. = FALSE)
  w <- s * survive_high / (s * survive_high + (1 - s) * survive_low)
  hz <- w * h + (1 - w) * l
  names(w) <- names(hz) <- c("control", "treated")
  list(surviving_high_share = w, period2_hazard = hz, period2_hazard_ratio = unname(hz[2] / hz[1]),
       theorems = c("Research.P5.survivor_hazard_mono", "Research.P5.hr2_gt_one_of_depletion", "Research.P5.hr2_witness"))
}
