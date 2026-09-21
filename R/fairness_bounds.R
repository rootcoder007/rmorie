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
