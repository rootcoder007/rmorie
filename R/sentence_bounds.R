# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P11: sentencing effects as intervals (research/lean/P11Bounds.lean;
# Manski, Identification for Prediction and Decision, sec. 7.2; Manski & Nagin 1998).
#
#   Research.P11.Pop.outcome_bounds     P(y=1, z=t) <= P[y(t)=1] <= P(y=1, z=t) + P(z != t), both ends attained
#   Research.P11.Pop.ate_width_one      the worst-case interval for P[y(b)=1] - P[y(a)=1] has width 1
#   Research.P11.Pop.ate_contains_zero  and contains 0

#' Worst-case identification bounds for a binary outcome under two sentences
#'
#' Each person receives one sentence and reveals only the outcome under it.
#' With no assumption on how sentences were assigned, the probability of
#' the outcome under sentence \code{t} is identified only up to
#' \eqn{[P(y=1, z=t),\; P(y=1, z=t) + P(z \ne t)]}, both ends attainable
#' (\code{Research.P11.Pop.outcome_bounds}); the contrast between the two
#' sentences lies in an interval of width exactly one that always contains
#' zero (\code{ate_width_one}, \code{ate_contains_zero}). The interval is
#' the honest report of an observational sentencing comparison before any
#' selection assumption is added; assumptions narrow it, data alone cannot.
#'
#' @param y Binary outcome per person (0/1, e.g. reconviction within two years).
#' @param z Sentence per person, a vector with exactly two distinct values.
#' @param weights Optional non-negative weights.
#' @param contrast Which of the two sentence values is the treatment (default
#'   the second in sorted order); the other is the comparison.
#' @return A list with \code{levels}, \code{joint} and \code{pz} per sentence,
#'   \code{outcome_bounds} (a 2 x 2 matrix, one row per sentence), \code{ate_bounds},
#'   \code{ate_width} (always 1), \code{naive_difference} (the difference of
#'   observed rates, which the interval always contains) and \code{theorems}.
#' @examples
#' set.seed(1)
#' z <- sample(c("community", "custody"), 500, replace = TRUE, prob = c(0.6, 0.4))
#' y <- rbinom(500, 1, ifelse(z == "custody", 0.55, 0.35))
#' morie_sentence_effect_bounds(y, z)
#' @export
morie_sentence_effect_bounds <- function(y, z, weights = NULL, contrast = NULL) {
  n <- length(y)
  if (length(z) != n) stop("y and z must have equal length", call. = FALSE)
  if (!all(y %in% c(0, 1))) stop("y must be 0/1", call. = FALSE)
  lv <- sort(unique(as.character(z)))
  if (length(lv) != 2L) stop("z must take exactly two distinct values", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0) || sum(weights) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  w <- weights / sum(weights)
  z <- as.character(z)
  trt <- if (is.null(contrast)) lv[2] else { if (!contrast %in% lv) stop("contrast must be one of the sentence values", call. = FALSE)
  as.character(contrast) }
  ctl <- setdiff(lv, trt)
  joint <- c(sum(w[z == ctl] * y[z == ctl]), sum(w[z == trt] * y[z == trt]))
  pz <- c(sum(w[z == ctl]), sum(w[z == trt]))
  names(joint) <- names(pz) <- c(ctl, trt)
  ob <- cbind(lower = joint, upper = joint + (1 - pz))
  ate <- c(lower = unname(ob[trt, "lower"] - ob[ctl, "upper"]), upper = unname(ob[trt, "upper"] - ob[ctl, "lower"]))
  list(levels = c(comparison = ctl, treatment = trt), joint = joint, pz = pz,
       outcome_bounds = ob, ate_bounds = ate, ate_width = unname(ate["upper"] - ate["lower"]),
       naive_difference = unname(joint[trt] / pz[trt] - joint[ctl] / pz[ctl]),
       theorems = c("Research.P11.Pop.outcome_bounds", "Research.P11.Pop.lower_attained",
                    "Research.P11.Pop.upper_attained", "Research.P11.Pop.ate_width_one",
                    "Research.P11.Pop.ate_contains_zero"))
}


#' Contaminated-sample bounds for a recorded proportion
#'
#' A recorded distribution is a mixture of the clean distribution of
#' interest and an unknown contaminant (mis-coded offences, unfounded
#' records, wrong addresses) in a known share \code{p}. For any event the
#' clean probability lies in
#' \eqn{[\max(0, (q-p)/(1-p)),\; \min(1, q/(1-p))]} with \eqn{q} the
#' recorded probability; both ends are attained, the width is
#' \eqn{p/(1-p)}, and the interval is informative only when
#' \eqn{p < \min(q, 1-q)} (\code{Research.P11.clean_bounds},
#' \code{clean_lower_attained}, \code{clean_upper_attained},
#' \code{clean_width}, \code{clean_informative}; Manski sec. 5.2).
#'
#' @param q Recorded probability (or vector of them) of the event.
#' @param p Known contamination share in [0, 1).
#' @return A data frame with \code{q}, \code{lower}, \code{upper}, \code{width}
#'   (before clipping to [0, 1]), \code{informative} and attribute \code{"theorems"}.
#' @examples
#' morie_contaminated_bounds(q = c(0.05, 0.3, 0.6), p = 0.1)
#' @export
morie_contaminated_bounds <- function(q, p) {
  if (length(p) != 1L || is.na(p) || p < 0 || p >= 1) stop("p must be a single number in [0, 1)", call. = FALSE)
  if (any(is.na(q)) || any(q < 0 | q > 1)) stop("q must lie in [0, 1]", call. = FALSE)
  lower <- pmax(0, (q - p) / (1 - p))
  upper <- pmin(1, q / (1 - p))
  out <- data.frame(q = q, lower = lower, upper = upper, width = p / (1 - p),
                    informative = (p < q) | (p < 1 - q))
  attr(out, "theorems") <- c("Research.P11.clean_bounds", "Research.P11.clean_lower_attained",
                             "Research.P11.clean_upper_attained", "Research.P11.clean_width",
                             "Research.P11.clean_informative")
  out
}


#' Monotone-treatment-response bounds for a binary outcome under two sentences
#'
#' If a harsher sentence never lowers the outcome for any one person (monotone
#' treatment response, Manski 1997), the contrast \eqn{E[y(b)] - E[y(a)]} is
#' non-negative (\code{Research.P11.Pop.mtr_lower}) and at most
#' \eqn{P(y=1, z=b) + P(y=0, z=a)}, the share of people whose observed outcome
#' leaves room for the counterfactual to differ; the upper end is attained
#' (\code{mtr_upper}, \code{mtr_upper_attained}). Against the assumption-free
#' interval of \code{\link{morie_sentence_effect_bounds}} (width one, contains
#' zero) MTR buys a sign at the price of a monotonicity assumption; reversing
#' the direction gives the bounds for a "never criminogenic" assumption.
#'
#' @inheritParams morie_sentence_effect_bounds
#' @param direction \code{"non-decreasing"} (harsher sentence never lowers the
#'   outcome) or \code{"non-increasing"}.
#' @return A list with \code{levels}, \code{bounds} (lower and upper on the
#'   treatment-minus-comparison contrast), \code{width}, \code{naive_difference}
#'   and \code{theorems}.
#' @examples
#' set.seed(1)
#' z <- sample(c("community", "custody"), 500, replace = TRUE)
#' y <- rbinom(500, 1, ifelse(z == "custody", 0.55, 0.35))
#' morie_sentence_effect_mtr(y, z)$bounds
#' @export
morie_sentence_effect_mtr <- function(y, z, weights = NULL, contrast = NULL,
                                      direction = c("non-decreasing", "non-increasing")) {
  direction <- match.arg(direction)
  b <- morie_sentence_effect_bounds(y, z, weights, contrast)
  trt <- b$levels[["treatment"]]
  ctl <- b$levels[["comparison"]]
  # P(y=1, z=trt) + P(y=0, z=ctl)
  up <- unname(b$joint[trt] + (b$pz[ctl] - b$joint[ctl]))
  bounds <- if (direction == "non-decreasing") c(lower = 0, upper = up) else {
    # non-increasing: the contrast is <= 0 and >= -(P(y=0, z=trt) + P(y=1, z=ctl))
    c(lower = -unname((b$pz[trt] - b$joint[trt]) + b$joint[ctl]), upper = 0)
  }
  list(levels = b$levels, direction = direction, bounds = bounds, width = unname(bounds["upper"] - bounds["lower"]),
       naive_difference = b$naive_difference,
       theorems = c("Research.P11.Pop.mtr_lower", "Research.P11.Pop.mtr_upper", "Research.P11.Pop.mtr_upper_attained"))
}
