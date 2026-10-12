# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P18: selective labels (research/lean/P18Selective.lean; Lakkaraju et al. 2017; Kleinberg et al. 2018).
#
#   Research.P18.observed_rate_is_conditional   the reported failure rate is the rate among the released
#   Research.P18.nested_rate_identified         a rule's set inside a lenient judge's release set is scored from that judge's outcomes
#   Research.P18.unobserved_bounds              otherwise the rule's rate lies in [fails(M & R)/w(M), (fails(M & R) + w(M \ R))/w(M)]
#   Research.P18.unobserved_width               the interval's width is the unobserved share w(M \ R)/w(M)
#   Research.P18.unobserved_ends_attained       both ends are reached by admissible fillings
#   Research.P18.chain_width_antitone           nested judges: the interval narrows with leniency; chain_contraction, chain_width_zero_iff

#' Selective labels: what a release rule's failure rate can be known from released cases
#'
#' Outcomes (a missed court date, a new arrest) are observed only for the
#' defendants a judge released. A proposed rule releases a set \code{M}; its
#' failure rate is identified only through the released part \eqn{M \cap R}:
#' exactly when \eqn{M \subseteq R} (the contraction case,
#' \code{Research.P18.nested_rate_identified}), and otherwise only up to the
#' interval \eqn{[\mathrm{fails}(M\cap R)/w(M),\ (\mathrm{fails}(M\cap R) + w(M \setminus R))/w(M)]},
#' both ends attainable, of width equal to the unobserved share
#' (\code{unobserved_bounds}, \code{unobserved_width},
#' \code{unobserved_ends_attained}). The naive estimate, the failure rate on
#' \eqn{M \cap R} alone, is reported beside the interval; it is a point inside
#' the interval chosen by assumption.
#' @param y Failure indicator (0/1) per defendant; only its values on
#'   \code{released} are used.
#' @param released Logical: released by the judge (outcome observed).
#' @param rule_released Logical: released by the proposed rule.
#' @param weights Optional non-negative weights.
#' @return A list with \code{observed_rate} (among the judge's released),
#'   \code{rule_share_unobserved}, \code{identified} (whether the rule's set is
#'   inside the released set), \code{rule_rate} (when identified), \code{bounds},
#'   \code{width}, \code{naive_rate} and \code{theorems}.
#' @examples
#' set.seed(4)
#' n <- 500; risk <- runif(n); y <- rbinom(n, 1, risk)
#' released <- risk < 0.7                       # the judge detains the riskiest 30 percent
#' rule <- risk < 0.6                           # a stricter rule: inside the judge's releases
#' morie_selective_labels(y, released, rule)[c("identified", "rule_rate")]
#' rule2 <- risk < 0.8                          # a more lenient rule: part of it is unobserved
#' morie_selective_labels(y, released, rule2)[c("bounds", "width", "naive_rate")]
#' @export
morie_selective_labels <- function(y, released, rule_released, weights = NULL) {
  n <- length(y)
  released <- as.logical(released)
  rule_released <- as.logical(rule_released)
  if (length(released) != n || length(rule_released) != n) stop("y, released and rule_released must have equal length", call. = FALSE)
  if (anyNA(released) || anyNA(rule_released)) stop("released and rule_released must not contain NA", call. = FALSE)
  if (!all(y[released] %in% c(0, 1))) stop("y must be 0/1 on the released", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0)) stop("weights must be non-negative", call. = FALSE)
  w <- weights
  wy <- ifelse(released, w * y, 0)
  mass_R <- sum(w[released])
  mass_M <- sum(w[rule_released])
  if (mass_R <= 0 || mass_M <= 0) stop("both the released set and the rule's set must have positive mass", call. = FALSE)
  obs_rate <- sum(wy[released]) / mass_R
  inter <- released & rule_released
  fails_inter <- sum(wy[inter])
  mass_unobs <- sum(w[rule_released & !released])
  identified <- mass_unobs == 0
  lower <- fails_inter / mass_M
  upper <- (fails_inter + mass_unobs) / mass_M
  list(observed_rate = obs_rate, rule_share_unobserved = mass_unobs / mass_M, identified = identified,
       rule_rate = if (identified) lower else NA_real_,
       bounds = c(lower = lower, upper = upper), width = upper - lower,
       naive_rate = if (sum(w[inter]) > 0) fails_inter / sum(w[inter]) else NA_real_,
       theorems = c("Research.P18.observed_rate_is_conditional", "Research.P18.nested_rate_identified",
                    "Research.P18.unobserved_bounds", "Research.P18.unobserved_width", "Research.P18.unobserved_ends_attained"))
}

#' Selective labels across judges ordered by leniency
#'
#' With several judges whose release sets are nested from the strictest to the
#' most lenient, a rule's identified interval shrinks with leniency
#' (\code{Research.P18.chain_width_antitone}): the most lenient judge gives
#' the sharpest interval, and the interval collapses to a point exactly when
#' the rule releases nobody that judge detained
#' (\code{chain_width_zero_iff}, \code{chain_contraction}). Judges need not
#' be nested; then the function reports the sharpest interval among them and
#' says the chain theorems do not apply.
#'
#' @inheritParams morie_selective_labels
#' @param released Logical matrix with one column per judge (rows are
#'   defendants), or a list of logical vectors, ordered from strictest to
#'   most lenient.
#' @return A list with \code{judges} (a data frame: release share, interval,
#'   width and whether the rule is identified, one row per judge),
#'   \code{nested}, \code{best} (the judge with the narrowest interval),
#'   \code{identified}, \code{rule_rate} (when identified), \code{bounds},
#'   \code{width} and \code{theorems}.
#' @examples
#' set.seed(5)
#' n <- 400; risk <- runif(n); y <- rbinom(n, 1, risk)
#' judges <- cbind(strict = risk < 0.5, middle = risk < 0.7, lenient = risk < 0.85)
#' morie_selective_labels_judges(y, judges, rule_released = risk < 0.8)$judges
#' @export
morie_selective_labels_judges <- function(y, released, rule_released, weights = NULL) {
  if (is.list(released) && !is.data.frame(released)) released <- do.call(cbind, released)
  released <- as.matrix(released)
  if (!is.logical(released)) storage.mode(released) <- "logical"
  n <- length(y)
  if (nrow(released) != n || !ncol(released)) stop("released must have one row per defendant and at least one judge", call. = FALSE)
  if (anyNA(released)) stop("released must not contain NA", call. = FALSE)
  k <- ncol(released)
  judge_names <- if (!is.null(colnames(released))) colnames(released) else paste0("judge", seq_len(k))
  if (is.null(weights)) weights <- rep(1, n)
  nested <- all(vapply(seq_len(max(k - 1L, 0L)), function(j) !any(released[, j] & !released[, j + 1L]), logical(1)))
  per <- lapply(seq_len(k), function(j) morie_selective_labels(y, released[, j], rule_released, weights))
  tab <- data.frame(
    judge = judge_names,
    release_share = vapply(seq_len(k), function(j) sum(weights[released[, j]]) / sum(weights), numeric(1)),
    lower = vapply(per, function(p) p$bounds[["lower"]], numeric(1)),
    upper = vapply(per, function(p) p$bounds[["upper"]], numeric(1)),
    width = vapply(per, function(p) p$width, numeric(1)),
    identified = vapply(per, function(p) p$identified, logical(1)),
    stringsAsFactors = FALSE
  )
  best <- which.min(tab$width)
  list(
    judges = tab, nested = nested, best = judge_names[best],
    identified = tab$identified[best],
    rule_rate = if (tab$identified[best]) tab$lower[best] else NA_real_,
    bounds = c(lower = tab$lower[best], upper = tab$upper[best]), width = tab$width[best],
    theorems = c("Research.P18.unobserved_mass_antitone", "Research.P18.interval_width_antitone",
                 "Research.P18.chain_contraction", "Research.P18.chain_width_antitone", "Research.P18.chain_width_zero_iff")
  )
}
