# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P14: judge-leniency designs (research/lean/P14Instrument.lean; Imbens & Angrist 1994;
# Angrist, Imbens & Rubin 1996; Dobbie, Goldin & Yang 2018).
#
#   Research.P14.itt_decomposition         E[y(d(1))] - E[y(d(0))] = sum_compliers w (y1 - y0) - sum_defiers w (y1 - y0)
#   Research.P14.first_stage_decomposition E[d(1)] - E[d(0)] = P(complier) - P(defier)
#   Research.P14.late_identification       no defiers, P(complier) > 0  =>  Wald = mean effect among compliers
#   Research.P14.wald_with_defiers         otherwise Wald = (P(c) tau_c - P(d) tau_d) / (P(c) - P(d))
#   Research.P14.defiers_can_flip          a witness where every effect is positive and the Wald ratio is -5

#' Judge-leniency instrument on a known population: the exact decomposition
#'
#' With every person's potential treatment under a lenient (\code{d0}) and a
#' harsh (\code{d1}) judge and potential outcomes under release (\code{y0})
#' and detention (\code{y1}), the intention-to-treat contrast splits exactly
#' into the compliers' effect mass minus the defiers' effect mass
#' (\code{Research.P14.itt_decomposition}), the first stage into
#' \eqn{P(c) - P(d)} (\code{first_stage_decomposition}), and with no defiers
#' the Wald ratio is the compliers' mean effect (\code{late_identification});
#' with defiers it is \eqn{(P(c)\tau_c - P(d)\tau_d)/(P(c)-P(d))}
#' (\code{wald_with_defiers}), whose sign can be wrong when every individual
#' effect is positive (\code{defiers_can_flip}). Use it on a simulated or
#' fully specified population to see what a design can and cannot recover;
#' \code{\link{morie_judge_iv}} is the observational counterpart.
#' @param d0,d1 Potential treatment (0/1) under instrument 0 and 1.
#' @param y0,y1 Potential outcomes under treatment 0 and 1.
#' @param weights Optional non-negative weights.
#' @return A list with \code{shares} (complier, defier, always, never),
#'   \code{effects} (mean effect by type, \code{NA} for an empty type),
#'   \code{itt}, \code{first_stage}, \code{wald}, \code{late} (the compliers'
#'   effect), \code{monotone}, \code{ate} (the population mean effect, for
#'   comparison) and \code{theorems}.
#' @examples
#' d0 <- c(0, 0, 1, 0, 1); d1 <- c(1, 1, 1, 0, 0)       # two compliers, an always-taker, a never-taker, a defier
#' y0 <- c(0, 0, 1, 0, 0); y1 <- c(1, 0, 1, 1, 1)
#' morie_judge_iv_population(d0, d1, y0, y1)[c("shares", "wald", "late")]
#' @export
morie_judge_iv_population <- function(d0, d1, y0, y1, weights = NULL) {
  n <- length(d0)
  if (length(d1) != n || length(y0) != n || length(y1) != n) stop("d0, d1, y0 and y1 must have equal length", call. = FALSE)
  if (!all(c(d0, d1) %in% c(0, 1))) stop("d0 and d1 must be 0/1", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0) || sum(weights) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  w <- weights / sum(weights)
  type <- ifelse(d0 == 0 & d1 == 1, "complier", ifelse(d0 == 1 & d1 == 0, "defier", ifelse(d0 == 1, "always", "never")))
  eff <- y1 - y0
  shares <- vapply(c("complier", "defier", "always", "never"), function(t) sum(w[type == t]), numeric(1))
  effects <- vapply(c("complier", "defier", "always", "never"), function(t) if (shares[[t]] > 0) sum(w[type == t] * eff[type == t]) / shares[[t]] else NA_real_, numeric(1))
  yz1 <- ifelse(d1 == 1, y1, y0)
  yz0 <- ifelse(d0 == 1, y1, y0)
  itt <- sum(w * yz1) - sum(w * yz0)
  fs <- sum(w * d1) - sum(w * d0)
  list(shares = shares, effects = effects, itt = itt, first_stage = fs,
       wald = if (fs != 0) itt / fs else NA_real_,
       late = effects[["complier"]], monotone = shares[["defier"]] == 0, ate = sum(w * eff),
       theorems = c("Research.P14.itt_decomposition", "Research.P14.first_stage_decomposition",
                    "Research.P14.late_identification", "Research.P14.wald_with_defiers", "Research.P14.defiers_can_flip"))
}

#' Judge-leniency instrument on observed data: Wald ratio, compliers and the defier sensitivity
#'
#' From an instrument \code{z} (0 lenient, 1 harsh), the treatment \code{d}
#' it moved and the outcome \code{y}, reports the first stage, the
#' intention-to-treat contrast and the Wald ratio. Under monotonicity the
#' Wald ratio is the mean effect among compliers and the type shares are
#' identified: always-takers \eqn{P(d=1 \mid z=0)}, never-takers
#' \eqn{P(d=0 \mid z=1)}, compliers the rest
#' (\code{Research.P14.late_identification}). The defier sensitivity reports,
#' for assumed defier shares and defier effects, the compliers' effect the
#' same Wald ratio would imply (\code{wald_with_defiers}): the estimand depends
#' on an assumption the data cannot check.
#' @param z Instrument (0/1), e.g. a harsh-judge indicator or leniency above the median.
#' @param d Treatment (0/1), e.g. pretrial detention.
#' @param y Outcome.
#' @param weights Optional non-negative weights.
#' @param defier_share,defier_effect Assumed defier shares and effects for the sensitivity table.
#' @return A list with \code{first_stage}, \code{itt}, \code{wald},
#'   \code{shares_if_monotone}, \code{sensitivity} (a data frame of implied
#'   complier effects) and \code{theorems}.
#' @examples
#' set.seed(2)
#' n <- 2000; z <- rbinom(n, 1, 0.5)
#' d0 <- rbinom(n, 1, 0.2); d1 <- pmax(d0, rbinom(n, 1, 0.5))     # monotone
#' d <- ifelse(z == 1, d1, d0); y <- 0.3 * d + rnorm(n)
#' morie_judge_iv(z, d, y)[c("first_stage", "wald")]
#' @export
morie_judge_iv <- function(z, d, y, weights = NULL, defier_share = c(0, 0.05, 0.1), defier_effect = 0) {
  n <- length(z)
  if (length(d) != n || length(y) != n) stop("z, d and y must have equal length", call. = FALSE)
  if (!all(z %in% c(0, 1)) || !all(d %in% c(0, 1))) stop("z and d must be 0/1", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0) || sum(weights) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  if (!any(z == 1) || !any(z == 0)) stop("z must take both values", call. = FALSE)
  w <- weights / sum(weights)
  m <- function(v, idx) sum(w[idx] * v[idx]) / sum(w[idx])
  fs <- m(d, z == 1) - m(d, z == 0)
  itt <- m(y, z == 1) - m(y, z == 0)
  wald <- if (fs != 0) itt / fs else NA_real_
  always <- m(d, z == 0)
  never <- 1 - m(d, z == 1)
  shares <- c(complier = 1 - always - never, always = always, never = never)
  grid <- expand.grid(defier_share = defier_share, defier_effect = defier_effect)
  grid$complier_share <- shares[["complier"]] + grid$defier_share
  grid$implied_complier_effect <- ifelse(grid$complier_share > 0 & !is.na(wald),
                                         (wald * (grid$complier_share - grid$defier_share) + grid$defier_share * grid$defier_effect) / grid$complier_share,
                                         NA_real_)
  list(first_stage = fs, itt = itt, wald = wald, shares_if_monotone = shares, sensitivity = grid,
       theorems = c("Research.P14.late_identification", "Research.P14.wald_with_defiers", "Research.P14.first_stage_decomposition"))
}

#   Research.P14Slope.propensity_mono         nested judges: P_j <= P_k
#   Research.P14Slope.outcome_diff            Y_k - Y_j = effect summed over the marginal compliers
#   Research.P14Slope.slope_bound             |Y_k - Y_j| <= (hi - lo)(P_k - P_j)
#   Research.P14Slope.violation_refutes_monotonicity   a steeper pair is not nested

#' The many-judge slope test of monotonicity
#'
#' With judges randomly assigned, each judge's detention rate \eqn{P_j} and
#' mean outcome \eqn{Y_j} estimate population quantities under that judge.
#' When judge \eqn{k} detains everyone judge \eqn{j} detains (the many-judge
#' form of monotonicity), \eqn{Y_k - Y_j} is the treatment effect summed over
#' the marginal compliers (\code{Research.P14Slope.outcome_diff}), so the
#' pair's Wald ratio is their average effect, and with outcomes in
#' \eqn{[lo, hi]} the mean outcome cannot move faster than the detention
#' rate times the range: \eqn{|Y_k - Y_j| \le (hi - lo)(P_k - P_j)}
#' (\code{slope_bound}). A pair that violates the bound is not nested
#' (\code{violation_refutes_monotonicity}); that is the test of Frandsen,
#' Lefgren and Leslie (2023).
#' @param judge Judge identifier per case.
#' @param d Detention or treatment indicator (0/1) per case.
#' @param y Outcome per case.
#' @param weights Optional non-negative case weights.
#' @param lo,hi Bounds of the outcome; default its observed range.
#' @return A list with \code{judges} (a data frame ordered by propensity:
#'   \code{judge}, \code{n}, \code{propensity}, \code{outcome}),
#'   \code{pairs} (\code{j}, \code{k}, \code{dP}, \code{dY}, \code{bound},
#'   \code{violation}, \code{late}), \code{violations},
#'   \code{monotone_consistent} and \code{theorems}.
#' @examples
#' judge <- rep(c("A", "B", "C"), each = 6)
#' d <- c(0, 0, 0, 1, 1, 0,  0, 1, 1, 1, 0, 1,  1, 1, 1, 1, 1, 0)
#' y <- c(1, 0, 0, 1, 0, 0,  0, 1, 1, 0, 0, 1,  1, 1, 1, 1, 0, 1)
#' s <- morie_judge_slope_test(judge, d, y)
#' s$judges
#' s$pairs
#' @export
morie_judge_slope_test <- function(judge, d, y, weights = NULL, lo = min(y), hi = max(y)) {
  n <- length(judge)
  if (length(d) != n || length(y) != n) stop("judge, d and y must have equal length", call. = FALSE)
  if (anyNA(judge) || anyNA(d) || anyNA(y)) stop("no missing values allowed", call. = FALSE)
  if (!all(d %in% c(0, 1))) stop("d must be 0/1", call. = FALSE)
  if (any(y < lo) || any(y > hi)) stop("y must lie in [lo, hi]", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else weights
  if (length(w) != n || anyNA(w) || any(w < 0)) stop("weights must be non-negative", call. = FALSE)
  ids <- unique(as.character(judge))
  judge <- as.character(judge)
  P <- vapply(ids, function(j) { s <- judge == j
  sum(w[s] * d[s]) / sum(w[s]) }, numeric(1))
  Y <- vapply(ids, function(j) { s <- judge == j
  sum(w[s] * y[s]) / sum(w[s]) }, numeric(1))
  cnt <- vapply(ids, function(j) sum(judge == j), numeric(1))
  if (any(!is.finite(P))) stop("every judge needs positive total weight", call. = FALSE)
  o <- order(P)
  judges <- data.frame(judge = ids[o], n = cnt[o], propensity = P[o], outcome = Y[o], stringsAsFactors = FALSE, row.names = NULL)
  J <- length(ids)
  pairs <- NULL
  if (J >= 2L) {
    idx <- utils::combn(J, 2)
    j <- idx[1, ]
    k <- idx[2, ]
    dP <- judges$propensity[k] - judges$propensity[j]
    dY <- judges$outcome[k] - judges$outcome[j]
    bound <- (hi - lo) * dP
    pairs <- data.frame(j = judges$judge[j], k = judges$judge[k], dP = dP, dY = dY, bound = bound,
                        violation = abs(dY) > bound + 1e-12,
                        late = ifelse(dP > 0, dY / dP, NA_real_), stringsAsFactors = FALSE)
  }
  viol <- if (is.null(pairs)) 0L else sum(pairs$violation)
  list(judges = judges, pairs = pairs, violations = viol, monotone_consistent = viol == 0L,
       lo = lo, hi = hi,
       theorems = c("Research.P14Slope.propensity_mono", "Research.P14Slope.outcome_diff",
                    "Research.P14Slope.slope_bound", "Research.P14Slope.violation_refutes_monotonicity"))
}

#   Research.P14Slope.subgroup_propensity_mono   nested judges are ordered inside every subgroup
#   Research.P14Slope.subgroup_reversal_refutes  a subgroup reversal refutes the nesting
#   Research.P14Slope.crossing_refutes_both      stricter overall, more lenient in a subgroup: unordered

#' Monotonicity of judges without outcomes: do the leniency rankings hold in every subgroup?
#'
#' Judge designs need judges ordered by leniency: whoever a lenient judge detains, a stricter
#' one detains too (nesting, the many-judge monotonicity). Nesting holds person by person, so it
#' orders the judges' detention rates inside every subgroup of defendants
#' (\code{Research.P14Slope.subgroup_propensity_mono}). For every pair of judges, the one with the
#' higher detention rate overall should not have a lower one in any subgroup; a pair that does is
#' nested in neither direction (\code{crossing_refutes_both}). With sampling noise a reversal is
#' evidence only when it is significant, so each is tested (one-sided, difference of proportions)
#' and the p-values are adjusted across every comparison made.
#' @param judge Judge of each case.
#' @param d 0/1 decision (1 = detained or incarcerated).
#' @param group Subgroup of each case (an offence category, a year).
#' @param min_n Smallest number of a judge's cases in a subgroup for that cell to be compared.
#' @param adjust Multiple-comparison adjustment passed to \code{p.adjust}.
#' @param alpha Level at which an adjusted reversal counts as significant.
#' @return A list: \code{judges} (overall rate and cases per judge), \code{comparisons} (one row
#'   per pair and subgroup compared, stricter-overall judge first, with the subgroup rates, the
#'   z statistic for the reversal, raw and adjusted p-values), \code{summary} (comparisons,
#'   reversals, significant reversals, the pairs and judges involved), \code{by_group} (per
#'   subgroup: judges compared and the slope of the subgroup rate on the overall rate, the
#'   average-monotonicity check) and \code{theorems}.
#' @examples
#' set.seed(1)
#' judge <- sample(sprintf("J%02d", 1:8), 4000, replace = TRUE)
#' strict <- setNames(seq(0.2, 0.6, length.out = 8), sprintf("J%02d", 1:8))
#' group <- sample(c("drugs", "theft", "weapons"), 4000, replace = TRUE)
#' d <- rbinom(4000, 1, strict[judge])
#' r <- morie_judge_monotonicity(judge, d, group)
#' r$summary
#' @export
morie_judge_monotonicity <- function(judge, d, group, min_n = 30, adjust = "holm", alpha = 0.05) {
  n <- length(judge)
  if (length(d) != n || length(group) != n) stop("judge, d and group must have equal length", call. = FALSE)
  if (anyNA(judge) || anyNA(d) || anyNA(group)) stop("no missing values allowed", call. = FALSE)
  if (!all(d %in% c(0, 1))) stop("d must be 0/1", call. = FALSE)
  if (!is.numeric(min_n) || length(min_n) != 1L || min_n < 1) stop("min_n must be a positive number", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !(alpha > 0 && alpha < 1)) stop("alpha must be in (0, 1)", call. = FALSE)
  judge <- as.character(judge)
  group <- as.character(group)
  jt <- tapply(d, judge, mean)
  jn <- tapply(d, judge, length)
  judges <- data.frame(judge = names(jt), rate = as.numeric(jt), n = as.integer(jn), stringsAsFactors = FALSE)
  judges <- judges[order(-judges$rate), ]
  rows <- list()
  slopes <- list()
  for (g in sort(unique(group))) {
    s <- group == g
    cr <- tapply(d[s], judge[s], mean)
    cn <- tapply(d[s], judge[s], length)
    keep <- names(cn)[cn >= min_n]
    if (length(keep) >= 2L) {
      o <- jt[keep]
      fit <- stats::lm.wfit(cbind(1, as.numeric(o)), as.numeric(cr[keep]), as.numeric(cn[keep]))
      slopes[[g]] <- data.frame(group = g, judges = length(keep), cases = sum(cn[keep]), slope = unname(fit$coefficients[2]),
                                stringsAsFactors = FALSE)
    }
    if (length(keep) < 2L) next
    pr <- utils::combn(keep, 2L)
    a <- pr[1L, ]
    b <- pr[2L, ]
    # the stricter judge overall first
    swap <- jt[a] < jt[b]
    hi <- ifelse(swap, b, a)
    lo <- ifelse(swap, a, b)
    tie <- jt[hi] == jt[lo]
    hi <- hi[!tie]
    lo <- lo[!tie]
    if (!length(hi)) next
    ph <- cr[hi]
    pl <- cr[lo]
    nh <- cn[hi]
    nl <- cn[lo]
    pp <- (ph * nh + pl * nl) / (nh + nl)
    se <- sqrt(pp * (1 - pp) * (1 / nh + 1 / nl))
    z <- ifelse(se > 0, (pl - ph) / se, 0)
    rows[[g]] <- data.frame(group = g, stricter = hi, lenient = lo, rate_stricter = as.numeric(jt[hi]),
                            rate_lenient = as.numeric(jt[lo]), group_rate_stricter = as.numeric(ph),
                            group_rate_lenient = as.numeric(pl), n_stricter = as.integer(nh), n_lenient = as.integer(nl),
                            z = as.numeric(z), p_value = stats::pnorm(-as.numeric(z)), stringsAsFactors = FALSE)
  }
  comp <- if (length(rows)) do.call(rbind, rows) else
    data.frame(group = character(), stricter = character(), lenient = character(), z = numeric(), p_value = numeric())
  rownames(comp) <- NULL
  comp$p_adjusted <- stats::p.adjust(comp$p_value, adjust)
  comp$reversed <- if (nrow(comp)) comp$group_rate_stricter < comp$group_rate_lenient else logical()
  comp$significant <- comp$p_adjusted < alpha
  sig <- comp[comp$significant, , drop = FALSE]
  summary <- data.frame(comparisons = nrow(comp), reversed = sum(comp$reversed),
                        share_reversed = if (nrow(comp)) mean(comp$reversed) else NA_real_,
                        significant = nrow(sig), pairs_refuted = nrow(unique(sig[c("stricter", "lenient")])),
                        judges_involved = length(unique(c(sig$stricter, sig$lenient))))
  by_group <- if (length(slopes)) do.call(rbind, slopes) else data.frame()
  rownames(by_group) <- NULL
  list(judges = judges, comparisons = comp, summary = summary, by_group = by_group,
       theorems = c("Research.P14Slope.propensity_mono", "Research.P14Slope.subgroup_propensity_mono",
                    "Research.P14Slope.subgroup_reversal_refutes", "Research.P14Slope.crossing_refutes_both"))
}
