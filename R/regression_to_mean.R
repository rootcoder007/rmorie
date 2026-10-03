# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P19: regression to the mean at selected hot spots (research/lean/P19Regression.lean;
# Galton 1886; Campbell & Stanley 1963; Sherman & Weisburd 1995).
#
#   Research.P19.exchange_mass / exchange_cross   under exchangeable periods the selected mass and the cross term reindex
#   Research.P19.indicator_bound                  x1 (1{x1>c} - 1{x2>c}) >= c (1{x1>c} - 1{x2>c})
#   Research.P19.selected_change_nonpos           sum_{x1 > c} w (x2 - x1) <= 0
#   Research.P19.low_selected_change_nonneg       the mirror for places selected for being low

#' Regression to the mean at selected hot spots
#'
#' Places selected because their first-period count exceeds \code{threshold}
#' show, under the null of exchangeable periods, a non-positive mean change in
#' the second period (\code{Research.P19.selected_change_nonpos}); places
#' selected for being low show a non-negative one
#' (\code{low_selected_change_nonneg}). The function reports the observed
#' change on the selected places, the mirror change (selection on the second
#' period, read backwards), and the symmetrised change, which is the same
#' statistic on the exchangeable population made of the data and its
#' period-swapped copy, and is therefore non-positive by the theorem. The
#' difference between the observed and the symmetrised change is the part of
#' the drop that selection symmetry does not deliver; a treatment effect is
#' identified only against a control group selected the same way.
#' @param x1,x2 Counts per place in the first and second period.
#' @param threshold Selection cut: places with \code{x1 > threshold} are the hot spots.
#' @param weights Optional non-negative weights.
#' @return A list with \code{n_selected}, \code{selected_change} (mean of
#'   \eqn{x_2 - x_1} on the selected places), \code{mirror_change},
#'   \code{symmetrised_change} (always \eqn{\le 0}), \code{excess_over_symmetry},
#'   \code{low_selected_change} (places with \code{x1 < threshold}, always
#'   \eqn{\ge 0} after symmetrisation: \code{low_symmetrised_change}) and
#'   \code{theorems}.
#' @examples
#' set.seed(5)
#' mu <- rgamma(300, 2, 0.5)                   # stable place means
#' x1 <- rpois(300, mu); x2 <- rpois(300, mu)  # no treatment, no trend
#' morie_regression_to_mean(x1, x2, threshold = quantile(x1, 0.9))[c("selected_change", "symmetrised_change")]
#' @export
morie_regression_to_mean <- function(x1, x2, threshold, weights = NULL) {
  n <- length(x1)
  if (length(x2) != n) stop("x1 and x2 must have equal length", call. = FALSE)
  if (anyNA(x1) || anyNA(x2)) stop("counts must not contain NA", call. = FALSE)
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) stop("threshold must be a single number", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0)) stop("weights must be non-negative", call. = FALSE)
  w <- weights
  sel1 <- x1 > threshold; sel2 <- x2 > threshold
  low1 <- x1 < threshold; low2 <- x2 < threshold
  if (!any(sel1)) stop("no place exceeds the threshold", call. = FALSE)
  sel_change <- sum(w[sel1] * (x2 - x1)[sel1]) / sum(w[sel1])
  mirror <- if (any(sel2)) sum(w[sel2] * (x1 - x2)[sel2]) / sum(w[sel2]) else NA_real_
  # the symmetrised population: the data plus its period-swapped copy (exchangeable by construction)
  sym_num <- sum(w[sel1] * (x2 - x1)[sel1]) + sum(w[sel2] * (x1 - x2)[sel2])
  sym_den <- sum(w[sel1]) + sum(w[sel2])
  sym <- sym_num / sym_den
  low_change <- if (any(low1)) sum(w[low1] * (x2 - x1)[low1]) / sum(w[low1]) else NA_real_
  low_sym <- if (any(low1) || any(low2)) (sum(w[low1] * (x2 - x1)[low1]) + sum(w[low2] * (x1 - x2)[low2])) / (sum(w[low1]) + sum(w[low2])) else NA_real_
  list(n_selected = sum(sel1), selected_change = sel_change, mirror_change = mirror,
       symmetrised_change = sym, excess_over_symmetry = sel_change - sym,
       low_selected_change = low_change, low_symmetrised_change = low_sym,
       theorems = c("Research.P19.exchange_mass", "Research.P19.exchange_cross", "Research.P19.indicator_bound",
                    "Research.P19.selected_change_nonpos", "Research.P19.low_selected_change_nonneg"))
}
