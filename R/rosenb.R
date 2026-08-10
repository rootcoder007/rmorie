# SPDX-License-Identifier: AGPL-3.0-or-later

#' Rosenbaum sensitivity bounds over a grid of Gamma
#'
#' For each sensitivity parameter Gamma (the largest factor by which two
#' units matched on covariates may differ in their odds of treatment),
#' the null distribution of the Wilcoxon signed-rank statistic is
#' bracketed by two extreme distributions in which each pair contributes
#' its rank with probability \eqn{p_+ = \Gamma/(1+\Gamma)} (upper) or
#' \eqn{p_- = 1/(1+\Gamma)} (lower). This routine tabulates the
#' per-Gamma p-value intervals by calling \code{\link{CnsRos}} -- the
#' single-Gamma machinery already verified against
#' \code{stats::wilcox.test} at Gamma = 1 -- which is how Rosenbaum
#' presents the analysis. \code{gamma_critical} is the largest grid
#' value whose upper p-value still falls below \code{alpha} (NULL if
#' already insensitive at the smallest Gamma).
#'
#' @param matched_pairs Within-pair differences (treated minus control);
#'   exact zeros are dropped.
#' @param Gamma_grid Numeric vector of sensitivity parameters, each at
#'   least 1.
#' @param alpha Level used to report \code{gamma_critical}.
#' @return List with \code{Gamma}, \code{p_upper}, \code{p_lower}
#'   (parallel vectors), \code{gamma_critical}, \code{n_pairs},
#'   \code{W}, \code{alpha}, \code{method}.
#' @references Rosenbaum, P. R. (2002), Observational Studies, 2nd ed.,
#'   Springer, Section 4.3 and the Gamma-table presentation of Chapter
#'   4. Single-Gamma engine: \code{\link{CnsRos}}.
#' @export
Rosenb <- function(matched_pairs, Gamma_grid = c(1, 1.5, 2, 3),
                   alpha = 0.05) {
  gs <- as.numeric(Gamma_grid)
  if (length(gs) == 0) stop("Rosenb: Gamma_grid is empty")
  pu <- numeric(length(gs))
  pl <- numeric(length(gs))
  W <- NA_real_
  n_pairs <- NA_integer_
  for (k in seq_along(gs)) {
    r <- CnsRos(matched_pairs, Gamma = gs[k])
    pu[k] <- r$p_upper
    pl[k] <- r$p_lower
    W <- r$W
    n_pairs <- r$n_pairs
  }
  crit <- NULL
  for (k in seq_along(gs)) if (pu[k] <= alpha) crit <- gs[k]
  .t1_result(Gamma = gs, p_upper = pu, p_lower = pl,
             gamma_critical = crit, n_pairs = n_pairs, W = W,
             alpha = alpha,
             method = "Rosenbaum signed-rank sensitivity bounds over Gamma")
}
