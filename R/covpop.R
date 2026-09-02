# SPDX-License-Identifier: AGPL-3.0-or-later

#' Coverage correction of sampling weights to known population totals
#'
#' Formula: w_i' = w_i * (N_target_h / hat N_h)
#'
#' Within each post-stratum h the weights are rescaled so that they sum
#' exactly to the known population count.  By construction the adjusted
#' weights reproduce the control totals, which is the check the method
#' is built to satisfy.
#'
#' @param y Survey variable, length n.
#' @param weights Design weights before correction, length n.
#' @param target_totals Known population count per post-stratum, in the
#'   order the stratum labels first appear in \code{strata}.
#' @param strata Post-stratum label per unit, or NULL for one stratum.
#' @return List with \code{estimate} (post-stratified mean of y),
#'   \code{w_adj}, \code{factors}, \code{total}, \code{n}, \code{method}.
#' @references Sarndal, Swensson & Wretman (1992), Model Assisted Survey
#'   Sampling, Springer, section 7.6.
#' @export
#' @examples
#' Covpop(y = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8), target_totals = 5L)
Covpop <- function(y, weights, target_totals, strata = NULL) {
  y <- .s03vec(y)
  w <- .s03vec(weights)
  tt <- .s03vec(target_totals)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (length(w) != n) stop("y and weights must have the same length")
  ids <- if (is.null(strata)) rep(0L, n) else strata
  if (length(ids) != n) stop("y and strata must have the same length")
  keys <- unique(ids)
  if (length(tt) != length(keys))
    stop("target_totals must have one entry per stratum")
  factors <- numeric(length(keys))
  w_adj <- w
  for (j in seq_along(keys)) {
    idx <- which(ids == keys[j])
    nh <- sum(w[idx])
    if (nh <= 0) stop("stratum has zero estimated size; cannot correct")
    f <- tt[j] / nh
    factors[j] <- f
    w_adj[idx] <- w[idx] * f
  }
  tot <- sum(w_adj)
  .t1_result(estimate = sum(w_adj * y) / tot, w_adj = w_adj,
             factors = factors, total = tot, n = n,
             method = "coverage correction of weights to known population totals")
}
