# SPDX-License-Identifier: AGPL-3.0-or-later
#' lppd, effective parameters and WAIC from a matrix of log densities.
#'
#' The average over draws happens INSIDE the log, once per observation,
#' and only then are observations summed. Computed through a log-sum-exp.
#'
#' Formula: lppd = sum_i log( (1/S) sum_s p(y_i | theta^s) );
#'   p_waic = sum_i var_s( log p(y_i | theta^s) );
#'   elpd_waic = lppd - p_waic; WAIC = -2 elpd_waic
#'
#' @param logdens Matrix, row s column i holding log p(y_i | theta^s).
#' @return List with \code{lppd}, \code{p_waic}, \code{elpd_waic},
#'   \code{waic}, \code{pointwise_lppd}, \code{pointwise_var}, \code{S},
#'   \code{n}.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari & Rubin (2013),
#'   Bayesian Data Analysis, 3rd edition, Section 7.2, equation (7.5).
#'   Fetched as the full text of the book from the author's own copy.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Lppd(V)
Lppd <- function(logdens) {
  L <- as.matrix(logdens)
  S <- nrow(L); n <- ncol(L)
  if (S < 2L) stop("at least two posterior draws are required")
  pl <- pv <- numeric(n)
  for (i in seq_len(n)) {
    col <- L[, i]
    m <- max(col)
    pl[i] <- m + log(sum(exp(col - m)) / S)
    pv[i] <- stats::var(col)
  }
  tot <- sum(pl); pw <- sum(pv); el <- tot - pw
  .t1_result(lppd = tot, p_waic = pw, elpd_waic = el, waic = -2 * el,
             pointwise_lppd = pl, pointwise_var = pv, S = as.numeric(S),
             n = as.numeric(n),
             method = "lppd and WAIC, BDA3 Section 7.2 equation (7.5)")
}
