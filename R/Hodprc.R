# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hodrick-Prescott filter
#'
#' The trend solves the banded normal equations of the penalised least
#' squares problem.  lambda = 0 returns the series itself and
#' lambda -> infinity returns the least-squares straight line; both
#' limits are exact and are what the tests check.
#'
#' Formula: (I + lambda D'D) tau = y with D the second-difference
#'   operator.
#'
#' @param y Numeric series of length at least three.
#' @param lam Non-negative smoothing parameter.
#' @return List with \code{estimate} (penalised objective),
#'   \code{trend}, \code{cycle}, \code{cycle_ss}, \code{roughness},
#'   \code{lam}, \code{n}, \code{method}.
#' @references Hodrick and Prescott (1997), Postwar U.S. business
#'   cycles: an empirical investigation, Journal of Money, Credit and
#'   Banking 29(1):1-16. \doi{10.2307/2953682}
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Hodprc(V)
Hodprc <- function(y, lam = 1600) {
  v <- .s03vec(y)
  n <- length(v)
  if (n < 3L) stop("hodrick_prescott: need at least three observations")
  lv <- as.numeric(lam)
  if (lv < 0) stop("hodrick_prescott: lam must be non-negative")
  K <- diag(1, n)
  for (r in seq_len(n - 2L)) {
    row <- rep(0, n)
    row[r] <- 1; row[r + 1L] <- -2; row[r + 2L] <- 1
    K <- K + lv * (row %o% row)
  }
  tau <- .s03cholsolve(K, v)
  cyc <- v - tau
  rough <- 0
  for (r in seq_len(n - 2L)) rough <- rough + (tau[r] - 2 * tau[r + 1L] + tau[r + 2L])^2
  .t1_result(estimate = sum(cyc * cyc) + lv * rough, trend = tau, cycle = cyc,
             cycle_ss = sum(cyc * cyc), roughness = rough, lam = lv, n = n,
             method = "(I + lambda D'D) tau = y, Hodrick & Prescott (1997)")
}
