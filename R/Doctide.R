# SPDX-License-Identifier: AGPL-3.0-or-later
#' de Chaisemartin-D'Haultfoeuille DID_M estimator
#'
#' Robust to heterogeneous and dynamic treatment effects.  For every
#' period \code{t >= 2},
#' \code{DID_{+,t}} contrasts the mean outcome change of the groups that
#' switch into treatment against the groups that stay untreated, and
#' \code{DID_{-,t}} contrasts the groups that stay treated against the
#' groups that switch out.  Either is set to zero when one of its two
#' donor sets is empty, as the paper prescribes.  The aggregate is
#' \code{DID_M = sum_t (N10_t/NS DID_{+,t} + N01_t/NS DID_{-,t})} with
#' \code{NS = sum_t (N10_t + N01_t)}.
#'
#' @param y Outcome, long format, one entry per unit-period.
#' @param D Binary treatment indicator.
#' @param unit,time Unit and period identifiers.
#' @return List with \code{estimate}, \code{periods}, \code{did_plus},
#'   \code{did_minus}, \code{n10}, \code{n01}, \code{n_switch},
#'   \code{n_units}, \code{n}.
#' @references de Chaisemartin, C. and D'Haultfoeuille, X. (2020).
#'   Two-way fixed effects estimators with heterogeneous treatment
#'   effects. American Economic Review 110(9), 2964-2996; working paper
#'   arXiv:1803.08807, page 16.
#' @export
Doctide <- function(y, D, unit, time) {
  yv <- .s03vec(y); dv <- .s03vec(D)
  n <- length(yv)
  if (n == 0L) stop("Doctide: empty input, y has no observations")
  u <- as.character(unit); tt <- as.numeric(time)
  if (length(dv) != n || length(u) != n || length(tt) != n)
    stop("Doctide: y, D, unit and time must have the same length")
  if (any(dv != 0 & dv != 1)) stop("Doctide: D must be binary 0/1")
  key <- paste(u, tt, sep = "\r")
  units <- unique(u); per <- sort(unique(tt))
  if (length(per) < 2L) stop("Doctide: DID_M needs at least two periods")
  yof <- yv; names(yof) <- key
  dof <- dv; names(dof) <- key
  dplus <- numeric(0); dminus <- numeric(0)
  n10 <- numeric(0); n01 <- numeric(0)
  for (j in seq_along(per)[-1L]) {
    tc <- per[j]; tp <- per[j - 1L]
    swin <- numeric(0); st0 <- numeric(0)
    st1 <- numeric(0); swout <- numeric(0)
    for (g in units) {
      k1 <- paste(g, tc, sep = "\r"); k0 <- paste(g, tp, sep = "\r")
      if (is.na(yof[k1]) || is.na(yof[k0])) next
      if (!(k1 %in% key) || !(k0 %in% key)) next
      dy <- yof[[k1]] - yof[[k0]]
      a <- dof[[k1]]; b <- dof[[k0]]
      if (a == 1 && b == 0) swin <- c(swin, dy)
      else if (a == 0 && b == 0) st0 <- c(st0, dy)
      else if (a == 1 && b == 1) st1 <- c(st1, dy)
      else swout <- c(swout, dy)
    }
    dp <- if (length(swin) && length(st0)) .s03mean(swin) - .s03mean(st0) else 0
    dm <- if (length(st1) && length(swout)) .s03mean(st1) - .s03mean(swout) else 0
    dplus <- c(dplus, dp); dminus <- c(dminus, dm)
    n10 <- c(n10, length(swin)); n01 <- c(n01, length(swout))
  }
  ns <- sum(n10) + sum(n01)
  if (ns <= 0) stop("Doctide: no group switches treatment, DID_M undefined")
  did_m <- sum((n10 / ns) * dplus + (n01 / ns) * dminus)
  .t1_result(estimate = did_m, periods = per[-1L], did_plus = dplus,
             did_minus = dminus, n10 = n10, n01 = n01, n_switch = ns,
             n_units = length(units), n = n,
             method = "de Chaisemartin-D'Haultfoeuille heterogeneous DID")
}
