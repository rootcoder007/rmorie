# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bound the complier effect when defiers are not ruled out
#'
#' The first stage identifies only the NET complier share,
#' \code{pi_c - pi_d}; monotonicity is what turns that into \code{pi_c}.
#' Without it the Wald ratio is a difference of two effects, and the data
#' bound the defier share only through
#' \code{pi_c <= min(P(D = 1 | Z = 1), P(D = 0 | Z = 0))}. The interval
#' widens with the admissible defier share, so the union over admissible
#' \code{pi_c} is attained at that maximum, and collapses to the Wald
#' ratio exactly when the maximum equals the net share.
#'
#' Derivation: \code{ITT_y = pi_c E[D | c] - pi_d E[D | d]} with
#' \code{|E[D | d]| <= y_1 - y_0}, so with \code{pi_d = pi_c - ITT_D},
#' \code{LATE in [(ITT_y - pi_d R) / pi_c, (ITT_y + pi_d R) / pi_c]} at
#' \code{pi_c = pi_c_max}.
#'
#' @param y Observed outcome.
#' @param D Binary treatment, coded 0/1.
#' @param Z Binary instrument, coded 0/1.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{wald}, \code{pi_net}, \code{pi_c_max},
#'   \code{pi_d_max}, \code{itt_y}, \code{n}.
#' @references de Chaisemartin, C. (2017). Tolerating defiance? Local
#'   average treatment effects without monotonicity. Quantitative
#'   Economics 8(2), 367-396. \doi{10.3982/QE601} -- the stub's
#'   attribution, for the problem; the paper was not accessible, so the
#'   interval above is the elementary mixture bound, derived here rather
#'   than taken from it.
#' @export
Bndnmt <- function(y, D, Z) {
  yv <- as.numeric(unlist(y))
  dv <- as.numeric(unlist(D))
  zv <- as.numeric(unlist(Z))
  n <- length(yv)
  if (n == 0L) stop("Bndnmt: y is empty")
  if (length(dv) != n || length(zv) != n)
    stop("Bndnmt: y, D and Z must have the same length")
  if (any(!(c(dv, zv) %in% c(0, 1))))
    stop("Bndnmt: D and Z must be coded 0/1")
  n1 <- sum(zv == 1)
  n0 <- n - n1
  if (n0 == 0L || n1 == 0L)
    stop("Bndnmt: the instrument takes only one value")
  itt_y <- mean(yv[zv == 1]) - mean(yv[zv == 0])
  pd1 <- mean(dv[zv == 1])
  pd0 <- mean(dv[zv == 0])
  net <- pd1 - pd0
  if (net <= 0) stop("Bndnmt: non-positive net first stage")
  pc_max <- min(pd1, 1 - pd0)
  pd_max <- pc_max - net
  rng <- max(yv) - min(yv)
  lo <- (itt_y - pd_max * rng) / pc_max
  hi <- (itt_y + pd_max * rng) / pc_max
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), wald = itt_y / net,
             pi_net = net, pi_c_max = pc_max, pi_d_max = pd_max,
             itt_y = itt_y, n = n,
             method = "Bound when monotonicity violated")
}
