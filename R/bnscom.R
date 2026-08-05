# SPDX-License-Identifier: AGPL-3.0-or-later
#' Complier means, principal-strata shares and the resulting ATE bound
#'
#' Under instrument independence and monotonicity the population splits
#' into compliers, always-takers and never-takers, whose shares are
#' identified from the first stage. The instrument reveals the treatment
#' effect only for the compliers; for the other two strata one of the two
#' potential outcomes is never observed at all, so the population ATE is a
#' mixture of one identified piece and two entirely unidentified ones, and
#' the interval simply admits the extremes for those.
#'
#' Derivation. \code{E[y D | Z = z]} sums over strata with \code{d(z) = 1},
#' so differencing in \code{z} leaves only compliers:
#' \code{E[y(1) | c] = (E[y D | Z = 1] - E[y D | Z = 0]) / pi_c} and
#' \code{E[y(0) | c] = (E[y (1 - D) | Z = 0] - E[y (1 - D) | Z = 1]) / pi_c},
#' with \code{pi_c = P(D = 1 | Z = 1) - P(D = 1 | Z = 0)}. Their difference
#' is the Wald ratio. The ATE bound is
#' \code{pi_c LATE + (1 - pi_c) [y_0 - y_1, y_1 - y_0]}.
#'
#' @param y Observed outcome.
#' @param D Binary treatment, coded 0/1.
#' @param Z Binary instrument, coded 0/1.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{late}, \code{pi_c}, \code{pi_a}, \code{pi_n},
#'   \code{e1c}, \code{e0c}, \code{n}.
#' @references Imbens, G. W. and Rubin, D. B. (1997). Estimating outcome
#'   distributions for compliers in instrumental variables models. Review
#'   of Economic Studies 64(4), 555-574. \doi{10.2307/2971731}. Angrist,
#'   J. D., Imbens, G. W. and Rubin, D. B. (1996). Identification of causal
#'   effects using instrumental variables. Journal of the American
#'   Statistical Association 91(434), 444-455.
#'   \doi{10.1080/01621459.1996.10476902}.
#' @export
Bnscom <- function(y, D, Z) {
  yv <- as.numeric(unlist(y))
  dv <- as.numeric(unlist(D))
  zv <- as.numeric(unlist(Z))
  n <- length(yv)
  if (n == 0L) stop("Bnscom: y is empty")
  if (length(dv) != n || length(zv) != n)
    stop("Bnscom: y, D and Z must have the same length")
  if (any(!(c(dv, zv) %in% c(0, 1))))
    stop("Bnscom: D and Z must be coded 0/1")
  n1 <- sum(zv == 1)
  n0 <- sum(zv == 0)
  if (n0 == 0L || n1 == 0L)
    stop("Bnscom: the instrument takes only one value")
  pd1 <- sum(dv[zv == 1]) / n1
  pd0 <- sum(dv[zv == 0]) / n0
  pi_c <- pd1 - pd0
  if (pi_c <= 0)
    stop("Bnscom: non-positive first stage; monotonicity fails")
  e1c <- (sum((yv * dv)[zv == 1]) / n1 - sum((yv * dv)[zv == 0]) / n0) / pi_c
  e0c <- (sum((yv * (1 - dv))[zv == 0]) / n0 -
          sum((yv * (1 - dv))[zv == 1]) / n1) / pi_c
  late <- e1c - e0c
  y0 <- min(yv)
  y1 <- max(yv)
  rest <- 1 - pi_c
  lo <- pi_c * late + rest * (y0 - y1)
  hi <- pi_c * late + rest * (y1 - y0)
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), late = late,
             pi_c = pi_c, pi_a = pd0, pi_n = 1 - pd1,
             e1c = e1c, e0c = e0c, n = n,
             method = "Bound under unknown compliance")
}
