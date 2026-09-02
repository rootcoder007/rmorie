# SPDX-License-Identifier: AGPL-3.0-or-later
#' Identified set as the zero-level set of the moment-inequality criterion
#'
#' The set estimate and the confidence set are different objects and the
#' difference is visible here: this function reports the level set at
#' criterion zero, with no critical value inflating it. Every candidate
#' inside the interval satisfies both inequalities in sample and scores
#' exactly zero; every candidate outside scores strictly positive.
#'
#' Formula: \code{H = {theta : q(theta) = 0}} with \code{q} the sum of
#' squared positive parts, Molinari (2021) equations (4.2) and (4.4).
#'
#' @param y Lower end of each observation's interval.
#' @param X Upper end of each observation's interval, same length as
#'   \code{y}.
#' @param moments Candidate parameter values at which the criterion is
#'   evaluated.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{n_in_set}, \code{q_min}, \code{q_max_stat}, \code{n}.
#' @references Chernozhukov, V., Hong, H. and Tamer, E. (2007). Estimation
#'   and confidence regions for parameter sets in econometric models.
#'   Econometrica 75(5), 1243-1284.
#'   \doi{10.1111/j.1468-0262.2007.00794.x}. Criterion and level set as
#'   equations (4.2), (4.3) and (4.4) of Molinari, F. (2021), Handbook of
#'   Econometrics 7A (arXiv:2004.11751 pp. 88-89).
#' @export
#' @examples
#' Bnsiii(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), moments = c(1, 2, 3, 4, 5, 6, 7, 8))
Bnsiii <- function(y, X, moments) {
  yl <- as.numeric(unlist(y))
  yu <- as.numeric(unlist(X))
  if (length(yl) != length(yu))
    stop("Bnsiii: y and X must have the same length")
  iv <- .bnd_interval(cbind(yl, yu), "Bnsiii")
  st <- .bnd_mistats(iv$yl, iv$yu)
  grid <- as.numeric(unlist(moments))
  if (length(grid) == 0L) stop("Bnsiii: moments grid is empty")
  q <- vapply(grid, .bnd_crit, 0, st = st)
  qm <- vapply(grid, .bnd_critmax, 0, st = st)
  .t1_result(lower = st$mL, upper = st$mU, width = st$mU - st$mL,
             n_in_set = sum(q <= 0), q_min = min(q),
             q_max_stat = max(qm), n = st$n,
             method = "Identification by intersection of inequalities")
}
