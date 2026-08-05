# SPDX-License-Identifier: AGPL-3.0-or-later
#' Solve transport once part of the source marginal has been deleted
#'
#' Deleting mass is the discrete face of Caffarelli and McCann's obstacle
#' problem: the surviving support is an unknown free boundary, so the
#' shifted problem is not the original plan with rows thinned out -- the
#' optimum reorganises. \code{delta} is taken as a total to remove, spread
#' over the bins in proportion to \code{a}, or as a per-bin vector when a
#' vector is supplied.
#'
#' Formula: solve \code{min_T <T,C>} with \code{a' = a - delta} and total
#' transported mass \code{sum(a')}, the partial-transport problem of
#' Caffarelli and McCann (2010).
#'
#' @param a,b Source and target weights.
#' @param C Ground cost, n by m.
#' @param delta Mass removed from the source, in total or per bin.
#' @return List with \code{T}, \code{cost}, \code{a_shift},
#'   \code{removed}, \code{mass}, \code{n}, \code{m}.
#' @references Caffarelli, L. A. and McCann, R. J. (2010). Annals of
#'   Mathematics 171(2):673-730. \doi{10.4007/annals.2010.171.673}.
#' @export
Otmarsh <- function(a, b, C, delta) {
  aa <- .ot_hist(a); bb <- .ot_hist(b)
  Cm <- as.matrix(C)
  n <- length(aa); m <- length(bb)
  if (nrow(Cm) != n || ncol(Cm) != m)
    stop("cost matrix does not match the marginals")
  dv <- as.numeric(delta)
  if (length(dv) == 1L) {
    tot <- sum(aa)
    if (tot <= 0) stop("the source marginal has no mass to remove")
    d <- dv * aa / tot
  } else if (length(dv) == n) {
    d <- dv
  } else stop("delta must be a scalar or one value per source bin")
  ash <- aa - d
  if (any(ash < -1e-12)) stop("delta removes more mass than a bin holds")
  ash[ash < 0] <- 0
  pp <- .ot_partial_plan(ash, bb, Cm, sum(ash))
  .t1_result(T = pp$T, cost = pp$cost, a_shift = ash, removed = sum(d),
             mass = sum(pp$T), n = n, m = m,
             method = "Optimal transport under a marginal shift")
}
