# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transport only \code{m} units of mass and leave the rest in place
#'
#' The Kantorovich problem insists that everything move. Partial transport
#' relaxes that to an inequality and asks only that a fixed budget of mass
#' be matched, which is the right formulation when the two measures
#' overlap on a sub-region -- Caffarelli and McCann's free boundary is
#' precisely the edge of the matched part. Solved exactly by padding the
#' cost with a zero-price dummy row and column.
#'
#' Formula: \code{min_T <T,C>} subject to \code{T 1 <= a},
#' \code{T' 1 <= b}, \code{sum T = m}.
#'
#' @param a,b Source and target weights.
#' @param C Ground cost, n by k.
#' @param m Mass to transport; must not exceed either total.
#' @return List with \code{T}, \code{cost}, \code{mass}, \code{a_left},
#'   \code{b_left}, \code{n}, \code{m_bins}.
#' @references Caffarelli, L. A. and McCann, R. J. (2010). Annals of
#'   Mathematics 171(2):673-730. \doi{10.4007/annals.2010.171.673}.
#' @export
Otpr <- function(a, b, C, m) {
  aa <- .ot_hist(a); bb <- .ot_hist(b)
  Cm <- as.matrix(C)
  if (nrow(Cm) != length(aa) || ncol(Cm) != length(bb))
    stop("cost matrix does not match the marginals")
  pp <- .ot_partial_plan(aa, bb, Cm, m)
  .t1_result(T = pp$T, cost = pp$cost, mass = sum(pp$T),
             a_left = aa - rowSums(pp$T), b_left = bb - colSums(pp$T),
             n = length(aa), m_bins = length(bb),
             method = "Partial optimal transport")
}
