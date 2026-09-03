# SPDX-License-Identifier: AGPL-3.0-or-later
#' Exact earth mover's distance between two histograms
#'
#' The linear program is solved outright, not smoothed: the answer is the
#' true optimum of the Kantorovich problem rather than an entropic
#' relaxation of it. The plan returned is a vertex of the transport
#' polytope, so at most \code{n + m - 1} of its entries are non-zero.
#'
#' Formula: \code{min_T <T, C>} subject to \code{T 1 = a},
#' \eqn{T prime 1 = b}, \code{T >= 0} -- Kantorovich's problem, eq. (2.11) of
#' Peyre and Cuturi (2019). Solved by the transportation simplex:
#' north-west-corner start, potentials from the basis tree, MODI pivoting.
#'
#' @param a Source weights, length n, non-negative.
#' @param b Target weights, length m, same total mass as \code{a}.
#' @param C Ground cost, n by m.
#' @return List with \code{T}, \code{cost}, \code{n}, \code{m},
#'   \code{n_basic}.
#' @references Rubner, Y., Tomasi, C. and Guibas, L. J. (2000).
#'   International Journal of Computer Vision 40(2):99-121.
#'   \doi{10.1023/A:1026543900054}. Peyre and Cuturi (2019),
#'   Computational Optimal Transport, eq. (2.11).
#' @export
#' @examples
#' Otemd(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4,
#' 5, 6, 7, 8))
Otemd <- function(a, b, C) {
  aa <- .ot_hist(a)
  bb <- .ot_hist(b)
  r <- .ot_emd(aa, bb, as.matrix(C))
  .t1_result(T = r$T, cost = r$cost, n = length(aa), m = length(bb),
             n_basic = sum(r$T > 1e-15),
             method = "Exact optimal transport (transportation simplex)")
}
