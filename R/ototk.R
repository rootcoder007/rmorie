# SPDX-License-Identifier: AGPL-3.0-or-later
#' Value of the dual objective for a candidate pair of potentials
#'
#' The dual turns a search over couplings into a search over two
#' functions on the marginals. Any feasible pair gives a lower bound on
#' the transport cost, so this is the optimum only when the potentials
#' are. Feasibility against a cost matrix is not checked.
#'
#' Formula: \code{<a, f> + <b, g>}.
#'
#' @param a,b Source and target marginals.
#' @param f,g Kantorovich potentials.
#' @return List with \code{dual_val}, \code{estimate}, \code{n}, \code{m}.
#' @references Villani, C. (2003). Topics in Optimal Transportation, AMS
#'   GSM 58, theorem 1.3.
#' @export
Ototk <- function(a, b, f, g) {
  val <- sum(as.numeric(a) * as.numeric(f)) + sum(as.numeric(b) * as.numeric(g))
  .t1_result(dual_val = val, estimate = val, n = length(a), m = length(b),
             method = "Kantorovich dual objective value")
}
