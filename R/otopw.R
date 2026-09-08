# SPDX-License-Identifier: AGPL-3.0-or-later
#' Warm-started Sinkhorn from previously optimised potentials
#'
#' Schmitzer (2019), SIAM J. Sci. Comput. 41(3), A1443-A1481
#' (arXiv:1610.06519 -- FETCHED): the epsilon-scaling scheme IS warm
#' starting -- the potentials solved at one regularisation initialise the
#' next, which is what makes small-epsilon problems tractable.  The
#' scaling factors are recovered by u = exp(f/eps), v = exp(g/eps), the
#' substitution the paper's section 3 makes explicit.  The saving is
#' reported rather than asserted: n_iter and n_iter_cold are the counts
#' with and without the warm start.
#'
#' @param a,b marginals.
#' @param C cost matrix.
#' @param epsilon regularisation.
#' @param f0,g0 the warm-start potentials.
#' @param max_iter,tol iteration controls.
#' @return list: T, f, g, estimate, cost, u, v, n_iter, n_iter_cold,
#'   saved, method.
#' @keywords internal
#' @examples
#' Sinkhwarm(c(0.5, 0.5), c(0.5, 0.5), matrix(c(0, 1, 1, 0), 2, 2),
#'           0.5, c(0, 0), c(0, 0))$saved
#' @export
Sinkhwarm <- function(a, b, C, epsilon = 0.1, f0 = NULL, g0 = NULL,
                      max_iter = 200, tol = 1e-13) {
  warm <- Sinkhlog(a, b, C, epsilon, max_iter, tol, f0, g0)
  cold <- Sinkhlog(a, b, C, epsilon, max_iter, tol, NULL, NULL)
  e <- as.numeric(epsilon)
  list(T = warm$T, f = warm$f, g = warm$g, estimate = warm$cost,
       cost = warm$cost, u = exp(warm$f / e), v = exp(warm$g / e),
       n_iter = warm$n_iter, n_iter_cold = cold$n_iter,
       saved = cold$n_iter - warm$n_iter,
       method = "Warm-started log-domain Sinkhorn (Schmitzer 2019 epsilon-scaling)")
}
