# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dual potentials of the entropic transport problem
#'
#' Entropic smoothing turns a constrained linear program into an
#' unconstrained smooth concave maximisation in the two potentials, and
#' that is what makes stochastic and large-scale solvers possible. The
#' potentials are computed in the log domain, so the result survives small
#' \code{epsilon} where the exponentiated form underflows.
#'
#' Formula: \code{max_{f,g} <a,f> + <b,g> - eps sum_ij exp((f_i + g_j -
#' C_ij)/eps)} -- Genevay et al. (2016) eq. (2); Peyre and Cuturi (2019)
#' eq. (4.30). At the optimum the exponential sum is the total mass, so
#' the dual value reduces to \code{<a,f> + <b,g> - eps}.
#'
#' @param a,b Marginals, equal total mass.
#' @param C Ground cost, n by m.
#' @param epsilon Regularisation strength, positive.
#' @param max_iter Sinkhorn sweeps; fixed count.
#' @return List with \code{f}, \code{g}, \code{dual_value},
#'   \code{primal_cost}, \code{n}, \code{m}.
#' @references Genevay, A., Cuturi, M., Peyre, G. and Bach, F. (2016).
#'   Advances in Neural Information Processing Systems 29:3440-3448.
#' @export
#' @examples
#' Otreg(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, C = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L)
Otreg <- function(a, b, C, epsilon, max_iter = 200) {
  aa <- .ot_hist(a); bb <- .ot_hist(b)
  Cm <- as.matrix(C)
  n <- length(aa); m <- length(bb)
  if (nrow(Cm) != n || ncol(Cm) != m)
    stop("cost matrix does not match the marginals")
  s <- .ot_sinkhorn(aa, bb, Cm, as.numeric(epsilon), max_iter)
  tot <- sum(s$T)
  dual <- sum(aa[aa > 0] * s$f[aa > 0]) + sum(bb[bb > 0] * s$g[bb > 0]) -
    as.numeric(epsilon) * tot
  .t1_result(f = s$f, g = s$g, dual_value = dual,
             primal_cost = sum(s$T * Cm), n = n, m = m,
             method = "Entropic optimal transport dual")
}
