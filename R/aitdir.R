# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet density on the simplex
#'
#' The density is the standard one,
#' f(x | alpha) = Gamma(sum alpha_i) / prod Gamma(alpha_i) * prod x_i^(alpha_i - 1),
#' on the open (D-1)-simplex.  The stub cites Wilks (1962), Mathematical
#' Statistics, Wiley, which gives the Dirichlet as the multivariate
#' generalisation of the beta; that text was not retrievable here, so the
#' density is written in its standard published form and pinned by two
#' independent identities instead of a page number: alpha = (1, ..., 1) makes
#' it constant and equal to Gamma(D) = (D-1)!, the reciprocal volume of the
#' unit simplex, and D = 2 collapses it to the Beta(alpha_1, alpha_2) density
#' in x_1.  Both are checked as anchors.  The value is computed on the log
#' scale and exponentiated once so the constant never overflows.
#'
#' @param x a point of the open simplex, strictly positive and summing to one
#'   to within 1e-8.
#' @param alpha strictly positive concentration parameters, same length as x.
#' @return list: f, estimate, log_f, log_const, alpha0, D, method.
#' @keywords internal
#' @examples
#' Aitdir(c(0.2, 0.3, 0.5), c(2, 3, 4))$f
#' @export
Aitdir <- function(x, alpha) {
  xx <- as.numeric(.s03vec(x))
  aa <- as.numeric(.s03vec(alpha))
  D <- length(xx)
  if (D < 2L) stop("dirichlet_density: a composition needs at least 2 parts")
  if (length(aa) != D) stop("dirichlet_density: x and alpha have different lengths")
  if (any(!(aa > 0))) stop("dirichlet_density: alpha must be strictly positive")
  s <- 0
  for (v in xx) {
    if (!(v > 0)) stop("dirichlet_density: x must be strictly inside the simplex")
    s <- s + v
  }
  if (abs(s - 1) > 1e-8) stop("dirichlet_density: x does not sum to one")
  a0 <- 0
  for (v in aa) a0 <- a0 + v
  lc <- lgamma(a0)
  for (v in aa) lc <- lc - lgamma(v)
  lf <- lc
  for (i in seq_len(D)) lf <- lf + (aa[i] - 1) * log(xx[i])
  list(f = exp(lf), estimate = exp(lf), log_f = lf, log_const = lc, alpha0 = a0, D = D,
       method = "f(x|alpha) = Gamma(sum alpha)/prod Gamma(alpha_i) prod x_i^(alpha_i-1)")
}
