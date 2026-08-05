# SPDX-License-Identifier: AGPL-3.0-or-later
#' Newton-Raphson minimisation
#'
#' Formula: \code{x_{t+1} = x_t - H(x_t)^-1 g(x_t)}.
#'
#' The step is obtained by SOLVING \code{H d = -g}, never by forming
#' \code{H^-1}: inverting costs three times the work and loses the
#' conditioning the solve keeps. A general solve is used rather than a
#' Cholesky factorisation, because \code{H} is only guaranteed positive
#' definite at a minimum, not on the way to one -- on a log-likelihood
#' being maximised the caller passes the Hessian of the log-likelihood
#' and the method descends on its negative.
#'
#' Determinism: a fixed iteration cap with a gradient-norm stopping
#' rule; no line search, no random restarts.
#'
#' @param f Objective, \code{f(x)} returning a scalar.
#' @param grad_f Gradient, \code{grad_f(x)} returning length p.
#' @param hess_f Hessian, \code{hess_f(x)} returning p by p.
#' @param x0 Starting point, length p.
#' @param n_iter Maximum iterations.
#' @param tol Stop when the Euclidean gradient norm falls below this.
#' @return List with \code{x}, \code{estimate}, \code{fval},
#'   \code{grad_norm}, \code{iterations}, \code{converged}, \code{p}.
#' @references Newton, I. (1669/1711). De analysi per aequationes
#'   numero terminorum infinitas; Raphson, J. (1690). Analysis
#'   aequationum universalis. The modern statement is Nocedal, J. &
#'   Wright, S. J. (2006), Numerical Optimization, 2nd ed., Springer,
#'   algorithm 3.2.
#' @export
Newraf <- function(f, grad_f, hess_f, x0, n_iter = 50, tol = 1e-12) {
  x <- as.numeric(x0)
  p <- length(x)
  if (p == 0L) stop("Newraf: x0 is empty")
  nit <- as.integer(n_iter)
  if (nit < 0L) stop("Newraf: n_iter must be non-negative")
  it <- 0L
  g <- as.numeric(grad_f(x))
  if (length(g) != p) stop("Newraf: grad_f returned the wrong length")
  gn <- sqrt(sum(g * g))
  while (it < nit && gn > tol) {
    H <- as.matrix(hess_f(x))
    if (nrow(H) != p || ncol(H) != p)
      stop("Newraf: hess_f returned the wrong shape")
    d <- as.numeric(solve(H, -g))
    x <- x + d
    it <- it + 1L
    g <- as.numeric(grad_f(x))
    gn <- sqrt(sum(g * g))
  }
  fv <- as.numeric(f(x))
  .t1_result(x = x, estimate = fv, fval = fv, grad_norm = gn,
             iterations = it, converged = if (gn <= tol) 1 else 0, p = p,
             method = "Newton-Raphson")
}
