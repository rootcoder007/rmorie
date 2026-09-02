# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gradient descent
#'
#' On the quadratic f(x) = x^2 the iteration has the exact closed form
#' x_t = x_0 (1 - 2 lr)^t, so it converges for lr < 1, oscillates at
#' lr = 1 and diverges beyond -- the behaviour the tests pin down.
#'
#' Formula: x <- x - lr * grad f(x).
#'
#' @param f Objective function of a numeric vector.
#' @param grad_f Gradient function returning a vector of the same length.
#' @param x0 Starting point.
#' @param lr Positive step size.
#' @param steps Number of iterations.
#' @param tol Stop when the gradient norm falls below this.
#' @return List with \code{estimate} (final objective), \code{x},
#'   \code{f_path}, \code{grad_norm}, \code{steps_used},
#'   \code{converged}, \code{n}, \code{method}.
#' @references Cauchy (1847), Methode generale pour la resolution des
#'   systemes d'equations simultanees, C. R. Acad. Sci. Paris 25:536-538.
#' @export
#' @examples
#' Gradds(function(x) sum(x^2), function(x) 2 * x, x0 = c(1, 1))
Gradds <- function(f, grad_f, x0, lr = 0.1, steps = 100, tol = 1e-12) {
  x <- .s03vec(x0)
  if (length(x) == 0L) stop("gradient_descent: x0 is empty")
  if (!is.function(f) || !is.function(grad_f)) stop("gradient_descent: f and grad_f must be callable")
  if (as.numeric(lr) <= 0) stop("gradient_descent: lr must be positive")
  ns <- as.integer(steps)
  if (ns < 1L) stop("gradient_descent: steps must be at least 1")
  path <- as.numeric(f(x))
  gn <- Inf
  used <- 0L
  for (i in seq_len(ns)) {
    g <- .s03vec(grad_f(x))
    if (length(g) != length(x)) stop("gradient_descent: gradient has the wrong length")
    gn <- sqrt(sum(g * g))
    if (gn <= tol) break
    x <- x - as.numeric(lr) * g
    path <- c(path, as.numeric(f(x)))
    used <- used + 1L
  }
  .t1_result(estimate = as.numeric(f(x)), x = x, f_path = path,
             grad_norm = gn, steps_used = used, converged = gn <= tol,
             n = length(x), method = "x <- x - lr grad f(x), Cauchy (1847)")
}
