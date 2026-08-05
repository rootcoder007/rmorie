# SPDX-License-Identifier: AGPL-3.0-or-later
#' Interior-point linear programming by the logarithmic barrier
#'
#' Damped Newton steps on the barrier objective from a strictly
#' feasible start.  The central-path point satisfies
#' c'x(tau) - p* <= (m + n) tau, a bound that does not run through this
#' code and is what the tests check against the simplex optimum.
#'
#' Formula: minimise c'x - tau [sum_i log(b_i - a_i'x) + sum_j log x_j].
#'
#' @param c Objective coefficients.
#' @param A Constraint matrix.
#' @param b Right-hand side.
#' @param x0 Strictly feasible starting point.
#' @param tau Positive barrier parameter.
#' @param iters Newton iteration cap.
#' @return List with \code{estimate}, \code{x}, \code{objective},
#'   \code{duality_bound}, \code{newton_decrement}, \code{tau},
#'   \code{n}, \code{method}.
#' @references Karmarkar (1984), A new polynomial-time algorithm for
#'   linear programming, Combinatorica 4(4):373-395.
#'   \doi{10.1007/BF02579150}; Boyd and Vandenberghe (2004), Convex
#'   Optimization, CUP, sect. 11.2.
#' @export
Intlpa <- function(c, A, b, x0, tau = 0.01, iters = 60) {
  cv <- .s03vec(c); M <- .s03mat(A); bv <- .s03vec(b); x <- .s03vec(x0)
  m <- nrow(M); n <- length(cv)
  if (m == 0L || n == 0L) stop("interior_point_lp: empty problem")
  if (length(bv) != m) stop("interior_point_lp: A and b have different row counts")
  if (length(x) != n) stop("interior_point_lp: x0 has the wrong length")
  t <- as.numeric(tau)
  if (t <= 0) stop("interior_point_lp: tau must be positive")
  if (min(bv - as.numeric(M %*% x)) <= 0 || min(x) <= 0)
    stop("interior_point_lp: x0 must be strictly feasible")
  lam <- Inf
  for (k in seq_len(as.integer(iters))) {
    s <- bv - as.numeric(M %*% x)
    g <- cv + t * as.numeric(t(M) %*% (1 / s)) - t / x
    H <- t * (t(M) %*% (M / (s * s))) + diag(t / (x * x), n)
    step <- .s03cholsolve(H, -g)
    lam <- sum(-g * step)
    if (lam / 2 <= 1e-14) break
    a <- 1
    for (i in seq_len(80)) {
      xn <- x + a * step
      if (min(xn) > 0 && min(bv - as.numeric(M %*% xn)) > 0) break
      a <- a / 2
    }
    x <- x + a * step
  }
  .t1_result(estimate = sum(cv * x), x = x, objective = sum(cv * x),
             duality_bound = (m + n) * t, newton_decrement = lam, tau = t,
             n = n,
             method = "log-barrier Newton central path, Karmarkar (1984); Boyd & Vandenberghe sect. 11.2")
}
