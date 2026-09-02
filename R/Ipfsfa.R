# SPDX-License-Identifier: AGPL-3.0-or-later
#' Barrier interior-point solver for smooth inequality-constrained problems
#'
#' The core of the IPOPT algorithm: a barrier subproblem solved
#' approximately for a decreasing sequence of mu, by Newton steps with a
#' backtracking line search that keeps the iterate strictly feasible.
#' Derivatives are central differences with a fixed step, so no symbolic
#' gradient is required and both language arms follow the same
#' trajectory.  When a difference stencil would step outside the
#' feasible set the barrier subproblem is declared finished: the iterate
#' is then within one difference step of the boundary, which is the
#' accuracy a barrier method offers there anyway.
#'
#' Formula: minimise f(x) - mu sum_i log(-g_i(x)) with g_i(x) <= 0.
#'
#' @param f Objective function of a numeric vector.
#' @param constraints List of functions g_i with g_i(x) <= 0 feasible.
#' @param x0 Strictly feasible starting point.
#' @param mu0 Initial barrier parameter.
#' @param outer Number of mu reductions.
#' @param inner Newton steps per barrier subproblem.
#' @return List with \code{estimate}, \code{x}, \code{objective},
#'   \code{max_violation}, \code{mu_final}, \code{n}, \code{method}.
#' @references Waechter and Biegler (2006), On the implementation of an
#'   interior-point filter line-search algorithm for large-scale
#'   nonlinear programming, Mathematical Programming 106(1):25-57.
#'   \doi{10.1007/s10107-004-0559-y}
#' @export
Ipfsfa <- function(f, constraints, x0, mu0 = 1, outer = 8, inner = 30) {
  x <- .s03vec(x0)
  n <- length(x)
  if (n == 0L) stop("ipopt_solver: x0 is empty")
  if (!is.function(f)) stop("ipopt_solver: f must be callable")
  cons <- as.list(constraints)
  for (g in cons) {
    if (!is.function(g)) stop("ipopt_solver: every constraint must be callable")
    if (as.numeric(g(x)) >= 0) stop("ipopt_solver: x0 must be strictly feasible")
  }
  mu <- as.numeric(mu0)
  if (mu <= 0) stop("ipopt_solver: mu0 must be positive")
  hh <- 1e-5
  phi <- function(x, mu) {
    v <- as.numeric(f(x))
    for (g in cons) {
      gv <- as.numeric(g(x))
      if (gv >= 0) return(Inf)
      v <- v - mu * log(-gv)
    }
    v
  }
  shift <- function(x, idx, amt) { x[idx] <- x[idx] + amt
  x }
  for (o in seq_len(as.integer(outer))) {
    for (k in seq_len(as.integer(inner))) {
      base <- phi(x, mu)
      plus <- vapply(seq_len(n), function(j) phi(shift(x, j, hh), mu), 0)
      minus <- vapply(seq_len(n), function(j) phi(shift(x, j, -hh), mu), 0)
      cross <- list()
      if (n > 1L) for (j in seq_len(n - 1L)) for (k2 in seq(j + 1L, n))
        for (sj in c(1, -1)) for (sk in c(1, -1))
          cross[[paste(j, k2, sj, sk)]] <- phi(shift(x, c(j, k2), c(sj * hh, sk * hh)), mu)
      vals <- c(base, plus, minus, unlist(cross))
      if (any(!is.finite(vals))) break
      g <- (plus - minus) / (2 * hh)
      H <- matrix(0, n, n)
      for (j in seq_len(n)) {
        H[j, j] <- (plus[j] - 2 * base + minus[j]) / (hh * hh) + 1e-8
        if (j < n) for (k2 in seq(j + 1L, n)) {
          v <- (cross[[paste(j, k2, 1, 1)]] - cross[[paste(j, k2, 1, -1)]] -
                cross[[paste(j, k2, -1, 1)]] + cross[[paste(j, k2, -1, -1)]]) / (4 * hh * hh)
          H[j, k2] <- v
          H[k2, j] <- v
        }
      }
      step <- tryCatch(.s03cholsolve(H, -g), error = function(e) -g)
      a <- 1
      moved <- FALSE
      for (i in seq_len(60)) {
        xn <- x + a * step
        fv <- phi(xn, mu)
        if (!is.na(fv) && fv < base) { x <- xn
        moved <- TRUE
        break }
        a <- a / 2
      }
      if (!moved) break
    }
    mu <- mu * 0.2
  }
  viol <- if (length(cons)) max(vapply(cons, function(g) as.numeric(g(x)), 0)) else -1
  .t1_result(estimate = as.numeric(f(x)), x = x, objective = as.numeric(f(x)),
             max_violation = viol, mu_final = mu, n = n,
             method = "decreasing-mu log-barrier with Newton steps, Waechter & Biegler (2006)")
}
