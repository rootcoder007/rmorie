# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cyclic block coordinate descent, exact minimisation per block
#'
#' Tseng result is that cyclic block descent converges on a smooth plus
#' separable non-smooth function; separability is what saves it. For the
#' convex quadratic each block subproblem has a closed form, so the
#' sweep is exact and the objective decreases monotonically.
#'
#' Determinism: fixed sweep count, fixed block order, no tolerance test.
#' The callable-objective form is not representable across both arms, so
#' the quadratic case is what is provided.
#'
#' Formula: minimise \code{f(x) = 0.5 x'Qx - b'x}; exact block minimiser
#' \code{x_B = Q_BB^{-1} (b_B - Q_{B,Bc} x_{Bc})}.
#'
#' @param Q Symmetric positive definite Hessian.
#' @param b Linear term.
#' @param blocks List of zero-based coordinate index vectors.
#' @param x0 Starting point; zeros by default.
#' @param n_iter Number of full sweeps.
#' @return List with \code{estimate}, \code{x}, \code{obj_trace}, \code{n_iter}.
#' @references Tseng, P. (2001). Convergence of a block coordinate
#'   descent method for nondifferentiable minimization. JOTA 109:475-494.
#' @export
#' @examples
#' Bcdblk(Q = 0.5, b = 5L, blocks = c("a", "b", "c"))
Bcdblk <- function(Q, b, blocks, x0 = NULL, n_iter = 20) {
  Qm <- as.matrix(Q)
  bv <- as.numeric(b)
  p <- length(bv)
  x <- if (is.null(x0)) rep(0, p) else as.numeric(x0)
  obj <- function(v) 0.5 * as.numeric(t(v) %*% Qm %*% v) - sum(bv * v)
  trace <- obj(x)
  for (it in seq_len(as.integer(n_iter))) {
    for (blk in blocks) {
      idx <- as.integer(blk) + 1L
      rest <- setdiff(seq_len(p), idx)
      Qbb <- Qm[idx, idx, drop = FALSE]
      rhs <- bv[idx] - if (length(rest)) as.numeric(Qm[idx, rest, drop = FALSE] %*% x[rest]) else 0
      x[idx] <- as.numeric(solve(Qbb, rhs))
    }
    trace <- c(trace, obj(x))
  }
  .t1_result(estimate = trace[length(trace)], x = x, obj_trace = trace,
             n_iter = as.integer(n_iter),
             method = "Block coordinate descent, exact quadratic blocks")
}
