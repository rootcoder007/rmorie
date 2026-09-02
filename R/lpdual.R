# SPDX-License-Identifier: AGPL-3.0-or-later
#' Form the dual of a linear program and certify a pair of solutions
#'
#' No linear program is SOLVED here. Duality is a certificate: a feasible
#' primal and dual with equal objectives are optimal, and \code{gap} is
#' otherwise the exact distance from optimality. Complementary slackness
#' is reported per constraint and per variable.
#'
#' Formula: primal max c'x s.t. A x <= b, x >= 0;
#'   dual min b'y s.t. A'y >= c, y >= 0; weak duality c'x <= b'y
#'
#' @param A Constraint matrix.
#' @param b Right-hand side.
#' @param c Objective coefficients.
#' @param x Candidate primal solution.
#' @param y Candidate dual solution.
#' @return List with \code{dual_A}, \code{dual_b}, \code{dual_c},
#'   \code{primal_objective}, \code{dual_objective}, \code{gap},
#'   \code{primal_feasible}, \code{dual_feasible}, \code{optimal},
#'   \code{slack}, \code{surplus}, \code{cs_constraint},
#'   \code{cs_variable}, \code{m}, \code{n}.
#' @references von Neumann, J. (1947), Discussion of a maximum problem,
#'   Institute for Advanced Study, reprinted in Collected Works VI, 89-95
#'   -- the first statement of linear programming duality. Gale, Kuhn &
#'   Tucker (1951), Linear programming and the theory of games, 317-329,
#'   for the first published proof.
#' @export
Lpdual <- function(A, b, c, x = NULL, y = NULL) {
  A <- as.matrix(A); m <- nrow(A); n <- ncol(A)
  if (m < 1L || n < 1L) stop("the constraint matrix must be non-empty")
  b <- .t1_vec(b); c <- .t1_vec(c)
  if (length(b) != m) stop("b must have one entry per constraint")
  if (length(c) != n) stop("c must have one entry per variable")
  x <- if (is.null(x)) rep(0, n) else .t1_vec(x)
  y <- if (is.null(y)) rep(0, m) else .t1_vec(y)
  if (length(x) != n) stop("x must have one entry per variable")
  if (length(y) != m) stop("y must have one entry per constraint")
  slack <- b - as.numeric(A %*% x)
  surp <- as.numeric(t(A) %*% y) - c
  pf <- as.numeric(all(slack >= -1e-9) && all(x >= -1e-9))
  df <- as.numeric(all(surp >= -1e-9) && all(y >= -1e-9))
  po <- sum(c * x); do_ <- sum(b * y); gap <- do_ - po
  .t1_result(dual_A = t(A), dual_b = c, dual_c = b,
             primal_objective = po, dual_objective = do_, gap = gap,
             primal_feasible = pf, dual_feasible = df,
             optimal = as.numeric(pf && df && abs(gap) <= 1e-9),
             slack = slack, surplus = surp,
             cs_constraint = y * slack, cs_variable = x * surp,
             m = as.numeric(m), n = as.numeric(n),
             method = "LP duality certificate (no LP is solved)")
}
