# SPDX-License-Identifier: AGPL-3.0-or-later
#' Linear programme by the primal simplex method
#'
#' The GLPK problem form, minimise c'x subject to Ax <= b and x >= 0,
#' solved by the textbook primal simplex on the slack tableau with
#' Bland's rule so the iteration cannot cycle.  Only origin-feasible
#' problems are accepted; a negative right-hand side needs a phase-one
#' problem and is refused rather than silently mishandled.  The final
#' objective row carries the simplex multipliers (negated, since the row
#' is updated by subtraction), so strong duality is available as a check.
#'
#' Formula: pivot on the slack tableau until every reduced cost is
#'   non-negative; x* from the basis, y* from the objective row.
#'
#' @param c Objective coefficients.
#' @param A Constraint matrix.
#' @param b Right-hand side, non-negative.
#' @param max_iter Iteration cap.
#' @return List with \code{estimate}, \code{x}, \code{objective},
#'   \code{dual}, \code{dual_objective}, \code{iterations},
#'   \code{status}, \code{n}, \code{method}.
#' @references Makhorin, GNU Linear Programming Kit reference manual;
#'   Bland (1977), Mathematics of Operations Research 2(2):103-107.
#'   \doi{10.1287/moor.2.2.103}
#' @export
#' @examples
#' Glpopt(c = c(-1, -2), A = matrix(c(1, 1, 1, 0, 0, 1), 3, 2, byrow = TRUE),
#'        b = c(4, 2, 3))
Glpopt <- function(c, A, b, max_iter = 200) {
  cv <- .s03vec(c); M <- .s03mat(A); bv <- .s03vec(b)
  m <- nrow(M); n <- length(cv)
  if (m == 0L) stop("glpk_lp: A has no rows")
  if (n == 0L) stop("glpk_lp: c is empty")
  if (length(bv) != m) stop("glpk_lp: A and b have different row counts")
  if (ncol(M) != n) stop("glpk_lp: A and c have different column counts")
  if (any(bv < 0)) stop("glpk_lp: needs b >= 0 (origin-feasible); phase one is not implemented")
  eps <- 1e-12
  T <- cbind(M, diag(1, m), bv)
  z <- c(cv, rep(0, m), 0)
  basis <- n + seq_len(m)
  it <- 0L; status <- "optimal"
  while (it < as.integer(max_iter)) {
    enter <- 0L
    for (j in seq_len(n + m)) if (z[j] < -eps) { enter <- j; break }
    if (enter == 0L) break
    leave <- 0L; best <- NA_real_
    for (i in seq_len(m)) if (T[i, enter] > eps) {
      ratio <- T[i, n + m + 1] / T[i, enter]
      if (is.na(best) || ratio < best - eps || (abs(ratio - best) <= eps && basis[i] < basis[leave])) {
        best <- ratio; leave <- i
      }
    }
    if (leave == 0L) { status <- "unbounded"; break }
    T[leave, ] <- T[leave, ] / T[leave, enter]
    for (i in seq_len(m)) if (i != leave && T[i, enter] != 0)
      T[i, ] <- T[i, ] - T[i, enter] * T[leave, ]
    z <- z - z[enter] * T[leave, ]
    basis[leave] <- enter
    it <- it + 1L
  }
  x <- rep(0, n)
  for (i in seq_len(m)) if (basis[i] <= n) x[basis[i]] <- T[i, n + m + 1]
  y <- -z[n + seq_len(m)]
  .t1_result(estimate = sum(cv * x), x = x, objective = sum(cv * x),
             dual = y, dual_objective = sum(bv * y), iterations = it,
             status = status, n = n,
             method = "primal simplex on the slack tableau with Bland's rule")
}
