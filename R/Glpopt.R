# SPDX-License-Identifier: AGPL-3.0-or-later
# One run of Bland's-rule primal simplex on tableau T (last column the
# right-hand side) with reduced-cost row z, basis `basis`; only columns in
# `allowed` may enter.  Returns the updated pieces and a status.
.glp_simplex <- function(T, z, basis, allowed, max_iter, it) {
  eps <- 1e-12
  m <- nrow(T)
  rhs <- ncol(T)
  status <- "optimal"
  repeat {
    enter <- 0L
    for (j in allowed) if (z[j] < -eps) {
      enter <- j
      break
    }
    if (enter == 0L) break
    if (it >= max_iter) {
      status <- "iteration_limit"
      break
    }
    leave <- 0L
    best <- NA_real_
    for (i in seq_len(m)) if (T[i, enter] > eps) {
      ratio <- T[i, rhs] / T[i, enter]
      if (is.na(best) || ratio < best - eps ||
            (abs(ratio - best) <= eps && basis[i] < basis[leave])) {
        best <- ratio
        leave <- i
      }
    }
    if (leave == 0L) {
      status <- "unbounded"
      break
    }
    T[leave, ] <- T[leave, ] / T[leave, enter]
    for (i in seq_len(m)) if (i != leave && T[i, enter] != 0)
      T[i, ] <- T[i, ] - T[i, enter] * T[leave, ]
    z <- z - z[enter] * T[leave, ]
    basis[leave] <- enter
    it <- it + 1L
  }
  list(T = T, z = z, basis = basis, status = status, it = it)
}

#' Linear programme by the two-phase primal simplex method
#'
#' The GLPK problem form, minimise c'x subject to Ax <= b and x >= 0,
#' solved by the textbook primal simplex on the slack tableau with
#' Bland's rule so the iteration cannot cycle.  A row with a negative
#' right-hand side is multiplied by -1 and given an artificial variable;
#' phase one minimises the sum of the artificials, and a positive
#' minimum means no x satisfies the constraints (status "infeasible").
#' Artificials left in the basis at zero are pivoted out where the row
#' allows, then phase two minimises c'x from that feasible basis.  When
#' every b is non-negative the origin is feasible and phase one is
#' skipped.  The final objective row carries the simplex multipliers
#' (negated, since the row is updated by subtraction): y = -z on the
#' slack columns, the same for flipped rows because their slack column
#' is flipped too, so strong duality c'x = b'y is available as a check.
#'
#' Formula: phase one min 1'a over [sA | sI | I_flip][x; s; a] = s b;
#'   phase two pivots until every reduced cost is non-negative; x* from
#'   the basis, y* from the objective row.
#'
#' @param c Objective coefficients.
#' @param A Constraint matrix.
#' @param b Right-hand side.
#' @param max_iter Iteration cap over both phases.
#' @return List with \code{estimate}, \code{x}, \code{objective},
#'   \code{dual}, \code{dual_objective}, \code{iterations},
#'   \code{status} ("optimal", "unbounded", "infeasible" or
#'   "iteration_limit"), \code{n}, \code{method}.  \code{x} and the
#'   objective are \code{NA} unless the status is "optimal".
#' @references Makhorin, GNU Linear Programming Kit reference manual;
#'   Bland (1977), Mathematics of Operations Research 2(2):103-107.
#'   \doi{10.1287/moor.2.2.103}; Dantzig, Orden and Wolfe (1955), Pacific
#'   Journal of Mathematics 5(2):183-195 (the two-phase method).
#' @export
#' @examples
#' Glpopt(c = c(-1, -2), A = matrix(c(1, 1, 1, 0, 0, 1), 3, 2, byrow = TRUE),
#'        b = c(4, 2, 3))
#' # x1 + x2 >= 2 written as -x1 - x2 <= -2 needs phase one
#' Glpopt(c = c(1, 3), A = rbind(c(-1, -1), c(1, 0)), b = c(-2, 5))$x
Glpopt <- function(c, A, b, max_iter = 200) {
  cv <- .s03vec(c)
  M <- .s03mat(A)
  bv <- .s03vec(b)
  m <- nrow(M)
  n <- length(cv)
  if (m == 0L) stop("glpk_lp: A has no rows")
  if (n == 0L) stop("glpk_lp: c is empty")
  if (length(bv) != m) stop("glpk_lp: A and b have different row counts")
  if (ncol(M) != n) stop("glpk_lp: A and c have different column counts")
  max_iter <- as.integer(max_iter)
  sg <- ifelse(bv < 0, -1, 1)
  flip <- which(sg < 0)
  k <- length(flip)
  art <- n + m + seq_len(k)
  Art <- matrix(0, m, k)
  Art[cbind(flip, seq_len(k))] <- 1
  T <- cbind(sg * M, diag(sg, m), Art, sg * bv)
  basis <- n + seq_len(m)
  basis[flip] <- art
  it <- 0L
  status <- "optimal"
  if (k > 0L) {
    w <- c(rep(0, n + m), rep(1, k), 0)
    for (i in flip) w <- w - T[i, ]
    w[art] <- 0
    p1 <- .glp_simplex(T, w, basis, seq_len(n + m), max_iter, it)
    T <- p1$T
    basis <- p1$basis
    it <- p1$it
    if (p1$status == "iteration_limit") status <- "iteration_limit"
    else if (-p1$z[ncol(T)] > 1e-9) status <- "infeasible"
    # pivot zero-level artificials out of the basis where the row allows
    for (i in which(basis %in% art)) {
      j <- which(abs(T[i, seq_len(n + m)]) > 1e-12)[1L]
      if (is.na(j)) next
      T[i, ] <- T[i, ] / T[i, j]
      for (r in seq_len(m)) if (r != i && T[r, j] != 0)
        T[r, ] <- T[r, ] - T[r, j] * T[i, ]
      basis[i] <- j
    }
  }
  if (status == "optimal") {
    cost <- c(cv, rep(0, m + k), 0)
    z <- cost
    for (i in seq_len(m)) z <- z - cost[basis[i]] * T[i, ]
    p2 <- .glp_simplex(T, z, basis, seq_len(n + m), max_iter, it)
    T <- p2$T
    basis <- p2$basis
    it <- p2$it
    status <- p2$status
    z <- p2$z
  }
  if (status != "optimal") {
    return(.t1_result(estimate = NA_real_, x = rep(NA_real_, n),
                      objective = NA_real_, dual = rep(NA_real_, m),
                      dual_objective = NA_real_, iterations = it,
                      status = status, n = n,
                      method = "two-phase primal simplex with Bland's rule"))
  }
  x <- rep(0, n)
  for (i in seq_len(m)) if (basis[i] <= n) x[basis[i]] <- T[i, ncol(T)]
  y <- -z[n + seq_len(m)]
  .t1_result(estimate = sum(cv * x), x = x, objective = sum(cv * x),
             dual = y, dual_objective = sum(bv * y), iterations = it,
             status = status, n = n,
             method = "two-phase primal simplex with Bland's rule")
}
