.gcp_tol <- 1e-9

.gcp_pivot <- function(st, r, cc) {
  T <- st$T
  T[r, ] <- T[r, ] / T[r, cc]
  for (i in seq_len(nrow(T))) {
    if (i != r && T[i, cc] != 0) T[i, ] <- T[i, ] - T[i, cc] * T[r, ]
  }
  st$T <- T
  st$basis[r] <- cc
  st
}

.gcp_primal <- function(st, max_iter) {
  for (it in seq_len(max_iter)) {
    T <- st$T
    m <- nrow(T) - 1
    ncol <- ncol(T) - 1
    cc <- which(T[m + 1, seq_len(ncol)] < -.gcp_tol)
    if (!length(cc)) return(list(st = st, status = "optimal"))
    cc <- cc[1]
    rows <- which(T[seq_len(m), cc] > .gcp_tol)
    if (!length(rows)) return(list(st = st, status = "unbounded"))
    ratio <- T[rows, ncol + 1] / T[rows, cc]
    r <- rows[order(ratio, st$basis[rows])[1]]
    st <- .gcp_pivot(st, r, cc)
  }
  list(st = st, status = "iteration_limit")
}

.gcp_dual <- function(st, max_iter) {
  for (it in seq_len(max_iter)) {
    T <- st$T
    m <- nrow(T) - 1
    ncol <- ncol(T) - 1
    r <- order(T[seq_len(m), ncol + 1], st$basis)[1]
    if (T[r, ncol + 1] >= -.gcp_tol) return(list(st = st, status = "optimal"))
    cols <- which(T[r, seq_len(ncol)] < -.gcp_tol)
    if (!length(cols)) return(list(st = st, status = "infeasible"))
    cc <- cols[order(T[m + 1, cols] / -T[r, cols], cols)[1]]
    st <- .gcp_pivot(st, r, cc)
  }
  list(st = st, status = "iteration_limit")
}

#' Gomory cutting-plane method for integer linear programs
#'
#' R arm of \code{morie.fn.cuttip}: maximise \eqn{c'x} subject to
#' \eqn{Ax \le b}, \eqn{x \ge 0}, the variables in \code{integer_indices}
#' (0-based, all by default) integer. The relaxation is solved by the primal
#' simplex method from the slack basis (\eqn{b \ge 0} required); while an
#' integer variable is fractional, the Gomory mixed-integer cut of the most
#' fractional row is appended and the dual simplex method re-optimises.
#' Slacks count as integer when every variable is integer and the data are
#' integer. Decisions use a 1e-9 tolerance, as the Python arm.
#'
#' @param c Objective coefficients (maximised).
#' @param A Constraint matrix.
#' @param b Right-hand sides, non-negative.
#' @param integer_indices 0-based integer variables (default all).
#' @param max_cuts Cut limit.
#' @param max_iter Pivot limit per simplex call.
#' @return List with \code{x}, \code{objective}, \code{lp_bound},
#'   \code{n_cuts}, \code{status}.
#' @references Gomory, R. E. (1958). Outline of an algorithm for integer
#'   solutions to linear programs. Bulletin of the American Mathematical
#'   Society 64, 275-278.
#'
#'   Gomory, R. E. (1960). An algorithm for the mixed integer problem. RAND
#'   report RM-2597.
#'
#'   Nemhauser, G. L. and Wolsey, L. A. (1988). Integer and Combinatorial
#'   Optimization. Wiley.
#' @examples
#' Cuttip(c(1, 1), rbind(c(-2, 2), c(8, 10)), c(1, 13))$x
#' @export
Cuttip <- function(c, A, b, integer_indices = NULL, max_cuts = 200, max_iter = 5000) {
  A <- as.matrix(A) + 0
  m <- nrow(A)
  n <- ncol(A)
  if (length(b) != m || length(c) != n) stop("A must be m x n with length(b) = m and length(c) = n")
  if (min(b) < 0) stop("b >= 0 is required for the slack starting basis")
  ints <- if (is.null(integer_indices)) seq_len(n) else as.integer(integer_indices) + 1L
  if (length(ints) == n && all(A == round(A)) && all(b == round(b))) ints <- c(ints, n + seq_len(m))
  st <- list(T = rbind(cbind(A, diag(m), b), c(-c, rep(0, m), 0)), basis = n + seq_len(m))
  fr <- function(v) v - floor(v)
  res <- .gcp_primal(st, max_iter)
  if (res$status != "optimal") return(list(x = NULL, objective = NULL, lp_bound = NULL, n_cuts = 0, status = res$status))
  st <- res$st
  lp_bound <- st$T[nrow(st$T), ncol(st$T)]
  cuts <- 0
  status <- "optimal"
  repeat {
    T <- st$T
    mm <- length(st$basis)
    rhs <- T[seq_len(mm), ncol(T)]
    f <- fr(rhs)
    cand <- which(st$basis %in% ints & f > .gcp_tol & f < 1 - .gcp_tol)
    if (!length(cand)) {
      status <- "optimal"
      break
    }
    if (cuts >= max_cuts) {
      status <- "cut_limit"
      break
    }
    key <- pmin(f[cand], 1 - f[cand])
    r <- cand[order(-key, cand)[1]]
    f0 <- f[r]
    ncol <- ncol(T) - 1
    g <- vapply(seq_len(ncol), function(j) {
      if (j %in% st$basis) return(0)
      a <- T[r, j]
      if (j %in% ints) {
        fj <- fr(a)
        if (fj <= f0) fj / f0 else (1 - fj) / (1 - f0)
      } else {
        if (a > 0) a / f0 else -a / (1 - f0)
      }
    }, 0)
    T <- cbind(T[, seq_len(ncol), drop = FALSE], 0, T[, ncol + 1])
    T <- rbind(T[seq_len(mm), , drop = FALSE], c(-g, 1, -1), T[mm + 1, ])
    st <- list(T = T, basis = c(st$basis, ncol + 1))
    cuts <- cuts + 1
    res <- .gcp_dual(st, max_iter)
    st <- res$st
    if (res$status != "optimal") {
      status <- res$status
      break
    }
  }
  x <- numeric(n)
  T <- st$T
  for (i in seq_along(st$basis)) if (st$basis[i] <= n) x[st$basis[i]] <- T[i, ncol(T)]
  ok <- status %in% c("optimal", "cut_limit")
  list(x = if (ok) x else NULL, objective = if (ok) sum(c * x) else NULL, lp_bound = lp_bound, n_cuts = cuts,
       status = status, estimate = if (ok) sum(c * x) else NULL)
}
