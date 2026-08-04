# SPDX-License-Identifier: AGPL-3.0-or-later

#' Identified bounds on beta when X is discrete
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.3.2, equation (2.13) (pages 15-16).  When
#' every component of X is discrete, beta is not point identified, but
#' if G is strictly increasing the support points sort so that
#' G(x_1'b) <= ... <= G(x_M'b), and tight identified bounds follow from
#' the linear programs
#'
#'   maximize (minimize) b_m subject to x_j'b <= x_(j+1)'b,
#'   j = 1, ..., M-1
#'
#' with equality imposed where G(x_j'b) equals G(x_(j+1)'b), and the
#' scale normalisation b_1 = 1.
#'
#' Solved with the package's own two-phase simplex under Bland's rule,
#' which is deterministic.  Free coefficients are shifted by the
#' explicit finite \code{blim} so the solver sees only nonnegative
#' variables.
#'
#' @param xs Numeric matrix, M by d, of the points of support of X.
#' @param gvals Numeric vector of E(Y | X = x_m) at each support point.
#' @param blim Numeric; the explicit finite box abs(b_j) <= blim.  A
#'   solution at the box edge is reported as unbounded.
#' @param tie Numeric; two \code{gvals} closer than this count as equal
#'   and turn the corresponding inequality into an equality.
#' @return Named list with lower, upper, width, bounded, dim, M, method.
#' @keywords internal
#' @examples
#' # Horowitz (2009) Example 2.5, Table 2.2 (page 16): bounds 1 < b2 < 1.2
#' xs <- rbind(c(0, 0), c(1, 0), c(0, 1), c(0.6, 0.5), c(1, 1))
#' Simidentd(xs, c(0, 0.1, 0.3, 0.35, 0.4))$lower
#' @export
Simidentd <- function(xs, gvals, blim = 100, tie = 1e-12) {
  Xs <- if (is.null(dim(xs))) matrix(xs, nrow = 1L) else as.matrix(xs)
  g <- as.numeric(gvals)
  M <- nrow(Xs)
  d <- ncol(Xs)
  k <- d - 1L
  if (length(g) != M || M < 2L || d < 2L) {
    return(list(lower = rep(NA_real_, max(k, 0L)),
                upper = rep(NA_real_, max(k, 0L)),
                width = rep(NA_real_, max(k, 0L)),
                bounded = rep(FALSE, max(k, 0L)),
                dim = as.integer(d), M = as.integer(M),
                method = "identified bounds (2.13) -- input too small"))
  }
  o <- order(g)
  Xs <- Xs[o, , drop = FALSE]
  g <- g[o]

  rows_ub <- matrix(0, 0L, k)
  rhs_ub <- numeric(0)
  rows_eq <- matrix(0, 0L, k)
  rhs_eq <- numeric(0)
  for (j in seq_len(M - 1L)) {
    a <- Xs[j, -1L] - Xs[j + 1L, -1L]
    c0 <- Xs[j, 1L] - Xs[j + 1L, 1L]
    rhs <- blim * sum(a) - c0
    if (abs(g[j + 1L] - g[j]) <= tie) {
      rows_eq <- rbind(rows_eq, a)
      rhs_eq <- c(rhs_eq, rhs)
    } else {
      rows_ub <- rbind(rows_ub, a)
      rhs_ub <- c(rhs_ub, rhs)
    }
  }
  A_ub <- if (nrow(rows_ub)) rows_ub else NULL
  b_ub <- if (nrow(rows_ub)) rhs_ub else NULL
  A_eq <- if (nrow(rows_eq)) rows_eq else NULL
  b_eq <- if (nrow(rows_eq)) rhs_eq else NULL
  lower <- rep(0, k)
  upper <- rep(2 * blim, k)

  lo <- rep(NA_real_, k)
  hi <- rep(NA_real_, k)
  bounded <- logical(k)
  for (m in seq_len(k)) {
    cv <- numeric(k)
    cv[m] <- 1
    r1 <- .bnd_simplex(cv, A_ub, b_ub, A_eq, b_eq, lower, upper)
    r2 <- .bnd_simplex(-cv, A_ub, b_ub, A_eq, b_eq, lower, upper)
    v1 <- if (identical(r1$status, 0L)) r1$x[m] - blim else NA_real_
    v2 <- if (identical(r2$status, 0L)) r2$x[m] - blim else NA_real_
    lo[m] <- v1
    hi[m] <- v2
    bounded[m] <- is.finite(v1) && is.finite(v2) &&
      v1 > -blim + 1e-6 && v2 < blim - 1e-6
  }
  list(lower = lo, upper = hi, width = hi - lo, bounded = bounded,
       dim = as.integer(d), M = as.integer(M),
       method = "Horowitz (2009) eq. (2.13) linear programs")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Simidentd
#' @keywords internal
#' @export
morie_horowitz_sim_id_discrete_x <- Simidentd
