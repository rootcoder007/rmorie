# SPDX-License-Identifier: AGPL-3.0-or-later
#' Roughness penalty (integrated squared second derivative)
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 579-631\], Chapter 14, Section 14.4, equations (14.10) and
#' (14.11), p. 601, read as a rendered page.  The penalised sum of squares is
#' SSE_lambda(beta) = sum_i (y_i - mu - sum_l x_il beta_l)^2 + lambda J_beta
#' (14.10), and "often the penalty term J_beta is based on the integrated pth
#' order derivatives", J_beta = int_0^T \[d^p/dt^p beta(t)\]^2 dt (14.11),
#' which "can be expressed as J_beta = beta' P beta, where P is a square
#' matrix with entries P_ij = int_0^T phi_i^(p)(t) phi_j^(p)(t)".  "Typical
#' chosen values of p are 1 and 2."
#'
#' Derivatives are taken by central differences inside the grid and by
#' one-sided differences at its two ends, so the trapezoid rule integrates
#' over the whole of \[a, b\] rather than dropping the two end intervals.  Both
#' stencils are exact at the polynomial degree the order needs, so f(t) = t^2
#' integrates to exactly 4 and an affine function to exactly 0, as (14.11)
#' requires.
#'
#' @param basis a vector of function values on an equally spaced grid, or an
#'   m-by-L matrix whose columns are basis functions on that grid.
#' @param lam the smoothing parameter lambda of equation (14.10).
#' @param a,b end points of the grid; the unit interval by default.
#' @param p derivative order of equation (14.11); 1 or 2.
#' @return list: estimate, penalty, J, P, n, method.
#' @keywords internal
#' @examples
#' Rpnlt(seq(0, 1, length.out = 101)^2, 1)$J
#' @export
Rpnlt <- function(basis, lam, a = 0, b = 1, p = 2) {
  B <- .s03mat(basis)
  m <- nrow(B)
  if (m < 4L) stop("roughness_penalty: need at least four grid points")
  L <- ncol(B)
  lam <- as.numeric(lam)
  if (lam < 0) stop("roughness_penalty: lambda must be non-negative")
  pp <- as.integer(p)
  if (!(pp %in% c(1L, 2L))) stop("roughness_penalty: p must be 1 or 2")
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (!(b > a)) stop("roughness_penalty: the grid must have positive width")
  h <- (b - a) / (m - 1L)
  D <- matrix(0, m, L)
  for (i in seq_len(m)) {
    for (j in seq_len(L)) {
      D[i, j] <- if (pp == 1L) {
        if (i == 1L) (-3 * B[1L, j] + 4 * B[2L, j] - B[3L, j]) / (2 * h)
        else if (i == m) (3 * B[m, j] - 4 * B[m - 1L, j] + B[m - 2L, j]) / (2 * h)
        else (B[i + 1L, j] - B[i - 1L, j]) / (2 * h)
      } else {
        if (i == 1L) (2 * B[1L, j] - 5 * B[2L, j] + 4 * B[3L, j] - B[4L, j]) / (h * h)
        else if (i == m) {
          (2 * B[m, j] - 5 * B[m - 1L, j] + 4 * B[m - 2L, j] - B[m - 3L, j]) / (h * h)
        } else (B[i + 1L, j] - 2 * B[i, j] + B[i - 1L, j]) / (h * h)
      }
    }
  }
  P <- matrix(0, L, L)
  for (i in seq_len(L)) {
    for (j in seq_len(L)) {
      s <- 0
      for (r in seq_len(m)) {
        wgt <- if (r == 1L || r == m) 0.5 else 1
        s <- s + wgt * D[r, i] * D[r, j]
      }
      P[i, j] <- s * h
    }
  }
  J <- 0
  for (i in seq_len(L)) for (j in seq_len(L)) J <- J + P[i, j]
  list(estimate = lam * J, penalty = lam * J, J = J, P = P, n = m,
       method = "J = integral (D^p f)^2 dt = c'Pc, Chapter 14 eqs. (14.10)-(14.11)")
}
