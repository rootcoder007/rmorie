# SPDX-License-Identifier: AGPL-3.0-or-later

.hrztf_big <- 1.0e12

# T_n evaluated at an observation.  Page 219: T_n(y) "is replaced with
# an arbitrarily large negative number if y < y2 and an arbitrarily
# large positive number if y > y1".  Inside [y2, y1] the grid values
# are interpolated.
#' T_n evaluated at an observation.  Page 219: T_n(y) "is replaced with
#'
#' an arbitrarily large negative number if y < y2 and an arbitrarily
#' large positive number if y > y1".  Inside [y2, y1] the grid values
#' are interpolated.
#'
#' @param yv Numeric; combined arithmetically in the body.
#' @param ygrid A vector; its length is taken and its elements indexed.
#' @param T A vector; indexed elementwise.
#' @param y2 Passed to \code{<}.
#' @param y1 Passed to \code{>}.
#' @return The value of \code{[}.
#' @export
.hrztf_tn_at <- function(yv, ygrid, T, y2, y1) {
  if (yv < y2) {
    return(-.hrztf_big)
  }
  if (yv > y1) {
    return(.hrztf_big)
  }
  ny <- length(ygrid)
  if (yv <= ygrid[1L]) {
    return(T[1L])
  }
  for (k in 2:ny) {
    if (yv <= ygrid[k]) {
      lo <- ygrid[k - 1L]
      hi <- ygrid[k]
      if (hi <= lo) {
        return(T[k])
      }
      f <- (yv - lo) / (hi - lo)
      return(T[k - 1L] + f * (T[k] - T[k - 1L]))
    }
  }
  T[ny]
}

#' Transformation model with BOTH T and F nonparametric
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 6.3, pages 215-219.  Adds the estimator of F
#' to the estimator of T implemented in [Hrztmod()].  Because U is
#' independent of X, P(U <= u | a < Z <= b) = F(u) for any a, b in the
#' support of Z, so for any y2 < y1
#'
#'   F(u) = P[U <= u | T(y2) - u < Z <= T(y1) - u] = A(u)/B(u)   (6.63)
#'   A(u) = E{I(U <= u) I[T(y2) - u < Z <= T(y1) - u]}           (6.64)
#'   B(u) = E{I[T(y2) - u < Z <= T(y1) - u]}                     (6.65)
#'
#' and the estimator replaces these by sample analogues (6.66) with
#' U_ni = T_n(Y_i) - Z_ni.
#'
#' The conditioning is not a technical nicety and the obvious shortcut
#' is wrong.  Page 219: "It may seem that F can be estimated more
#' simply by the empirical distribution function of
#' U_n = T_n(Y) - X'b_n, but this is not the case."  T is
#' n^(-1/2)-consistent only on a compact [y2, y1] strictly inside the
#' support of Y, so U is effectively observed under CENSORING, and the
#' empirical CDF is inconsistent under censoring.  (6.66) is consistent
#' despite it.
#'
#' Since the indicator set of A_n is a subset of that of B_n,
#' A_n <= B_n pointwise and Fhat lies in [0, 1] identically; both are
#' asserted rather than assumed.
#'
#' @param x Numeric vector or n by d matrix of covariates.
#' @param y Numeric vector, the response.
#' @param ny,nz Integer grid sizes passed to the estimator of T.
#' @param nu Integer, grid points for F.
#' @param bandwidth Numeric or NULL, passed to the estimator of T.
#' @return Named list with T_hat, F_hat, beta_hat, u_grid, A_n, B_n,
#'   y_grid, y0, index, F_in_unit, A_le_B, n, method.
#' @keywords internal
#' @examples
#' n <- 40
#' x <- cbind(sin(1.3 * seq_len(n)), cos(0.6 * seq_len(n)))
#' y <- exp(x[, 1] + 0.5 * x[, 2])
#' Hrztf(x, y, ny = 7L, nz = 7L, nu = 7L)$A_le_B
#' @export
Hrztf <- function(x, y, ny = 21L, nz = 21L, nu = 25L, bandwidth = NULL) {
  base <- Hrztmod(x, y, ny = ny, nz = nz, bandwidth = bandwidth)
  T <- base$T_hat
  ygrid <- base$y_grid
  Z <- base$index
  y2 <- base$y2
  y1 <- base$y1
  n <- base$n
  nu <- as.integer(nu)
  if (nu < 3L) stop(sprintf("nu must be at least 3, got %d.", nu))

  Ty2 <- T[1L]
  Ty1 <- T[length(T)]
  yl <- as.numeric(y)
  U <- vapply(seq_len(n),
              function(i) .hrztf_tn_at(yl[i], ygrid, T, y2, y1) - Z[i], 0)

  fin <- U[abs(U) < .hrztf_big / 2]
  if (length(fin) < 2L) {
    stop(paste("no observation of Y falls inside [y2, y1], so F is not",
               "estimable; widen the interval."))
  }
  ulo <- min(fin)
  uhi <- max(fin)
  if (uhi <= ulo) stop("the residuals U have no spread.")
  du <- (uhi - ulo) / (nu - 1L)
  ugrid <- ulo + (seq_len(nu) - 1L) * du
  # Pin the endpoints exactly.  The accumulated value ulo + (nu-1)*du
  # need not round to uhi, and F is an INDICATOR count: a last grid
  # point a single ulp below max(U) excludes that observation and drops
  # F(u_max) from 1 to 1 - 1/n.
  ugrid[1L] <- ulo
  ugrid[nu] <- uhi

  A <- numeric(nu)
  B <- numeric(nu)
  Fh <- numeric(nu)
  for (k in seq_len(nu)) {
    u <- ugrid[k]
    lo <- Ty2 - u
    hi <- Ty1 - u
    inb <- as.numeric(Z > lo & Z <= hi)
    b <- sum(inb)
    a <- sum(inb * as.numeric(U <= u))
    A[k] <- a / n
    B[k] <- b / n
    Fh[k] <- if (b > 0) a / b else NA_real_
  }

  a_le_b <- all(A <= B + 1e-12)
  ok <- is.finite(Fh)
  f_unit <- all(Fh[ok] >= -1e-12 & Fh[ok] <= 1 + 1e-12)

  list(T_hat = T, F_hat = Fh, beta_hat = base$beta_hat, u_grid = ugrid,
       A_n = A, B_n = B, y_grid = ygrid, y0 = base$y0, index = Z,
       F_in_unit = f_unit, A_le_B = a_le_b, n = n,
       method = paste("Horowitz (2009) eqs. (6.60) and (6.66),",
                      "T and F both nonparametric"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrztf
#' @keywords internal
#' @export
morie_horowitz_both_nonpar_transform <- Hrztf
