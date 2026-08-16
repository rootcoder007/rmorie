# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Chebyshev-Hermite polynomials and disjunctive kriging.
# Schabenberger & Gotway (2005), Sec. 5.6.4.
#
# Basis: eq (5.64), built from the three-term recurrence the text gives,
#   H_0 = 1, H_1 = x, H_{p+1}(x) = x H_p(x) - p H_{p-1}(x),
# rather than by differentiating. These are orthogonal but NOT orthonormal
# (the integral is p!), so eta_p(x) = H_p(x)/sqrt(p!) is the basis used.
#
# Matheron's result makes the scheme work: for bivariate Gaussian Z with
# correlation rho(h) and p >= 1, Cov[eta_p(Z(s+h)), eta_p(Z(s))] = rho(h)^p,
# so each component is kriged with rho raised to its own power. eta_p has
# mean 0 and variance 1, so (5.68) is a simple-kriging system with no
# unbiasedness constraint: lambda R = rho, R = [rho_ij^p], rho = [rho_0i^p],
# variance (5.69) 1 - lambda'rho, predictor (5.70), variance (5.71).
#
# Internal; `aaa_` collates it before its callers.

#' .schab_hermite_e
#'
#' Part of the schab_hermite_shared implementation; see the file header
#' for the source it follows.
#'
#' @param x See Usage.
#' @param degree See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_hermite_e <- function(x, degree) {
  x <- as.numeric(x)
  degree <- as.integer(degree)
  if (degree < 0L) stop("`degree` must be non-negative", call. = FALSE)
  out <- matrix(0, degree + 1L, length(x))
  out[1, ] <- 1
  if (degree >= 1L) out[2, ] <- x
  if (degree >= 2L) {
    for (p in 1:(degree - 1L)) {
      out[p + 2L, ] <- x * out[p + 1L, ] - p * out[p, ]
    }
  }
  out
}

#' .schab_factorial
#'
#' Part of the schab_hermite_shared implementation; see the file header
#' for the source it follows.
#'
#' @param p See Usage.
#' @return The value of \code{prod}.
#' @export
.schab_factorial <- function(p) {
  if (p < 2) {
    return(1)
  }
  prod(2:p)
}

#' .schab_hermite_orthonormal
#'
#' Part of the schab_hermite_shared implementation; see the file header
#' for the source it follows.
#'
#' @param x See Usage.
#' @param degree See Usage.
#' @return A numeric value.
#' @export
.schab_hermite_orthonormal <- function(x, degree) {
  h <- .schab_hermite_e(x, degree)
  scale <- sqrt(vapply(0:degree, .schab_factorial, numeric(1)))
  h / scale
}

#' Golub-Welsch: nodes are the eigenvalues of the symmetric tridiagonal
#'
#' Jacobi matrix (zero diagonal, sqrt(k) off-diagonal for the
#' probabilists\' Hermite system); weights are the squared first
#' eigenvector components. Written out rather than taken from a package
#' so this arm runs the same arithmetic as the Python one -- both need
#' only a symmetric eigensolver. The weights sum to 1, so sum(w * g(x))
#' is E[g(X)] for X ~ N(0, 1).
#'
#' @param n See Usage.
#' @return A list with \code{nodes}, \code{weights}.
#' @export
.schab_gauss_hermite <- function(n) {
  # Golub-Welsch: nodes are the eigenvalues of the symmetric tridiagonal
  # Jacobi matrix (zero diagonal, sqrt(k) off-diagonal for the probabilists'
  # Hermite system); weights are the squared first eigenvector components.
  # Written out rather than taken from a package so this arm runs the same
  # arithmetic as the Python one -- both need only a symmetric eigensolver.
  # The weights sum to 1, so sum(w * g(x)) is E[g(X)] for X ~ N(0, 1).
  n <- as.integer(n)
  if (n < 1L) stop("`n` must be positive", call. = FALSE)
  jac <- matrix(0, n, n)
  if (n > 1L) {
    off <- sqrt(seq_len(n - 1L))
    for (k in seq_len(n - 1L)) {
      jac[k, k + 1L] <- off[k]
      jac[k + 1L, k] <- off[k]
    }
  }
  e <- eigen(jac, symmetric = TRUE)
  ord <- order(e$values)
  list(nodes = e$values[ord], weights = (e$vectors[1, ord])^2)
}

#' B_p = integral g(x) eta_p(x) f(x) dx, eq (5.65), by Gauss-Hermite
#'
#' quadrature -- exact for polynomial g, so the Example 5.12 identities
#' come out exactly.
#'
#' @param g See Usage.
#' @param degree See Usage.
#' @param n_quad Defaults to \code{NULL}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_hermite_coefficients <- function(g, degree, n_quad = NULL) {
  # b_p = integral g(x) eta_p(x) f(x) dx, eq (5.65), by Gauss-Hermite
  # quadrature -- exact for polynomial g, so the Example 5.12 identities
  # come out exactly.
  degree <- as.integer(degree)
  if (is.null(n_quad)) n_quad <- 2L * degree + 32L
  q <- .schab_gauss_hermite(n_quad)
  eta <- .schab_hermite_orthonormal(q$nodes, degree)
  as.numeric(eta %*% (q$weights * g(q$nodes)))
}

#' Exact coefficients of I(Z <= z_k), eq (5.72). Quadrature must NOT be
#'
#' used: it is exact for polynomials and the indicator is a step
#' function, so it converges slowly and silently -- and the indicator is
#' the canonical target of the method. b_0 = F(z_k), b_p = (-1)^p
#' H_{p-1}(z_k) f(z_k) / sqrt(p!)
#'
#' @param z_k See Usage.
#' @param degree See Usage.
#' @return The value of \code{b}, as built in the body.
#' @export
.schab_indicator_coefficients <- function(z_k, degree) {
  # Exact coefficients of I(Z <= z_k), eq (5.72). Quadrature must NOT be
  # used: it is exact for polynomials and the indicator is a step function,
  # so it converges slowly and silently -- and the indicator is the
  # canonical target of the method.
  #   b_0 = F(z_k),  b_p = (-1)^p H_{p-1}(z_k) f(z_k) / sqrt(p!)
  degree <- as.integer(degree)
  z_k <- as.numeric(z_k)
  h <- as.numeric(.schab_hermite_e(z_k, degree))
  fz <- exp(-0.5 * z_k^2) / sqrt(2 * pi)
  b <- numeric(degree + 1L)
  b[1] <- stats::pnorm(z_k)
  for (p in seq_len(degree)) {
    b[p + 1L] <- ((-1)^p) * h[p] * fz / sqrt(.schab_factorial(p))
  }
  b
}

#' .schab_disjunctive_kriging
#'
#' Part of the schab_hermite_shared implementation; see the file header
#' for the source it follows.
#'
#' @param coords See Usage.
#' @param y See Usage.
#' @param target See Usage.
#' @param correlation_fn See Usage.
#' @param b See Usage.
#' @param degree See Usage.
#' @return A list with \code{prediction}, \code{variance}, \code{coefficients}, \code{component_variances}.
#' @export
.schab_disjunctive_kriging <- function(coords, y, target, correlation_fn,
                                       b, degree) {
  # The (5.67)-(5.71) loop, for coefficients already in hand.
  coords <- as.matrix(coords)
  y <- as.numeric(y)
  target <- matrix(as.numeric(target), nrow = 1)
  degree <- as.integer(degree)
  if (nrow(coords) != length(y)) {
    stop("`coords` and `y` must have the same number of rows", call. = FALSE)
  }
  d_mat <- as.matrix(stats::dist(coords))
  d_vec <- as.numeric(sqrt(rowSums((coords - matrix(target, nrow(coords),
    ncol(coords),
    byrow = TRUE
  ))^2)))
  rho_mat <- matrix(correlation_fn(as.numeric(d_mat)), nrow(d_mat), ncol(d_mat))
  rho_vec <- as.numeric(correlation_fn(d_vec))
  eta <- .schab_hermite_orthonormal(y, degree)
  pred <- b[1]
  var <- 0
  comp <- numeric(degree + 1L)
  for (p in seq_len(degree)) {
    r_mat <- rho_mat^p
    r_vec <- rho_vec^p
    lam <- tryCatch(solve(r_mat, r_vec),
      error = function(e) as.numeric(.morie_pinv(r_mat) %*% r_vec)
    )
    pred <- pred + b[p + 1L] * sum(lam * eta[p + 1L, ])
    s2 <- 1 - sum(lam * r_vec)
    comp[p + 1L] <- s2
    var <- var + b[p + 1L]^2 * s2
  }
  list(
    prediction = pred, variance = var, coefficients = b,
    component_variances = comp
  )
}
