# SPDX-License-Identifier: AGPL-3.0-or-later
# Internal helpers for the Horowitz (2009) inverse-problem and
# transformation-model shelf (Chapters 3, 5 and 6).
# Kept in their own file so both R trees hold a byte-identical copy.
#
# Horowitz, J. L. (2009). Semiparametric and Nonparametric Methods in
# Econometrics. Springer. ISBN 978-0-387-92869-2.

#' Internal helpers for the Horowitz inverse-problem shelf.
#' @keywords internal
#' @name horowitz_hrz3_helpers
NULL

.hrz3_sqrt2pi <- 2.5066282746310002

# Mid-rank transform onto [0, 1].  Horowitz (2009) p. 156 observes the
# support of (X, W) may be taken to be [0, 1]^2 with no loss of
# generality, "because it can always be satisfied by, if necessary,
# carrying out a monotone increasing transformation of (X, W)".  The
# mid-rank map is such a transformation and is exactly reproducible in
# both language arms, unlike a fitted CDF.
#' Mid-rank transform onto \[0, 1\].  Horowitz (2009) p. 156 observes the
#'
#' support of (X, W) may be taken to be \[0, 1\]^2 with no loss of
#' generality, "because it can always be satisfied by, if necessary,
#' carrying out a monotone increasing transformation of (X, W)".  The
#' mid-rank map is such a transformation and is exactly reproducible in
#' both language arms, unlike a fitted CDF.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
.hrz3_u01 <- function(v) {
  v <- as.numeric(v)
  n <- length(v)
  if (n == 0L) stop("empty input")
  (.s03rank(v) - 0.5) / n
}

# Equispaced grid on [0, 1] with trapezoid quadrature weights.
#' Equispaced grid on \[0, 1\] with trapezoid quadrature weights
#'
#' A step of the helpers_hrz3 implementation. Called by \code{Hrznpiv}, \code{Hrztiku}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{z}, \code{w}.
#' @export
.hrz3_grid_w <- function(m) {
  m <- as.integer(m)
  if (m < 3L) stop(sprintf("grid must have at least 3 points, got %d.", m))
  step <- 1 / (m - 1)
  z <- (seq_len(m) - 1) * step
  w <- rep(step, m)
  w[1L] <- step / 2
  w[m] <- step / 2
  list(z = z, w = w)
}

# Gaussian kernel matrix K((a_i - b_j)/h).
#' Gaussian kernel matrix K((a_i - b_j)/h)
#'
#' A step of the helpers_hrz3 implementation. Called by \code{.hrz3_fxw_grid}, \code{Hrznpiv}, \code{Hrzplrq} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Coerced to numeric by the body, with \code{as.numeric}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.hrz3_kmat <- function(a, b, h) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  h <- as.numeric(h)
  if (h <= 0) stop(sprintf("bandwidth must be positive, got %g.", h))
  u <- outer(a, b, "-") / h
  exp(-0.5 * u * u) / .hrz3_sqrt2pi
}

# Local-linear regression of y on z evaluated at zq (Appendix A.3).
# Local linear reproduces an affine function exactly, which is what
# makes an exact-recovery anchor possible for the additive fits built
# on it; Nadaraya-Watson does not.
#' Local-linear regression of y on z evaluated at zq (Appendix A.3)
#'
#' Local linear reproduces an affine function exactly, which is what
#' makes an exact-recovery anchor possible for the additive fits built
#' on it; Nadaraya-Watson does not.
#'
#' @param z A vector; its length is taken.
#' @param y A vector; its length is taken.
#' @param zq Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Numeric; combined arithmetically in the body.
#' @return A vector, from \code{vapply}.
#' @export
.hrz3_ll_smooth <- function(z, y, zq, h) {
  z <- as.numeric(z)
  y <- as.numeric(y)
  zq <- as.numeric(zq)
  h <- as.numeric(h)
  if (h <= 0) stop(sprintf("bandwidth must be positive, got %g.", h))
  if (length(z) != length(y)) {
    stop(sprintf("z has %d points but y has %d.", length(z), length(y)))
  }
  vapply(zq, function(q) {
    u <- (z - q) / h
    wt <- exp(-0.5 * u * u)
    s0 <- sum(wt)
    s1 <- sum(wt * u)
    s2 <- sum(wt * u * u)
    t0 <- sum(wt * y)
    t1 <- sum(wt * u * y)
    det <- s0 * s2 - s1 * s1
    if (abs(det) < 1e-300) {
      if (s0 > 0) t0 / s0 else 0
    } else {
      (s2 * t0 - s1 * t1) / det
    }
  }, 0)
}

# Weighted tau-quantile: the smallest order statistic of v whose
# cumulative normalised weight reaches tau.  With equal weights this is
# the usual empirical quantile, so it agrees with a plain sort on a
# degenerate kernel -- the anchor used by Hrzplrq.
#' Weighted tau-quantile: the smallest order statistic of v whose
#'
#' cumulative normalised weight reaches tau.  With equal weights this is
#' the usual empirical quantile, so it agrees with a plain sort on a
#' degenerate kernel -- the anchor used by Hrzplrq.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @param w A vector; its length is taken and its elements indexed.
#' @param tau Numeric; combined arithmetically in the body.
#' @return The value of \code{[}.
#' @export
.hrz3_wquant <- function(v, w, tau) {
  v <- as.numeric(v)
  w <- as.numeric(w)
  if (length(v) != length(w)) {
    stop(sprintf("v has %d points but w has %d.", length(v), length(w)))
  }
  if (length(v) == 0L) stop("empty input")
  tot <- sum(w)
  if (tot <= 0) stop("weights sum to zero")
  ord <- order(v)
  acc <- 0
  for (i in ord) {
    acc <- acc + w[i] / tot
    if (acc >= tau - 1e-12) {
      return(v[i])
    }
  }
  v[ord[length(ord)]]
}

# Density-weighted average derivative, Horowitz Sec. 2.6.1:
#   delta = E[f_X(X) dE(Y|X)/dX] = -2 E[f_X'(X) Y].
# In a single-index model delta is proportional to beta, so it fixes
# the index DIRECTION without optimising over it.  The leave-one-out
# form is used: the own-observation term of a kernel density derivative
# is identically zero only in the limit, and keeping it biases delta
# toward zero.
#' Density-weighted average derivative, Horowitz Sec. 2.6.1:
#'
#' delta = E\[f_X(X) dE(Y|X)/dX\] = -2 E\[f_X\'(X) Y\]. In a single-index
#' model delta is proportional to beta, so it fixes the index DIRECTION
#' without optimising over it.  The leave-one-out form is used: the
#' own-observation term of a kernel density derivative is identically
#' zero only in the limit, and keeping it biases delta toward zero.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; its length is taken and its elements indexed.
#' @param h Numeric; combined arithmetically in the body.
#' @return The value of \code{delta}, as built in the body.
#' @export
.hrz3_ade <- function(X, y, h) {
  X <- if (is.null(dim(X))) matrix(as.numeric(X), ncol = 1L) else as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  d <- ncol(X)
  if (length(y) != n) {
    stop(sprintf("X has %d rows but y has %d.", n, length(y)))
  }
  if (n < 3L) stop(sprintf("need at least 3 observations, got %d.", n))
  h <- as.numeric(h)
  if (h <= 0) stop(sprintf("bandwidth must be positive, got %g.", h))
  delta <- rep(0, d)
  scale <- 1 / ((n - 1) * h^(d + 1))
  for (i in seq_len(n)) {
    g <- rep(0, d)
    for (j in seq_len(n)) {
      if (j == i) next
      us <- (X[i, ] - X[j, ]) / h
      prod <- prod(exp(-0.5 * us * us) / .hrz3_sqrt2pi)
      g <- g + (-us) * prod
    }
    delta <- delta + (-2) * y[i] * g * scale / n
  }
  delta
}

# Index direction with the scale normalisation |beta_1| = 1
# (Horowitz assumption HT2(a), p. 219).
#' Index direction with the scale normalisation |beta_1| = 1
#'
#' (Horowitz assumption HT2(a), p. 219).
#'
#' @param X Passed to \code{.hrz3_ade}.
#' @param y Passed to \code{.hrz3_ade}.
#' @param h Passed to \code{.hrz3_ade}.
#' @return A numeric value.
#' @export
.hrz3_index_dir <- function(X, y, h) {
  d <- .hrz3_ade(X, y, h)
  lead <- d[1L]
  if (abs(lead) < 1e-300) {
    stop(paste("the first covariate has a zero average derivative, so the",
               "normalisation |beta_1| = 1 (HT2(a)) is not available."))
  }
  d / abs(lead)
}

# Default bandwidth on the mid-rank [0, 1] scale.  After .hrz3_u01 the
# marginals are exactly uniform on [0, 1], whose standard deviation is
# 1/sqrt(12), so Silverman's constant gives a scale rather than only a
# rate.  n^(-1/6) alone is a rate and on the unit interval is far too
# wide: at n = 40 it puts two thirds of the kernel mass outside support.
#' Default bandwidth on the mid-rank \[0, 1\] scale.  After .hrz3_u01 the
#'
#' marginals are exactly uniform on \[0, 1\], whose standard deviation is
#' 1/sqrt(12), so Silverman\'s constant gives a scale rather than only a
#' rate.  n^(-1/6) alone is a rate and on the unit interval is far too
#' wide: at n = 40 it puts two thirds of the kernel mass outside
#' support.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.hrz3_bw01 <- function(n) {
  n <- as.integer(n)
  if (n < 2L) stop(sprintf("need at least 2 observations, got %d.", n))
  1.06 * n^(-1 / 6) / sqrt(12)
}

# Bivariate kernel density of (X, W) on the grid, mass-corrected.
# A Gaussian kernel has unbounded support, so on the compact [0, 1]^2 a
# fixed share of its mass falls outside and the raw estimate does NOT
# integrate to one.  Horowitz (2009) p. 173 requires a compactly
# supported kernel (HH5) and notes boundary effects "can be
# accommodated by replacing the kernel K with a boundary kernel".
# Renormalising the discretised density to unit mass on [0, 1]^2 is
# such a correction, and it makes mass == 1 an exact identity that
# fails loudly if the 1/(n h^2) constant is mis-wired.
#' Bivariate kernel density of (X, W) on the grid, mass-corrected
#'
#' A Gaussian kernel has unbounded support, so on the compact \[0, 1\]^2 a
#' fixed share of its mass falls outside and the raw estimate does NOT
#' integrate to one.  Horowitz (2009) p. 173 requires a compactly
#' supported kernel (HH5) and notes boundary effects "can be
#' accommodated by replacing the kernel K with a boundary kernel".
#' Renormalising the discretised density to unit mass on \[0, 1\]^2 is
#' such a correction, and it makes mass == 1 an exact identity that
#' fails loudly if the 1/(n h^2) constant is mis-wired.
#'
#' @param u A vector; its length is taken.
#' @param v A vector; its length is taken.
#' @param z Passed to \code{.hrz3_kmat}.
#' @param wq Passed to \code{outer}.
#' @param h Numeric; combined arithmetically in the body.
#' @return A list with \code{f}, \code{mass}.
#' @export
.hrz3_fxw_grid <- function(u, v, z, wq, h) {
  u <- as.numeric(u)
  v <- as.numeric(v)
  n <- length(u)
  if (length(v) != n) {
    stop(sprintf("u has %d points but v has %d.", n, length(v)))
  }
  KX <- .hrz3_kmat(z, u, h)
  KW <- .hrz3_kmat(z, v, h)
  f <- (KX %*% t(KW)) / (n * h * h)
  mass <- sum(outer(wq, wq) * f)
  if (mass <= 0) stop("the kernel density estimate has non-positive mass.")
  list(f = f / mass, mass = mass)
}

# Series basis on [0, 1], eq. (5.79).  "cos" is the orthonormal cosine
# basis {1, sqrt(2) cos(pi k v)}, for which the coefficients in (5.79)
# are literally the inner products beta_j = <g, psi_j> as the text
# notes.  "poly" is the monomial basis, which spans the same spaces but
# is not orthonormal.
#' Series basis on \[0, 1\], eq. (5.79).  "cos" is the orthonormal cosine
#'
#' basis {1, sqrt(2) cos(pi k v)}, for which the coefficients in (5.79)
#' are literally the inner products beta_j = <g, psi_j> as the text
#' notes.  "poly" is the monomial basis, which spans the same spaces but
#' is not orthonormal.
#'
#' @param z A vector; its length is taken.
#' @param J A count; the body uses it as \code{seq_len(...)}.
#' @param kind One of \code{"cos"}, \code{"poly"}. Defaults to \code{"poly"}.
#' @return Nothing; this branch always raises.
#' @export
.hrz3_sieve <- function(z, J, kind = "poly") {
  z <- as.numeric(z)
  J <- as.integer(J)
  if (J < 1L) stop(sprintf("J must be at least 1, got %d.", J))
  if (kind == "poly") {
    return(outer(z, seq_len(J) - 1L, "^"))
  }
  if (kind == "cos") {
    out <- matrix(1, length(z), J)
    if (J > 1L) {
      for (k in seq_len(J - 1L)) {
        out[, k + 1L] <- sqrt(2) * cos(pi * k * z)
      }
    }
    return(out)
  }
  stop(sprintf("kind must be 'poly' or 'cos', got '%s'.", kind))
}
