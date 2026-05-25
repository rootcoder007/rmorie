# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: SAR-lag concentrated negative log-likelihood in rho.
# Extracted from the sarla() optimiser closure so the singular-system /
# non-positive-variance guard is directly unit-testable. `e0`, `e1` are
# the residual projections, `n` the sample size, `I`/`W` the identity
# and spatial-weights matrices.
.sarla_negll <- function(rho, e0, e1, n, I, W) {
  e <- e0 - rho * e1
  sigma2 <- as.numeric(sum(e^2)) / n
  A <- I - rho * W
  det_sign <- determinant(A, logarithm = TRUE)
  # !is.finite(modulus) catches a singular A (det == 0), for which
  # determinant() still reports sign = +1; without it the -logdetA term
  # below would be +Inf rather than the intended penalty.
  if (det_sign$sign <= 0 || sigma2 <= 0 || !is.finite(det_sign$modulus)) {
    return(1e12)
  }
  logdetA <- as.numeric(det_sign$modulus)
  0.5 * n * log(2 * pi * sigma2) - logdetA + 0.5 * n
}

#' Spatial autoregressive lag model (SAR lag, ML).
#'
#' Y = rho W Y + X beta + eps,  eps ~ N(0, sigma2 I).
#' Concentrated log-likelihood in rho.
#'
#' @param x Design matrix (n by p).
#' @param y Response, length n.
#' @param w n-by-n weights matrix.
#' @return Named list: estimate, se, rho, sigma2, n, method.
#' @references Anselin (1988); Schabenberger & Gotway (2005), Ch 7.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
sarla <- function(x, y, w) {
  X <- as.matrix(x)
  y <- as.numeric(y)
  W <- as.matrix(w)
  n <- length(y)
  p <- ncol(X)
  if (nrow(X) != n || any(dim(W) != c(n, n))) {
    stop("shape mismatch among x, y, w")
  }
  I <- diag(n)
  XtX_inv <- solve(crossprod(X))
  M <- I - X %*% XtX_inv %*% t(X)
  e0 <- M %*% y
  e1 <- M %*% (W %*% y)
  neg_ll <- function(rho) .sarla_negll(rho, e0, e1, n, I, W)
  res <- stats::optimize(neg_ll, interval = c(-0.99, 0.99))
  rho <- res$minimum
  Wy <- W %*% y
  y_star <- as.numeric(y - rho * Wy)
  beta <- as.numeric(XtX_inv %*% crossprod(X, y_star))
  e <- y_star - as.numeric(X %*% beta)
  sigma2 <- sum(e^2) / max(n - p - 1, 1)
  cov_b <- sigma2 * XtX_inv
  se <- sqrt(pmax(diag(cov_b), 0))
  list(
    estimate = beta, se = se, rho = rho, sigma2 = sigma2, n = n,
    method = "SAR lag (ML, concentrated log-likelihood)"
  )
}

# CANONICAL TEST  (with row-standardised path graph)

#' @rdname sarla
#' @keywords internal
#' @export
morie_spatial_ar_lag <- sarla
