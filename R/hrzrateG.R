# SPDX-License-Identifier: AGPL-3.0-or-later

#' Kernel estimate of the index function G and its rate
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.4, equations (2.15) to (2.18) (pages 17-18),
#' with the limit constants from Appendix A.2 (page 241).  The kernel
#' estimator of G at a point z of the index support is
#'
#'   G_n(z) = (1/(n h p_n(z))) sum_i Y_i K((z - X_i'b_n)/h)     (2.17)
#'   p_n(z) = (1/(n h))        sum_i     K((z - X_i'b_n)/h)     (2.18)
#'
#' Because b_n converges at n^(-1/2), estimating beta costs nothing
#' asymptotically, and G converges at the one-dimensional rate
#' n^(-s/(2s+1)) with sigma_R^2 = B sigma^2(z) / (c p(z)), where
#' B = integral K(v)^2 dv.
#'
#' The bandwidth is explicit or set by the fixed formula
#' h = c n^(-1/(2s+1)): nothing is cross-validated and nothing is random.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param beta Numeric index coefficients, scale normalised.
#' @param grid Optional numeric points of the index at which to
#'   evaluate G; default is \code{ngrid} equally spaced points.
#' @param h Numeric bandwidth; default c * n^(-1/(2s+1)).
#' @param s Integer smoothness / kernel order.
#' @param c Numeric bandwidth constant.
#' @param ngrid Integer number of default grid points.
#' @return Named list with grid, ghat, se, density, bandwidth,
#'   exponent, rate, effn, n, method.
#' @keywords internal
#' @examples
#' n <- 400
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.7))
#' b <- c(1, 0.5)
#' Simgrate(x, as.numeric(x %*% b)^2, b, h = 0.15)$exponent
#' @export
Simgrate <- function(x, y, beta, grid = NULL, h = NULL, s = 2L, c = 1,
                     ngrid = 25L) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  b <- as.numeric(beta)
  if (ncol(X) != length(b) && nrow(X) == length(b)) X <- t(X)
  n <- nrow(X)
  if (length(yv) != n) {
    stop("y must have one entry per row of x.", call. = FALSE)
  }
  hh <- if (is.null(h)) c * n^(-1 / (2 * as.integer(s) + 1)) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)

  z <- as.numeric(X %*% b)
  g <- if (is.null(grid)) seq(min(z), max(z), length.out = as.integer(ngrid)) else as.numeric(grid)
  u <- outer(g, z, "-") / hh
  K <- exp(-0.5 * u * u) / sqrt(2 * pi)
  ks <- rowSums(K)
  safeks <- ifelse(ks > 1e-300, ks, 1e-300)
  dens <- ks / (n * hh)
  ghat <- as.numeric(K %*% yv) / safeks
  resid2 <- outer(rep(1, length(g)), yv) - ghat
  resid2 <- resid2 * resid2
  sig2 <- rowSums(K * resid2) / safeks
  safed <- ifelse(dens > 1e-300, dens, 1e-300)
  bg <- 1 / (2 * sqrt(pi))
  se <- sqrt(bg * sig2 / (n * hh * safed))
  expo <- as.integer(s) / (2 * as.integer(s) + 1)
  list(grid = g, ghat = ghat, se = se, density = dens, bandwidth = hh,
       exponent = expo, rate = n^(-expo), effn = n * hh, n = n,
       method = "Horowitz (2009) eq. (2.17)-(2.18), rate n^{-s/(2s+1)}")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Simgrate
#' @keywords internal
#' @export
morie_horowitz_rate_G_estimation <- Simgrate
