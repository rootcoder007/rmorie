# SPDX-License-Identifier: AGPL-3.0-or-later

#' Pointwise confidence bands for a nonparametric mean regression
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Appendix A.2.1, pages 240-241.  With
#' h_n = c n^(-1/(2s+1)), an order s kernel and s continuous
#' derivatives of p and g, n^(s/(2s+1)) (g_n(x) - g(x)) is
#' asymptotically normal with variance
#' sigma_R^2 = B sigma^2(x) / (c p(x)), B = integral K(v)^2 dv, so
#' pointwise bands are
#'
#'   g_n(x) +/- z * sqrt(B sigmahat^2(x) / (n h p_n(x))).
#'
#' The bias mu_R is NOT subtracted: the book gives it in terms of the
#' unknown derivatives of g and p and supplies no estimator, so these
#' bands are bias-uncorrected and say so.
#'
#' The book states the uniform rate sup_x abs(g_n(x) - g(x)) =
#' O(((log n)/(n h_n))^(1/2)) almost surely (page 241) but gives NO
#' explicit constant or critical value for a uniform band, so no
#' uniform band is returned; the uniform rate is a diagnostic only.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric outcome vector.
#' @param grid Optional numeric evaluation points; default \code{ngrid}
#'   equally spaced points spanning the observed x.
#' @param h Numeric bandwidth; default c * n^(-1/(2s+1)).
#' @param alpha Numeric two-sided level; the band is 1 - alpha pointwise.
#' @param s Integer smoothness / kernel order.
#' @param c Numeric bandwidth constant.
#' @param ngrid Integer number of default grid points.
#' @return Named list with grid, ghat, se, lower, upper, density,
#'   bandwidth, zcrit, unifrate, alpha, n, method.
#' @keywords internal
#' @examples
#' xv <- seq(0, 1, length.out = 400)
#' Npconfband(xv, sin(3 * xv), h = 0.05)$zcrit
#' @export
Npconfband <- function(x, y, grid = NULL, h = NULL, alpha = 0.05, s = 2L,
                       c = 1, ngrid = 25L) {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  n <- length(xv)
  if (length(yv) != n) {
    stop("x and y must have the same length.", call. = FALSE)
  }
  if (!(alpha > 0 && alpha < 1)) {
    stop("alpha must lie strictly between 0 and 1.", call. = FALSE)
  }
  hh <- if (is.null(h)) c * n^(-1 / (2 * as.integer(s) + 1)) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)
  g <- if (is.null(grid)) seq(min(xv), max(xv), length.out = as.integer(ngrid)) else as.numeric(grid)

  u <- outer(g, xv, "-") / hh
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
  z <- stats::qnorm(1 - alpha / 2)
  list(grid = g, ghat = ghat, se = se, lower = ghat - z * se,
       upper = ghat + z * se, density = dens, bandwidth = hh, zcrit = z,
       unifrate = sqrt(log(n) / (n * hh)), alpha = alpha, n = n,
       method = "Horowitz (2009) Appendix A.2.1 pointwise bands (bias uncorrected)")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Npconfband
#' @keywords internal
#' @export
morie_horowitz_confidence_bands <- Npconfband
