# SPDX-License-Identifier: AGPL-3.0-or-later

#' Single-index model of a conditional quantile function
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.9, equations (2.56)-(2.57) (pages 48-49):
#' Q_alpha(Y|X=x) = G(x'beta), identified under the assumptions of
#' Theorem 2.1.  beta is proportional to dQ_alpha(Y|X=x)/dx, so with a
#' weight W, delta = E[W(x) dQ_alpha(Y|X=x)/dx] equals beta up to
#' scale and the average-derivative estimator is
#' deltahat_AD = (1/n) sum_i [dQhat_alpha(Y|X_i)/dx] W(X_i).
#' Chaudhuri, Doksum and Samarov (1997) derive its asymptotics.
#'
#' Local derivatives come from kernel-weighted linear quantile
#' regressions fitted by a FIXED number of IRLS iterations on the check
#' loss, with no tolerance-based early exit.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param alpha Numeric quantile level in (0, 1).
#' @param h Numeric bandwidth of the local quantile fits; default
#'   n^(-1/(d+4)).
#' @param hg Numeric bandwidth for the quantile regression on the
#'   fitted index; default n^(-1/5).
#' @param niter Integer FIXED IRLS iterations.
#' @param ngrid Integer points of the index grid.
#' @param weights Optional numeric W(X_i) in (2.57); default ones.
#' @return Named list with estimate, delta, index, grid, ghat, alpha,
#'   bandwidth, hg, n, method.
#' @keywords internal
#' @examples
#' n <- 90
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.9))
#' z <- as.numeric(x %*% c(1, 0.7))
#' Simquant(x, z + 0.2 * z^2, h = 0.8, hg = 0.4)$estimate
#' @export
Simquant <- function(x, y, alpha = 0.5, h = NULL, hg = NULL, niter = 40L,
                     ngrid = 25L, weights = NULL) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("a single-index model needs at least two covariates.", call. = FALSE)
  }
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) {
    stop("alpha must lie strictly between 0 and 1.", call. = FALSE)
  }
  hh <- if (is.null(h)) n^(-1 / (d + 4)) else as.numeric(h)
  hgv <- if (is.null(hg)) n^(-0.2) else as.numeric(hg)
  W <- if (is.null(weights)) rep(1, n) else as.numeric(weights)

  delta <- rep(0, d)
  for (i in seq_len(n)) {
    Xi <- X - rep(X[i, ], each = n)
    w <- rep(1, n)
    for (j in seq_len(d)) w <- w * .hrz2_gk(Xi[, j] / hh)
    Dm <- cbind(rep(1, n), Xi)
    co <- .hrz2_qirls(Dm, yv, w, a, niter = as.integer(niter))
    delta <- delta + W[i] * co[-1L]
  }
  delta <- delta / n
  if (abs(delta[1L]) < 1e-300) {
    stop(paste("the average derivative in the first coordinate is zero, so",
               "the scale normalisation beta_1 = 1 is unavailable."),
         call. = FALSE)
  }
  beta <- delta / delta[1L]

  z <- as.numeric(X %*% beta)
  g <- seq(min(z), max(z), length.out = as.integer(ngrid))
  gh <- numeric(length(g))
  for (tt in seq_along(g)) {
    w <- .hrz2_gk((g[tt] - z) / hgv)
    Dm <- cbind(rep(1, n), z - g[tt])
    co <- .hrz2_qirls(Dm, yv, w, a, niter = as.integer(niter))
    gh[tt] <- co[1L]
  }
  list(estimate = beta, delta = delta, index = z, grid = g, ghat = gh,
       alpha = a, bandwidth = hh, hg = hgv, n = n,
       method = "Horowitz (2009) eq. (2.56)-(2.57) quantile average derivative")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Simquant
#' @keywords internal
#' @export
morie_horowitz_sim_quantile <- Simquant
