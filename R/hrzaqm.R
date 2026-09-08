# SPDX-License-Identifier: AGPL-3.0-or-later

#' Additive model of a conditional quantile function
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 3.4, equation (3.29) (pages 81-82):
#' Y = mu + m_1(x^1) + ... + m_d(x^d) + U_alpha, where the
#' alpha-quantile of U_alpha given X = x is zero.  The book describes
#' the two-stage estimator of Horowitz and Lee (2005): a series
#' approximation fitted by minimising the check loss
#' rho_alpha(u) = abs(u) + (2 alpha - 1) u, then a local second stage,
#' giving an asymptotically normal, oracle-efficient estimator with no
#' curse of dimensionality.  Horowitz and Lee normalise by
#' integral m_j(v) dv = 0 on [-1, 1].
#'
#' Both stages use a FIXED number of IRLS iterations on the check loss
#' with no tolerance-based early exit.  Components are centred over
#' their grids, the discrete form of that integral normalisation.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param alpha Numeric quantile level in (0, 1).
#' @param K Integer series length per coordinate.
#' @param h Numeric second-stage bandwidth; default n^(-1/5).
#' @param niter Integer FIXED IRLS iterations.
#' @param ngrid Integer points per component grid.
#' @return Named list with mu, grids, components, fitted, resid,
#'   checkloss, alpha, bandwidth, K, d, n, method.
#' @keywords internal
#' @examples
#' n <- 120
#' x1 <- seq(-2, 2, length.out = n)
#' x2 <- cos(seq_len(n) * 0.9)
#' Addquant(cbind(x1, x2), 1 + 0.7 * x1 + 0.4 * x2^2, h = 0.5)$checkloss
#' @export
Addquant <- function(x, y, alpha = 0.5, K = 4L, h = NULL, niter = 40L,
                     ngrid = 25L) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("an additive model needs at least two covariates.", call. = FALSE)
  }
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) {
    stop("alpha must lie strictly between 0 and 1.", call. = FALSE)
  }
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  Ki <- as.integer(K)

  cols <- list(rep(1, n))
  for (j in seq_len(d)) {
    scv <- X[, j]
    rng <- max(scv) - min(scv)
    scv <- (scv - min(scv)) / (if (rng > 0) rng else 1) * 2 - 1
    for (k in seq_len(Ki)) cols[[length(cols) + 1L]] <- scv^k
  }
  P <- do.call(cbind, cols)
  theta <- .hrz2_qirls(P, yv, rep(1, n), a, niter = as.integer(niter))
  stage1 <- as.numeric(P %*% theta)

  resid1 <- yv - stage1
  gs <- vector("list", d)
  comps <- vector("list", d)
  for (j in seq_len(d)) {
    g <- seq(min(X[, j]), max(X[, j]), length.out = as.integer(ngrid))
    gs[[j]] <- g
    mj <- numeric(length(g))
    for (tt in seq_along(g)) {
      w <- .hrz2_gk((g[tt] - X[, j]) / hh)
      Dm <- cbind(rep(1, n), X[, j] - g[tt])
      co <- .hrz2_qirls(Dm, resid1, w, a, niter = as.integer(niter))
      mj[tt] <- co[1L]
    }
    comps[[j]] <- mj - mean(mj)
  }

  mu <- mean(stage1)
  fit <- rep(mu, n) + (stage1 - mu)
  for (j in seq_len(d)) {
    fit <- fit + stats::approx(gs[[j]], comps[[j]], X[, j], rule = 2)$y
  }
  r <- yv - fit
  list(mu = mu, grids = gs, components = comps, fitted = fit, resid = r,
       checkloss = sum(abs(r) + (2 * a - 1) * r), alpha = a,
       bandwidth = hh, K = Ki, d = as.integer(d), n = n,
       method = "Horowitz (2009) eq. (3.29), check-loss series then local fit")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Addquant
#' @keywords internal
#' @export
morie_horowitz_additive_quantile <- Addquant
