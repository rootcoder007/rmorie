# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nonparametric additive model by marginal integration
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 3.1.1, equations (3.6) to (3.9) (pages 55-57).
#' For E(Y|X=x) = mu + m_1(x^1) + ... + m_d(x^d) the decomposition is
#' unique only after the location normalisation E\[m_j(X^j)\] = 0 (3.6);
#' then E(Y) = mu (3.7) and
#'
#'   m_1(x^1) = integral E(Y|X=x) p_{-1}(x^{-1}) dx^{-1} - mu   (3.8)
#'
#' estimated by averaging the product-kernel estimator (3.9) over the
#' sample.  Bandwidths are explicit with fixed defaults; nothing is
#' cross-validated and nothing is random.  The additive structure is
#' what buys the escape from the curse of dimensionality.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param h1 Numeric bandwidth for the direction being estimated;
#'   default n^(-1/5).
#' @param h2 Numeric bandwidth for the remaining directions; default
#'   n^(-1/(d+3)).
#' @param ngrid Integer points per component grid.
#' @param grids Optional list of explicit evaluation grids.
#' @return Named list with mu, grids, components, fitted, resid, rss,
#'   h1, h2, d, n, method.
#' @keywords internal
#' @examples
#' n <- 150
#' x1 <- seq(-2, 2, length.out = n)
#' x2 <- cos(seq_len(n) * 0.9) * 2
#' Npaddreg(cbind(x1, x2), 1 + x1 + 0.5 * x2^2, h1 = 0.4, h2 = 0.5)$mu
#' @export
Npaddreg <- function(x, y, h1 = NULL, h2 = NULL, ngrid = 25L, grids = NULL) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("an additive model needs at least two covariates.", call. = FALSE)
  }
  a1 <- if (is.null(h1)) n^(-0.2) else as.numeric(h1)
  a2 <- if (is.null(h2)) n^(-1 / (d + 3)) else as.numeric(h2)
  mu <- mean(yv)

  gs <- vector("list", d)
  comps <- vector("list", d)
  for (j in seq_len(d)) {
    g <- if (is.null(grids)) {
      seq(min(X[, j]), max(X[, j]), length.out = as.integer(ngrid))
    } else as.numeric(grids[[j]])
    gs[[j]] <- g
    W2 <- matrix(1, n, n)
    for (k in seq_len(d)) {
      if (k == j) next
      W2 <- W2 * .hrz2_gk(outer(X[, k], X[, k], "-") / a2)
    }
    K1 <- .hrz2_gk(outer(g, X[, j], "-") / a1)
    mj <- numeric(length(g))
    for (tt in seq_along(g)) {
      Wt <- W2 * rep(K1[tt, ], each = n)
      den <- rowSums(Wt)
      den <- ifelse(den > 1e-300, den, 1e-300)
      mj[tt] <- mean(as.numeric(Wt %*% yv) / den)
    }
    comps[[j]] <- mj - mean(mj)
  }

  fit <- rep(mu, n)
  for (j in seq_len(d)) {
    fit <- fit + stats::approx(gs[[j]], comps[[j]], X[, j], rule = 2)$y
  }
  r <- yv - fit
  list(mu = mu, grids = gs, components = comps, fitted = fit, resid = r,
       rss = sum(r * r), h1 = a1, h2 = a2, d = as.integer(d), n = n,
       method = "Horowitz (2009) eq. (3.6)-(3.9) marginal integration")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Npaddreg
#' @keywords internal
#' @export
morie_horowitz_additive_model <- Npaddreg
