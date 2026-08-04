# SPDX-License-Identifier: AGPL-3.0-or-later

#' Multiple-index model
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.2, equation (2.5) (page 10):
#'
#'   E(Y | X = x) = x_0'beta_0 + G(x_1'beta_1, ..., x_M'beta_M)
#'
#' with M known, each x_m a subvector of x, and G unknown.  If the
#' betas are identified they are estimable at n^(-1/2), while the
#' estimator of E(Y|X=x) converges at the rate of an M-dimensional
#' nonparametric estimator, so the curse of dimensionality bites on
#' E(Y|X=x) but not on beta (page 11).
#'
#' Each beta_m is estimated up to scale by a density-weighted average
#' derivative within its own block (Section 2.6.1, equation (2.40)),
#' normalised so its first component is one; G is then a product-kernel
#' Nadaraya-Watson regression on the fitted indices.  Bandwidths are
#' explicit with fixed defaults: no random search, no cross-validation.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param blocks List of integer vectors giving the 1-based columns of
#'   \code{x} forming each index.
#' @param x0 Optional numeric matrix of covariates entering linearly.
#' @param h Numeric bandwidth for the average-derivative step; default
#'   n^(-1/(k+4)) inside a block of width k.
#' @param hg Numeric bandwidth for the regression of Y on the fitted
#'   indices; default n^(-1/(M+4)).
#' @param ngrid Unused placeholder; G is returned at the sample indices.
#' @return Named list with estimate, beta0, indices, ghat, resid, rss,
#'   betaexp, gexp, M, n, method.
#' @keywords internal
#' @examples
#' n <- 200
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.6))
#' z <- as.numeric(x %*% c(1, 0.7))
#' Multindex(x, z + 0.3 * z^2, list(c(1L, 2L)), h = 0.6, hg = 0.3)$M
#' @export
Multindex <- function(x, y, blocks, x0 = NULL, h = NULL, hg = NULL,
                      ngrid = 0L) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  n <- nrow(X)
  if (length(yv) != n) {
    stop("y must have one entry per row of x.", call. = FALSE)
  }
  M <- length(blocks)
  if (M < 1L) stop("at least one index block is required.", call. = FALSE)

  resid <- yv
  beta0 <- NULL
  if (!is.null(x0)) {
    X0 <- if (is.null(dim(x0))) matrix(x0, ncol = 1L) else as.matrix(x0)
    if (nrow(X0) != n) X0 <- t(X0)
    beta0 <- as.numeric(qr.solve(X0, yv))
    resid <- yv - as.numeric(X0 %*% beta0)
  }

  gk <- function(u) exp(-0.5 * u * u) / sqrt(2 * pi)
  betas <- vector("list", M)
  idx <- matrix(0, n, M)
  for (m in seq_len(M)) {
    cols <- as.integer(blocks[[m]])
    Xb <- X[, cols, drop = FALSE]
    k <- length(cols)
    hb <- if (is.null(h)) n^(-1 / (k + 4)) else as.numeric(h)
    W <- matrix(1, n, n)
    for (j in seq_len(k)) W <- W * gk(outer(Xb[, j], Xb[, j], "-") / hb)
    d <- numeric(k)
    for (j in seq_len(k)) {
      diffj <- outer(Xb[, j], Xb[, j], "-")
      G <- W * (-diffj / (hb * hb))
      d[j] <- -2 * sum(resid * G) / (n * n * hb^k)
    }
    if (abs(d[1L]) < 1e-300) {
      stop(paste("the first covariate of an index block has a zero average",
                 "derivative, so the scale normalisation beta_1 = 1 is",
                 "unavailable for that block."), call. = FALSE)
    }
    b <- d / d[1L]
    betas[[m]] <- b
    idx[, m] <- as.numeric(Xb %*% b)
  }

  hgv <- if (is.null(hg)) n^(-1 / (M + 4)) else as.numeric(hg)
  W <- matrix(1, n, n)
  for (m in seq_len(M)) W <- W * gk(outer(idx[, m], idx[, m], "-") / hgv)
  den <- rowSums(W)
  den[den <= 1e-300] <- 1e-300
  ghat <- as.numeric(W %*% resid) / den
  r <- resid - ghat
  list(estimate = betas, beta0 = beta0, indices = idx, ghat = ghat,
       resid = r, rss = sum(r * r), betaexp = 0.5, gexp = 2 / (4 + M),
       M = as.integer(M), n = n,
       method = "Horowitz (2009) eq. (2.5), average-derivative indices")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Multindex
#' @keywords internal
#' @export
morie_horowitz_multiple_index_model <- Multindex
