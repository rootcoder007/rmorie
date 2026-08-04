# SPDX-License-Identifier: AGPL-3.0-or-later

#' Identification of beta in the partially linear model
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 3.6.1, equations (3.30) to (3.33) (pages
#' 85-86).  For Y = X'beta + g(Z) + U with E(U | X, Z) = 0,
#' differencing out E(. | Z) gives (3.32) and hence
#' beta = E(Xt Xt')^{-1} E(Xt Yt) with Xt = X - E(X | Z), provided
#'
#'   Sigma_X = E[X - E(X|Z)][X - E(X|Z)]' > 0                (3.33)
#'
#' which fails when X is a deterministic function of Z and rules out
#' an intercept in X, since any intercept is absorbed into g.
#'
#' E(X | Z) is a Nadaraya-Watson fit with a product Gaussian kernel and
#' an explicit bandwidth; nothing is cross-validated and nothing is
#' random.
#'
#' @param x Numeric matrix, n by p, of covariates entering linearly.
#' @param z Numeric matrix, n by q, of covariates entering through g.
#' @param h Numeric bandwidth; default is the fixed formula
#'   n^(-1/(4+q)).
#' @param tol Numeric; eigenvalues at or below tol times the largest
#'   count as zero.
#' @return Named list with identified, mineig, maxeig, condnum,
#'   eigvals, rank, dim, hasintercept, bandwidth, n, method.
#' @keywords internal
#' @examples
#' n <- 120
#' z <- seq(-2, 2, length.out = n)
#' x <- cbind(cos(seq_len(n) * 1.7), sin(seq_len(n) * 0.9))
#' Plrident(x, matrix(z, ncol = 1))$identified
#' @export
Plrident <- function(x, z, h = NULL, tol = 1e-10) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  Z <- if (is.null(dim(z))) matrix(z, ncol = 1L) else as.matrix(z)
  if (nrow(Z) != nrow(X)) Z <- t(Z)
  n <- nrow(X)
  p <- ncol(X)
  q <- ncol(Z)
  hh <- if (is.null(h)) n^(-1 / (4 + q)) else as.numeric(h)

  sdcol <- apply(X, 2L, function(v) sqrt(mean((v - mean(v))^2)))
  hasintercept <- any(sdcol <= 0)

  W <- matrix(1, n, n)
  for (j in seq_len(q)) {
    u <- outer(Z[, j], Z[, j], "-") / hh
    W <- W * (exp(-0.5 * u * u) / sqrt(2 * pi))
  }
  den <- rowSums(W)
  den[den <= 1e-300] <- 1e-300
  Xt <- X - (W %*% X) / den

  Sigma <- crossprod(Xt) / n
  ev <- sort(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)
  mineig <- ev[1L]
  maxeig <- ev[length(ev)]
  rank <- sum(ev > maxeig * tol)
  condnum <- if (mineig > 0) maxeig / mineig else Inf
  list(identified = mineig > maxeig * tol && !hasintercept,
       mineig = mineig, maxeig = maxeig, condnum = condnum,
       eigvals = ev, rank = as.integer(rank), dim = as.integer(p),
       hasintercept = hasintercept, bandwidth = hh, n = n,
       method = "Horowitz (2009) eq. (3.33), Sigma_X positive definite")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Plrident
#' @keywords internal
#' @export
morie_horowitz_plr_identification <- Plrident
