# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparse variational GP classification
#'
#' The variational bound over M inducing values, with the expectation
#' taken by Gauss-Hermite quadrature whose nodes come from the
#' Golub-Welsch tridiagonal (so both language arms generate the same
#' nodes rather than carrying a hard-coded table).  The variational
#' parameters move by fixed-step gradient ascent with central-difference
#' gradients -- deterministic, unlike the stochastic natural gradients
#' of the paper, so the two arms follow the same path.  At the starting
#' point q(u) = p(u) the KL term is exactly zero.
#'
#' Formula: L = sum_i E_q[log p(y_i | f_i)] - KL(q(u) || p(u)),
#'   mu_i = a_i' m, var_i = k_ii - a_i'(K_mm - S) a_i.
#'
#' @param X Training inputs, one row per point.
#' @param y Binary labels, 0 or 1.
#' @param X_test Test inputs; the training inputs by default.
#' @param M Number of inducing points, between 1 and n.
#' @param lengthscale,variance RBF hyperparameters.
#' @param iters,step Gradient ascent budget and step size.
#' @param nodes Gauss-Hermite nodes.
#' @param jitter Ridge added to K_mm.
#' @return List with \code{estimate}, \code{p}, \code{predicted},
#'   \code{latent_mean}, \code{elbo}, \code{kl}, \code{elbo_path},
#'   \code{n}, \code{method}.
#' @references Hensman, Matthews and Ghahramani (2015), Scalable
#'   variational Gaussian process classification, AISTATS 2015, PMLR
#'   38:351-360. arXiv:1411.2005
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 10, 2)
#' y <- rbinom(10, 1, 0.5)
#' Gpcgs(X, y)
Gpcgs <- function(X, y, X_test = NULL, M = 3, lengthscale = 1, variance = 1,
                  iters = 40, step = 0.05, nodes = 11, jitter = 1e-8) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("gp_classification_svgp: X is empty")
  yv <- as.integer(.s03vec(y))
  if (length(yv) != n) stop("gp_classification_svgp: X and y have different lengths")
  if (any(!(yv %in% c(0L, 1L)))) stop("gp_classification_svgp: labels must be 0 or 1")
  m <- as.integer(M)
  if (m < 1L || m > n) stop("gp_classification_svgp: M must lie between 1 and n")
  ell <- as.numeric(lengthscale); var <- as.numeric(variance)
  if (ell <= 0 || var <= 0) stop("gp_classification_svgp: lengthscale and variance must be positive")
  kf <- function(P, Q) {
    o <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      o[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    o
  }
  gh <- function(nn) {
    B <- matrix(0, nn, nn)
    if (nn > 1L) for (i in seq_len(nn - 1L)) {
      v <- sqrt(i / 2)
      B[i, i + 1L] <- v; B[i + 1L, i] <- v
    }
    e <- .s03jacobi(B)
    list(x = e$values, w = sqrt(pi) * e$vectors[1, ]^2)
  }
  idx <- if (m > 1L) round((seq_len(m) - 1L) * (n - 1) / (m - 1)) + 1L else 1L
  Z <- A[idx, , drop = FALSE]
  Kmm <- kf(Z, Z) + diag(as.numeric(jitter), m)
  Knm <- kf(A, Z)
  Ai <- matrix(0, n, m)
  for (i in seq_len(n)) Ai[i, ] <- .s03cholsolve(Kmm, Knm[i, ])
  Lp <- .s03chol(Kmm)
  q <- gh(as.integer(nodes))
  tri <- function(v) {
    L <- matrix(0, m, m); t <- 1L
    for (i in seq_len(m)) for (j in seq_len(i)) { L[i, j] <- v[t]; t <- t + 1L }
    L
  }
  bound <- function(theta) {
    mu_u <- theta[seq_len(m)]
    L <- tri(theta[(m + 1L):length(theta)])
    S <- L %*% t(L)
    tot <- 0
    for (i in seq_len(n)) {
      mi <- sum(Ai[i, ] * mu_u)
      vi <- var - as.numeric(t(Ai[i, ]) %*% (Kmm - S) %*% Ai[i, ])
      if (vi < 1e-12) vi <- 1e-12
      sgn <- if (yv[i] == 1L) 1 else -1
      e <- 0
      for (t in seq_along(q$x)) {
        z <- sgn * (mi + sqrt(2 * vi) * q$x[t])
        e <- e + q$w[t] * log(max(.s03pnorm(z), 1e-300))
      }
      tot <- tot + e / sqrt(pi)
    }
    tr <- 0
    for (j in seq_len(m)) tr <- tr + .s03cholsolve(Kmm, S[, j])[j]
    Kim <- .s03cholsolve(Kmm, mu_u)
    logdetK <- 2 * sum(log(diag(Lp)))
    dS <- sum(log(abs(diag(L)) + 1e-300))
    kl <- 0.5 * (tr + sum(mu_u * Kim) - m + logdetK - 2 * dS)
    list(b = tot - kl, kl = kl)
  }
  theta <- c(rep(0, m), unlist(lapply(seq_len(m), function(i) Lp[i, seq_len(i)])))
  path <- numeric(0); h <- 1e-5
  for (k in seq_len(as.integer(iters))) {
    b0 <- bound(theta)$b
    path <- c(path, b0)
    g <- numeric(length(theta))
    for (t in seq_along(theta)) {
      tp <- theta; tm <- theta; tp[t] <- tp[t] + h; tm[t] <- tm[t] - h
      g[t] <- (bound(tp)$b - bound(tm)$b) / (2 * h)
    }
    theta <- theta + as.numeric(step) * g
  }
  fin <- bound(theta)
  path <- c(path, fin$b)
  mu_u <- theta[seq_len(m)]
  L <- tri(theta[(m + 1L):length(theta)])
  S <- L %*% t(L)
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  Ksm <- kf(Xs, Z)
  p <- numeric(nrow(Xs)); mus <- numeric(nrow(Xs))
  for (j in seq_len(nrow(Xs))) {
    a <- .s03cholsolve(Kmm, Ksm[j, ])
    mj <- sum(a * mu_u)
    vj <- max(var - as.numeric(t(a) %*% (Kmm - S) %*% a), 1e-12)
    mus[j] <- mj
    p[j] <- .s03pnorm(mj / sqrt(1 + vj))
  }
  .t1_result(estimate = p[1], p = p, predicted = as.integer(p >= 0.5),
             latent_mean = mus, elbo = fin$b, kl = fin$kl, elbo_path = path,
             n = n,
             method = "variational bound of Hensman, Matthews & Ghahramani (2015) with Gauss-Hermite quadrature")
}
