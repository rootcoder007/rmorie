# SPDX-License-Identifier: AGPL-3.0-or-later
#' Scalable variational Gaussian process classification (SVGP)
#'
#' SOURCE. Hensman, J., Matthews, A. and Ghahramani, Z. (2015), "Scalable
#' Variational Gaussian Process Classification", Proceedings of the 18th
#' International Conference on Artificial Intelligence and Statistics,
#' PMLR 38:351-360.
#'
#' The inducing-point posterior q(u) = N(m, S) at inputs Z induces
#' q(f_n) = N(a_n' m, k_nn - a_n' k_mn + a_n' S a_n) with
#' a_n = K_mm^\{-1\} k_mn (Eqs. 6-8), and the bound maximised is
#' ELBO = sum_n E_\{q(f_n)\}\[log p(y_n|f_n)\] - KL(q(u) || p(u)), the
#' Bernoulli expectation done by Gauss-Hermite quadrature. The Gaussian
#' KL is closed form: (1/2)\[tr(K^\{-1\} S) + m' K^\{-1\} m - M + log|K| -
#' log|S|].
#'
#' QUADRATURE. Nodes and weights are not tabulated: they come from
#' Golub-Welsch, the eigenvalues of the symmetric tridiagonal Jacobi
#' matrix with zero diagonal and off-diagonal sqrt(k/2), weights
#' sqrt(pi) v_\{1i\}^2. \code{quad_check} is the error in
#' E_\{N(0,1)\}\[f^2\] = 1, which a wrong node set cannot pass.
#'
#' OPTIMISATION. m and the Cholesky factor of S are fitted by gradient
#' ascent with central finite differences and deterministic backtracking.
#' The paper uses analytic gradients and stochastic optimisation; a
#' deterministic derivative-free ascent is used here so both language
#' arms perform the same arithmetic. That substitution is this
#' implementation's choice, stated rather than attributed. ELBO
#' monotonicity is asserted.
#'
#' INDUCING POINTS Z sit at evenly spaced ranks of the data ordered by
#' its first coordinate, not sampled.
#'
#' @param X n-by-d training inputs.
#' @param y Binary labels in \{0, 1\}.
#' @param X_test Test inputs, or NULL to predict at X.
#' @param m_inducing M in 1..n.
#' @param lengthscale,variance RBF hyperparameters, positive, fixed.
#' @param n_quad Gauss-Hermite nodes, at least 2.
#' @param steps Ascent steps.
#' @param step_size Initial step, positive.
#' @param jitter Added to the diagonal of K_mm.
#' @return List with \code{prob}, \code{pred}, \code{fit_prob},
#'   \code{fit_pred}, \code{elbo}, \code{elbo_path},
#'   \code{elbo_monotone}, \code{kl}, \code{m}, \code{S}, \code{Z},
#'   \code{quad_check}, \code{n}, \code{d}, \code{m_inducing}.
#' @references Hensman, J., Matthews, A. and Ghahramani, Z. (2015).
#'   PMLR 38:351-360.
#' @examples
#' Vargpc(matrix(c(-2, -1, 1, 2), 4, 1), c(0, 0, 1, 1), m_inducing = 2)$pred
#' @export
Vargpc <- function(X, y, X_test = NULL, m_inducing = 4, lengthscale = 1,
                   variance = 1, n_quad = 20, steps = 40, step_size = 0.1,
                   jitter = 1e-8) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("variational_gp_classifier: X is empty")
  d <- ncol(A)
  yv <- .s03vec(y)
  if (length(yv) != n) {
    stop("variational_gp_classifier: X and y must have the same length")
  }
  for (v in yv) if (v != 0 && v != 1) {
    stop("variational_gp_classifier: y must be binary 0/1")
  }
  M <- as.integer(m_inducing)
  if (is.na(M) || M < 1L || M > n) {
    stop("variational_gp_classifier: m_inducing must lie in 1 .. n")
  }
  ls <- as.numeric(lengthscale); vr <- as.numeric(variance)
  if (!(ls > 0) || !(vr > 0)) {
    stop("variational_gp_classifier: lengthscale and variance must be positive")
  }
  Q <- as.integer(n_quad)
  if (is.na(Q) || Q < 2L) stop("variational_gp_classifier: n_quad must be at least 2")
  if (step_size <= 0) stop("variational_gp_classifier: step_size must be positive")
  if (jitter < 0) stop("variational_gp_classifier: jitter must be non-negative")
  Tt <- if (is.null(X_test)) A else .s03mat(X_test)
  if (nrow(Tt) > 0L && ncol(Tt) != d) {
    stop("variational_gp_classifier: X_test must have d columns")
  }
  rbf <- function(a, b) {
    s <- 0
    for (j in seq_along(a)) s <- s + (a[j] - b[j])^2
    vr * exp(-0.5 * s / (ls * ls))
  }
  ltri <- function(p, M) {
    L <- matrix(0, M, M); t0 <- 1L
    for (i in seq_len(M)) for (j in seq_len(i)) { L[i, j] <- p[t0]; t0 <- t0 + 1L }
    L
  }
  ord <- order(A[, 1L], seq_len(n))
  Z <- matrix(0, M, d)
  for (t in seq_len(M)) {
    pos <- if (M == 1L) 1L else as.integer(round((t - 1) * (n - 1) / (M - 1))) + 1L
    Z[t, ] <- A[ord[pos], ]
  }
  Kmm <- matrix(0, M, M)
  for (i in seq_len(M)) for (j in seq_len(M)) {
    Kmm[i, j] <- rbf(Z[i, ], Z[j, ]) + (if (i == j) jitter else 0)
  }
  Lk <- .s03chol(Kmm)
  logdetK <- 0
  for (i in seq_len(M)) logdetK <- logdetK + 2 * log(Lk[i, i])
  Kinv <- matrix(0, M, M)
  for (j in seq_len(M)) {
    e <- numeric(M); e[j] <- 1
    Kinv[, j] <- .s03cholsolve(Kmm, e)
  }
  Amat <- matrix(0, n, M)
  knn <- numeric(n)
  for (i in seq_len(n)) {
    kmn <- numeric(M)
    for (t in seq_len(M)) kmn[t] <- rbf(Z[t, ], A[i, ])
    Amat[i, ] <- .s03cholsolve(Kmm, kmn)
    q <- rbf(A[i, ], A[i, ])
    for (t in seq_len(M)) q <- q - Amat[i, t] * kmn[t]
    knn[i] <- q
  }
  J <- matrix(0, Q, Q)
  if (Q > 1L) for (k in seq_len(Q - 1L)) {
    b <- sqrt(k / 2)
    J[k + 1L, k] <- b; J[k, k + 1L] <- b
  }
  je <- .s03jacobi(J)
  gx <- je$values
  gw <- numeric(Q)
  for (i in seq_len(Q)) gw[i] <- sqrt(pi) * je$vectors[1L, i] * je$vectors[1L, i]
  sq <- sqrt(pi); r2 <- sqrt(2)
  npar <- M + (M * (M + 1L)) %/% 2L
  elbo_of <- function(p) {
    mu_u <- p[seq_len(M)]
    Lm <- ltri(p[(M + 1L):npar], M)
    S <- .s03matmul(Lm, t(Lm))
    tot <- 0
    for (i in seq_len(n)) {
      a <- Amat[i, ]
      mn <- 0
      for (t in seq_len(M)) mn <- mn + a[t] * mu_u[t]
      v <- knn[i]
      for (t in seq_len(M)) for (u in seq_len(M)) v <- v + a[t] * S[t, u] * a[u]
      if (v < 0) v <- 0
      sd <- sqrt(v)
      sgn <- if (yv[i] == 1) 1 else -1
      acc <- 0
      for (k in seq_len(Q)) {
        f <- mn + r2 * sd * gx[k]
        z <- -sgn * f
        acc <- acc + gw[k] * (-(if (z > 0) z else 0) - log1p(exp(-abs(z))))
      }
      tot <- tot + acc / sq
    }
    tr <- 0
    for (t in seq_len(M)) for (u in seq_len(M)) tr <- tr + Kinv[t, u] * S[u, t]
    qf <- 0
    for (t in seq_len(M)) for (u in seq_len(M)) qf <- qf + mu_u[t] * Kinv[t, u] * mu_u[u]
    logdetS <- 0
    for (t in seq_len(M)) {
      if (Lm[t, t] == 0) return(list(v = -Inf, kl = 0))
      logdetS <- logdetS + 2 * log(abs(Lm[t, t]))
    }
    kl <- 0.5 * (tr + qf - M + logdetK - logdetS)
    list(v = tot - kl, kl = kl)
  }
  p <- numeric(npar)
  t0 <- M + 1L
  for (i in seq_len(M)) for (j in seq_len(i)) { p[t0] <- Lk[i, j]; t0 <- t0 + 1L }
  e0 <- elbo_of(p)
  cur <- e0$v; kl <- e0$kl
  path <- cur
  h <- 1e-5
  for (it in seq_len(as.integer(steps))) {
    g <- numeric(npar)
    for (t in seq_len(npar)) {
      p[t] <- p[t] + h
      up <- elbo_of(p)$v
      p[t] <- p[t] - 2 * h
      dn <- elbo_of(p)$v
      p[t] <- p[t] + h
      g[t] <- (up - dn) / (2 * h)
    }
    gs <- sqrt(sum(g * g))
    if (gs == 0) break
    s <- step_size
    moved <- FALSE
    for (b in seq_len(20L)) {
      cand <- p + s * g / gs
      nn <- elbo_of(cand)
      if (nn$v > cur) {
        p <- cand; cur <- nn$v; kl <- nn$kl; moved <- TRUE
        break
      }
      s <- s * 0.5
    }
    path <- c(path, cur)
    if (!moved) break
  }
  mu_u <- p[seq_len(M)]
  Lm <- ltri(p[(M + 1L):npar], M)
  S <- .s03matmul(Lm, t(Lm))
  predict <- function(pts) {
    out <- numeric(nrow(pts))
    for (i in seq_len(nrow(pts))) {
      x0 <- pts[i, ]
      kmn <- numeric(M)
      for (t in seq_len(M)) kmn[t] <- rbf(Z[t, ], x0)
      a <- .s03cholsolve(Kmm, kmn)
      mn <- 0
      for (t in seq_len(M)) mn <- mn + a[t] * mu_u[t]
      v <- rbf(x0, x0)
      for (t in seq_len(M)) v <- v - a[t] * kmn[t]
      for (t in seq_len(M)) for (u in seq_len(M)) v <- v + a[t] * S[t, u] * a[u]
      if (v < 0) v <- 0
      sd <- sqrt(v)
      acc <- 0
      for (k in seq_len(Q)) acc <- acc + gw[k] * .s03sigmoid(mn + r2 * sd * gx[k])
      out[i] <- acc / sq
    }
    out
  }
  pr <- predict(Tt)
  fp <- if (is.null(X_test)) pr else predict(A)
  qc <- 0
  for (k in seq_len(Q)) qc <- qc + gw[k] * (r2 * gx[k])^2
  qc <- abs(qc / sq - 1)
  mono <- TRUE
  if (length(path) > 1L) {
    for (i in seq(2L, length(path))) if (path[i] < path[i - 1L] - 1e-10) mono <- FALSE
  }
  .t1_result(estimate = cur, prob = pr, pred = as.numeric(pr >= 0.5),
             fit_prob = fp, fit_pred = as.numeric(fp >= 0.5), elbo = cur,
             elbo_path = path, elbo_monotone = if (mono) 1 else 0, kl = kl,
             m = mu_u, S = S, Z = Z, quad_check = qc, n = n, d = d,
             m_inducing = M,
             method = paste("SVGP with Gauss-Hermite Bernoulli quadrature",
                            "(Hensman, Matthews and Ghahramani 2015 Eqs. 6-8)"))
}
