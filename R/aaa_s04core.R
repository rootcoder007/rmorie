# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shared numeric helpers for the s04 long-tail batch
#'
#' Internal only. Mirrors \code{morie.fn._s04core} on the Python side so
#' the two arms can be compared value-for-value. Base R supplies the
#' linear algebra and the quantile rule, so most of this is a naming
#' shim; the parts that are not (fixed-iteration IRLS) are the parts
#' that decide whether cross-language parity holds at all.
#'
#' @name s04_core
#' @keywords internal
NULL

.s4_expit <- function(z) ifelse(z >= 0, 1 / (1 + exp(-z)), exp(z) / (1 + exp(z)))

.s4_logit <- function(p) log(p / (1 - p))

.s4_clip <- function(v, lo, hi) pmin(pmax(v, lo), hi)

.s4_median <- function(x) {
  x <- sort(as.numeric(unlist(x)))
  n <- length(x)
  if (n == 0L) return(NaN)
  m <- n %/% 2L
  if (n %% 2L == 1L) x[m + 1L] else 0.5 * (x[m] + x[m + 1L])
}

## R quantile type 7, spelled out so the two arms cannot drift.
.s4_quantile7 <- function(x, p) {
  x <- sort(as.numeric(unlist(x)))
  n <- length(x)
  if (n == 0L) return(NaN)
  if (n == 1L) return(x[1L])
  h <- (n - 1) * p
  lo <- floor(h)
  hi <- min(lo + 1, n - 1)
  x[lo + 1L] + (h - lo) * (x[hi + 1L] - x[lo + 1L])
}

## 0-based order, ties by original position -- matches the Python arm.
.s4_order <- function(x) order(as.numeric(unlist(x)), seq_along(unlist(x))) - 1L

.s4_rank_avg <- function(x) as.numeric(rank(as.numeric(unlist(x)), ties.method = "average"))

.s4_softmax <- function(v) {
  e <- exp(v - max(v))
  e / sum(e)
}

.s4_glmbin <- function(X, y, iters = 25L, ridge = 1e-8) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  beta <- rep(0, p)
  for (it in seq_len(iters)) {
    eta <- as.numeric(X %*% beta)
    mu <- .s4_expit(eta)
    w <- .s4_clip(mu * (1 - mu), 1e-10, 0.25)
    z <- eta + (y - mu) / w
    A <- crossprod(X, X * w) + diag(ridge, p)
    rhs <- as.numeric(crossprod(X, w * z))
    beta <- as.numeric(solve(A, rhs))
  }
  beta
}

.s4_rbf <- function(X, Z, ell = 1) {
  X <- as.matrix(X); Z <- as.matrix(Z)
  out <- matrix(0, nrow(X), nrow(Z))
  for (i in seq_len(nrow(X))) {
    for (j in seq_len(nrow(Z))) {
      out[i, j] <- exp(-0.5 * sum((X[i, ] - Z[j, ])^2) / (ell * ell))
    }
  }
  out
}

.s4_gppost <- function(K, Ks, Kss, y, noise = 1e-6) {
  K <- as.matrix(K); Ks <- as.matrix(Ks)
  n <- nrow(K)
  A <- K + diag(noise, n)
  alpha <- as.numeric(solve(A, as.numeric(y)))
  mean <- as.numeric(crossprod(Ks, alpha))
  V <- solve(A, Ks)
  vr <- as.numeric(Kss) - colSums(Ks * V)
  list(mean = mean, var = vr)
}

.s4_colstd <- function(X) {
  X <- as.matrix(X)
  n <- nrow(X)
  out <- X * 0
  for (j in seq_len(ncol(X))) {
    col <- X[, j]
    m <- sum(col) / n
    s <- if (n > 1) sqrt(sum((col - m)^2) / (n - 1)) else 0
    out[, j] <- if (s > 0) (col - m) / s else 0
  }
  out
}

.s4_euclid <- function(a, b) sqrt(sum((as.numeric(a) - as.numeric(b))^2))

.s4_sgn <- function(v) ifelse(v >= 0, 1, -1)

## Half-away-from-zero. Deliberately not round(): both languages round
## half to even but disagree about which values are exactly half.
.s4_rnd <- function(v) .s4_sgn(v) * floor(abs(v) + 0.5)

## Thin QR by modified Gram-Schmidt. R diagonal is non-negative by
## construction, so Q is unique and there is no sign convention left for
## the two arms to disagree about (LAPACK and LINPACK differ here).
.s4_qr_mgs <- function(A) {
  A <- as.matrix(A)
  n <- nrow(A); p <- ncol(A)
  Q <- A
  R <- matrix(0, p, p)
  for (j in seq_len(p)) {
    if (j > 1L) for (i in seq_len(j - 1L)) {
      R[i, j] <- sum(Q[, i] * Q[, j])
      Q[, j] <- Q[, j] - R[i, j] * Q[, i]
    }
    R[j, j] <- sqrt(sum(Q[, j]^2))
    d <- if (R[j, j] > 1e-300) R[j, j] else 1e-300
    Q[, j] <- Q[, j] / d
  }
  list(Q = Q, R = R)
}

.s4_rank_first <- function(x) {
  x <- as.numeric(unlist(x))
  o <- order(x, seq_along(x))
  r <- integer(length(x))
  r[o] <- seq_along(x)
  r
}

## Outcome model Y = th0 + th1 a + th2 m + th3 a m + th4'c and mediator
## model M = b0 + b1 a + b2'c; cbar is where the decomposition is read.
.s4_medmodels <- function(Y, A, M, Cc = NULL) {
  Y <- as.numeric(Y); A <- as.numeric(A); M <- as.numeric(M)
  n <- length(Y)
  Cm <- if (is.null(Cc)) NULL else as.matrix(Cc)
  XO <- if (is.null(Cm)) cbind(1, A, M, A * M) else cbind(1, A, M, A * M, Cm)
  XM <- if (is.null(Cm)) cbind(1, A) else cbind(1, A, Cm)
  theta <- .s4_ols(XO, Y)$beta
  beta <- .s4_ols(XM, M)$beta
  cbar <- if (is.null(Cm)) numeric(0) else colSums(Cm) / n
  list(theta = theta, beta = beta, cbar = cbar)
}

## VanderWeele four-way decomposition from fitted coefficients.
.s4_fourway <- function(theta, beta, cbar, a = 1, astar = 0, m = 0) {
  d <- a - astar
  bc <- beta[1] + beta[2] * astar
  if (length(cbar)) bc <- bc + sum(beta[2 + seq_along(cbar)] * cbar)
  cde <- (theta[2] + theta[4] * m) * d
  intref <- theta[4] * (bc - m) * d
  intmed <- theta[4] * beta[2] * d * d
  pie <- (theta[3] * beta[2] + theta[4] * beta[2] * astar) * d
  list(cde = cde, intref = intref, intmed = intmed, pie = pie,
       te = cde + intref + intmed + pie)
}

## One TMLE pass for a binary point treatment. W carries its intercept.
.s4_tmle <- function(y, D, W, gbound = 0.025) {
  y <- as.numeric(y); D <- as.numeric(D); W <- as.matrix(W); n <- length(y)
  gb <- .s4_glmbin(W, D)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), gbound, 1 - gbound)
  qb <- .s4_ols(cbind(D, W), y)$beta
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Q <- ifelse(D > 0.5, Q1, Q0)
  H <- D / g - (1 - D) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (y - Q)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  Qs <- Q + eps * H
  psi <- sum(Q1s - Q0s) / n
  ic <- H * (y - Qs) + Q1s - Q0s - psi
  se <- if (n > 1) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  list(psi = psi, se = se, eps = eps, g = g, H = H, Q1 = Q1s, Q0 = Q0s,
       ic = ic, n = n)
}

## Least squares by the SAME modified Gram-Schmidt in both arms. The
## shared tail1 core uses each language own QR (MGS in Python, Householder
## in R); on an ill-conditioned design those part company around 1e-8,
## above the parity threshold and below anything a user would notice.
.s4_ols <- function(X, y) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  qr_ <- .s4_qr_mgs(X)
  Q <- qr_$Q; R <- qr_$R
  qty <- as.numeric(crossprod(Q, y))
  beta <- numeric(p)
  for (j in seq(p, 1L)) {
    d <- if (abs(R[j, j]) > 1e-300) R[j, j] else 1e-300
    s <- if (j < p) sum(R[j, (j + 1L):p] * beta[(j + 1L):p]) else 0
    beta[j] <- (qty[j] - s) / d
  }
  fitted <- as.numeric(X %*% beta)
  rinv <- .s4_triinv(R, p)
  list(beta = beta, fitted = fitted, resid = y - fitted,
       xtxinv = rinv %*% t(rinv))
}

## Upper-triangular inverse by back substitution. Floors the pivot rather
## than testing a condition number, so a rank-deficient design gives the
## same large numbers in both arms instead of one raising.
.s4_triinv <- function(R, p) {
  out <- matrix(0, p, p)
  for (j in seq_len(p)) {
    for (i in seq(j, 1L)) {
      d <- if (abs(R[i, i]) > 1e-300) R[i, i] else 1e-300
      s <- (if (i == j) 1 else 0) -
        (if (i < p) sum(R[i, (i + 1L):p] * out[(i + 1L):p, j]) else 0)
      out[i, j] <- s / d
    }
  }
  out
}

## Optimal assignment, Kuhn-Munkres shortest-augmenting-path form. Exact,
## O(n^3). Ties broken by the first strictly improving index so both arms
## walk the same path. Returns 0-based column for each row.
.s4_hungarian <- function(cost) {
  Cst <- as.matrix(cost); n <- nrow(Cst)
  INF <- Inf
  u <- rep(0, n + 1L); v <- rep(0, n + 1L)
  p <- rep(0L, n + 1L); way <- rep(0L, n + 1L)
  for (i in seq_len(n)) {
    p[1L] <- i
    j0 <- 0L
    minv <- rep(INF, n + 1L); used <- rep(FALSE, n + 1L)
    repeat {
      used[j0 + 1L] <- TRUE
      i0 <- p[j0 + 1L]; delta <- INF; j1 <- 0L
      for (j in seq_len(n)) {
        if (!used[j + 1L]) {
          cur <- Cst[i0, j] - u[i0 + 1L] - v[j + 1L]
          if (cur < minv[j + 1L]) { minv[j + 1L] <- cur; way[j + 1L] <- j0 }
          if (minv[j + 1L] < delta) { delta <- minv[j + 1L]; j1 <- j }
        }
      }
      for (j in 0:n) {
        if (used[j + 1L]) {
          u[p[j + 1L] + 1L] <- u[p[j + 1L] + 1L] + delta
          v[j + 1L] <- v[j + 1L] - delta
        } else minv[j + 1L] <- minv[j + 1L] - delta
      }
      j0 <- j1
      if (p[j0 + 1L] == 0L) break
    }
    repeat {
      j1 <- way[j0 + 1L]
      p[j0 + 1L] <- p[j1 + 1L]
      j0 <- j1
      if (j0 == 0L) break
    }
  }
  ans <- integer(n)
  for (j in seq_len(n)) if (p[j + 1L] > 0L) ans[p[j + 1L]] <- j - 1L
  ans
}
