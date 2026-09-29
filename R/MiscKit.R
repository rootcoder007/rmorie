# SPDX-License-Identifier: AGPL-3.0-or-later
# Crime Severity Index, U-learner, repeated k-fold indices, FastICA and rStress MDS.
# Identical to the Python arm morie.fn.misckit.

#' Crime Severity Index, U-learner, repeated k-fold indices, FastICA and rStress MDS
#'
#' \code{CsiWeights}: incarceration rate times mean sentence in days.
#' \code{CrimeSeverityIndex}: \code{100 R_t / R_base} with
#' \code{R_t = sum_i w_i n_ti / P_t * per} (Babyak et al. 2009).
#' \code{ULearnerCate}: cross-fitted ridge outcome and logistic propensity
#' models, pseudo-outcome \code{(Y - m) / (D - e)} regressed on \code{X}.
#' \code{RepeatedKfoldIndices}: Philox permutations dealt to folds (0-based).
#' \code{FastIca}: parallel FastICA with the log-cosh contrast after
#' eigen-whitening. \code{RstressMds}: L-BFGS minimisation of
#' \code{sum w (delta - d^r)^2} from a classical-scaling start.
#'
#' @param incarceration_rate,mean_sentence_days Offence-level rates and sentences.
#' @param counts Periods by offence types counts.
#' @param weights Offence weights, or MDS weights.
#' @param population Populations per period.
#' @param base Base period (0-based).
#' @param per Rate multiplier.
#' @param y Outcome.
#' @param d Treatment indicator.
#' @param X Covariates, or data matrix for \code{FastIca}.
#' @param folds,l2,clip Cross-fitting folds, ridge penalty and propensity clip.
#' @param seed Philox seed.
#' @param n,k,repeats Sample size, folds and repeats.
#' @param n_comp Number of components.
#' @param w_init Initial unmixing matrix, or NULL for the identity.
#' @param alpha Log-cosh constant.
#' @param max_iter,tol Iteration limit and tolerance.
#' @param delta Dissimilarity matrix.
#' @param r rStress power.
#' @param ndim Number of dimensions.
#' @return A list or vector.
#' @references Babyak, C. et al. (2009). The methodology of the police-reported
#'   Crime Severity Index. SSC Proceedings. Kunzel, S. R. et al. (2019). PNAS
#'   116, 4156-4165. Kuhn, M. and Johnson, K. (2013). Applied Predictive
#'   Modeling. Hyvarinen, A. (1999). IEEE Trans. Neural Networks 10, 626-634.
#'   de Leeuw, J., Groenen, P. J. F. and Mair, P. (2016). Minimizing rStress
#'   using majorization. UCLA.
#' @examples
#' CrimeSeverityIndex(rbind(c(10, 2), c(8, 3)), c(1, 50), c(1000, 1000))$index
#' RepeatedKfoldIndices(6, 3, 2)
#' @export
CsiWeights <- function(incarceration_rate, mean_sentence_days) as.numeric(incarceration_rate) * as.numeric(mean_sentence_days)

#' @rdname CsiWeights
#' @export
CrimeSeverityIndex <- function(counts, weights, population, base = 0, per = 1e5) {
  R <- as.numeric(as.matrix(counts) %*% as.numeric(weights)) / as.numeric(population) * per
  list(index = 100 * R / R[base + 1], weighted_rate = R)
}

.mk_perm <- function(n, seed, stream) {
  perm <- seq_len(n) - 1
  u <- .morie_random_uniform(n, seed = seed, stream = stream)
  if (n > 1) for (i in (n - 1):1) {
    j <- floor(u[i + 1] * (i + 1))
    tmp <- perm[i + 1]
    perm[i + 1] <- perm[j + 1]
    perm[j + 1] <- tmp
  }
  perm
}

.mk_ridge <- function(X, y, lam) {
  pen <- diag(c(0, rep(lam, ncol(X) - 1)), ncol(X))
  as.numeric(solve(crossprod(X) + pen, crossprod(X, y)))
}

.mk_logit_ridge <- function(X, d, lam) {
  b <- numeric(ncol(X))
  pen <- c(0, rep(lam, ncol(X) - 1))
  for (it in seq_len(50)) {
    pr <- 1 / (1 + exp(-as.numeric(X %*% b)))
    g <- as.numeric(crossprod(X, d - pr)) - pen * b
    H <- crossprod(X, X * (pr * (1 - pr))) + diag(pen, ncol(X))
    st <- as.numeric(solve(H, g))
    b <- b + st
    if (max(abs(st)) <= 1e-12) break
  }
  b
}

#' @rdname CsiWeights
#' @export
ULearnerCate <- function(y, d, X, folds = 5, l2 = 1, clip = 0.01, seed = 0) {
  y <- as.numeric(y)
  d <- as.numeric(d)
  Xr <- cbind(1, unname(as.matrix(X)) * 1)
  n <- length(y)
  fold <- integer(n)
  fold[.mk_perm(n, seed, 0) + 1] <- (seq_len(n) - 1) %% folds
  mhat <- ehat <- numeric(n)
  for (f in 0:(folds - 1)) {
    tr <- which(fold != f)
    te <- which(fold == f)
    bm <- .mk_ridge(Xr[tr, , drop = FALSE], y[tr], l2)
    be <- .mk_logit_ridge(Xr[tr, , drop = FALSE], d[tr], l2)
    mhat[te] <- as.numeric(Xr[te, , drop = FALSE] %*% bm)
    ehat[te] <- 1 / (1 + exp(-as.numeric(Xr[te, , drop = FALSE] %*% be)))
  }
  den <- d - ehat
  den <- ifelse(abs(den) < clip, ifelse(den >= 0, clip, -clip), den)
  U <- (y - mhat) / den
  bt <- .mk_ridge(Xr, U, l2)
  list(coefficients = bt, tau = as.numeric(Xr %*% bt), pseudo_outcome = U, m_hat = mhat, e_hat = ehat)
}

#' @rdname CsiWeights
#' @export
RepeatedKfoldIndices <- function(n, k = 10, repeats = 3, seed = 0) {
  lapply(seq_len(repeats) - 1, function(r) {
    fold <- integer(n)
    fold[.mk_perm(n, seed, r) + 1] <- (seq_len(n) - 1) %% k
    fold
  })
}

.mk_eig <- function(M) {
  e <- eigen(M, symmetric = TRUE)
  V <- e$vectors
  for (i in seq_len(ncol(V))) if (V[which.max(abs(V[, i])), i] < 0) V[, i] <- -V[, i]
  list(values = e$values, vectors = V)
}

.mk_sym_decorr <- function(W) {
  e <- .mk_eig(W %*% t(W))
  e$vectors %*% diag(1 / sqrt(e$values), nrow(W)) %*% t(e$vectors) %*% W
}

#' @rdname CsiWeights
#' @export
FastIca <- function(X, n_comp, w_init = NULL, alpha = 1, max_iter = 200, tol = 1e-4) {
  X <- unname(as.matrix(X)) * 1
  n <- nrow(X)
  Xc <- sweep(X, 2, colMeans(X))
  e <- .mk_eig(crossprod(Xc) / n)
  K <- t(e$vectors[, seq_len(n_comp), drop = FALSE]) / sqrt(e$values[seq_len(n_comp)])
  Z <- K %*% t(Xc)
  W <- if (is.null(w_init)) diag(n_comp) else as.matrix(w_init)
  W <- .mk_sym_decorr(W)
  it <- 0
  for (iter in seq_len(max_iter)) {
    it <- it + 1
    G <- tanh(alpha * W %*% Z)
    v1 <- G %*% t(Z) / n
    gp <- rowSums(alpha * (1 - G^2)) / n
    W1 <- .mk_sym_decorr(v1 - gp * W)
    lim <- max(abs(abs(rowSums(W1 * W)) - 1))
    W <- W1
    if (lim < tol) break
  }
  WK <- W %*% K
  list(S = t(WK %*% t(Xc)), W = W, K = K, A = if (n_comp == ncol(X)) solve(WK) else NULL, iterations = it)
}

#' @rdname CsiWeights
#' @export
RstressMds <- function(delta, r = 0.5, ndim = 2, weights = NULL, max_iter = 2000) {
  D <- unname(as.matrix(delta)) * 1
  n <- nrow(D)
  Wt <- if (is.null(weights)) 1 - diag(n) else as.matrix(weights)
  B0 <- -0.5 * D^(2 / r)
  B <- B0 - outer(rowMeans(B0), rowMeans(B0), "+") + mean(B0)
  e <- .mk_eig(B)
  x0 <- as.numeric(t(e$vectors[, seq_len(ndim), drop = FALSE] %*% diag(sqrt(pmax(e$values[seq_len(ndim)], 0)), ndim)))
  fg <- function(x) {
    Xm <- matrix(x, n, ndim, byrow = TRUE)
    f <- 0
    g <- matrix(0, n, ndim)
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      if (Wt[i, j] == 0) next
      diff <- Xm[i, ] - Xm[j, ]
      dd <- sqrt(sum(diff^2))
      res <- D[i, j] - dd^r
      f <- f + Wt[i, j] * res^2
      if (dd > 0) {
        cc <- -2 * Wt[i, j] * res * r * dd^(r - 2)
        g[i, ] <- g[i, ] + cc * diff
        g[j, ] <- g[j, ] - cc * diff
      }
    }
    list(f = f, g = as.numeric(t(g)))
  }
  res <- LbfgsbMinimize(function(v) fg(v)$f, x0, grad = function(v) fg(v)$g, pgtol = 1e-10, factr = 10, max_iter = max_iter)
  x <- as.numeric(res$x)
  raw <- fg(x)$f
  conf <- matrix(x, n, ndim, byrow = TRUE)
  conf <- sweep(conf, 2, colMeans(conf))
  norm <- sum((Wt * D^2)[upper.tri(D)])
  list(conf = conf, raw_stress = raw, stress = sqrt(raw / norm), r = r)
}
