# SPDX-License-Identifier: AGPL-3.0-or-later
# Space-time decompositions: tensors, matrix factorisation, Haar wavelets, harmonic regression.
# Identical to the Python arm morie.fn.stdecomp.

#' Space-time decompositions: CP and Tucker tensors, matrix factorisation, Haar wavelets, harmonic trends
#'
#' \code{CpAls}: CP/PARAFAC of a three-way array by alternating least squares
#' from the HOSVD start, unit-norm columns with weights, ordered by weight,
#' largest entry of each A and B column positive. \code{TuckerHooi}: HOSVD
#' followed by higher-order orthogonal iteration. \code{MatrixFactorizationAls}:
#' ridge-regularised \code{X ~ U t(V)} over observed entries (NA missing) by
#' alternating ridge regressions. \code{HaarDwt} and \code{HaarMra}: Haar
#' pyramid algorithm with \code{W_t = (V_(2t+1) - V_(2t)) / sqrt(2)} and the
#' additive multiresolution analysis (as the wavelets package).
#' \code{HaarDwt2d} and \code{HaarMra2d}: separable two-dimensional versions
#' (subbands a, h, v, d). \code{WaveletDetrend}: \code{x - S_J}.
#' \code{HarmonicRegression}: least-squares Fourier trend with amplitudes and phases.
#'
#' @param tensor Three-way array (or nested list).
#' @param rank Number of components.
#' @param ranks Tucker ranks (length 3).
#' @param max_iter,tol Iteration limit and tolerance.
#' @param X Data matrix (NA for missing).
#' @param l2 Ridge penalty.
#' @param x Series (length a multiple of \code{2^levels}).
#' @param levels Number of levels.
#' @param field Matrix (dimensions multiples of \code{2^levels}).
#' @param y Response series.
#' @param t Times.
#' @param period Fundamental period.
#' @param n_harmonics Number of harmonics.
#' @param trend Include a linear trend.
#' @return A list.
#' @references Kolda, T. G. and Bader, B. W. (2009). SIAM Review 51,
#'   455-500. De Lathauwer, L., De Moor, B. and Vandewalle, J. (2000). SIAM J.
#'   Matrix Anal. Appl. 21, 1253-1278. Koren, Y., Bell, R. and Volinsky, C.
#'   (2009). Computer 42(8), 30-37. Percival, D. B. and Walden, A. T. (2000).
#'   Wavelet Methods for Time Series Analysis. Mallat, S. G. (1989). IEEE
#'   Trans. PAMI 11, 674-693. Bloomfield, P. (2000). Fourier Analysis of Time Series.
#' @examples
#' HaarMra(c(1, 4, 2, 8, 5, 7, 3, 6), 2)$smooth
#' HarmonicRegression(2 + 3 * cos(2 * pi * (0:11) / 12), 0:11, 12, 1, trend = FALSE)$amplitude
#' @export
CpAls <- function(tensor, rank, max_iter = 500, tol = 1e-12) {
  T3 <- .std_array(tensor)
  d <- dim(T3)
  X0 <- .std_unfold(T3, 1)
  X1 <- .std_unfold(T3, 2)
  X2 <- .std_unfold(T3, 3)
  A <- .std_lead(X0, rank)
  B <- .std_lead(X1, rank)
  C <- .std_lead(X2, rank)
  norm2 <- sum(T3^2)
  kr <- function(P, Q) {
    out <- matrix(0, nrow(P) * nrow(Q), rank)
    for (r in seq_len(rank)) out[, r] <- as.numeric(t(outer(P[, r], Q[, r])))
    out
  }
  upd <- function(Xn, P, Q) Xn %*% kr(P, Q) %*% solve(crossprod(P) * crossprod(Q))
  fit_old <- -1
  fit <- 0
  it <- 0
  for (it in seq_len(max_iter)) {
    A <- upd(X0, C, B)
    B <- upd(X1, C, A)
    C <- upd(X2, B, A)
    inner <- 0
    for (r in seq_len(rank)) inner <- inner + sum(A[, r] * (X0 %*% as.numeric(t(outer(C[, r], B[, r])))))
    nh <- sum(crossprod(A) * crossprod(B) * crossprod(C))
    fit <- 1 - sqrt(max(norm2 - 2 * inner + nh, 0)) / sqrt(norm2)
    if (abs(fit - fit_old) < tol) break
    fit_old <- fit
  }
  lam <- numeric(rank)
  for (r in seq_len(rank)) {
    na <- sqrt(sum(A[, r]^2))
    nb <- sqrt(sum(B[, r]^2))
    nc <- sqrt(sum(C[, r]^2))
    sa <- if (A[which.max(abs(A[, r])), r] >= 0) 1 else -1
    sb <- if (B[which.max(abs(B[, r])), r] >= 0) 1 else -1
    A[, r] <- sa * A[, r] / na
    B[, r] <- sb * B[, r] / nb
    C[, r] <- sa * sb * C[, r] / nc
    lam[r] <- na * nb * nc
  }
  o <- order(-lam)
  list(weights = lam[o], A = A[, o, drop = FALSE], B = B[, o, drop = FALSE], C = C[, o, drop = FALSE], fit = fit,
       iterations = it)
}

.std_array <- function(x) {
  if (is.array(x) && length(dim(x)) == 3) return(x * 1)
  I <- length(x)
  J <- length(x[[1]])
  K <- length(x[[1]][[1]])
  a <- array(0, c(I, J, K))
  for (i in seq_len(I)) for (j in seq_len(J)) a[i, j, ] <- as.numeric(x[[i]][[j]])
  a
}

.std_unfold <- function(T3, mode) {
  d <- dim(T3)
  if (mode == 1) return(matrix(T3, d[1]))
  if (mode == 2) return(t(matrix(aperm(T3, c(1, 3, 2)), d[1] * d[3])))
  t(matrix(aperm(T3, c(1, 2, 3)), d[1] * d[2]))
}

.std_eig_desc <- function(M) {
  e <- eigen(M, symmetric = TRUE)
  V <- e$vectors
  for (i in seq_len(ncol(V))) if (V[which.max(abs(V[, i])), i] < 0) V[, i] <- -V[, i]
  list(values = e$values, vectors = V)
}

.std_lead <- function(M, r) {
  V <- .std_eig_desc(tcrossprod(M))$vectors
  out <- matrix(0, nrow(M), r)
  k <- min(r, ncol(V))
  out[, seq_len(k)] <- V[, seq_len(k)]
  out
}

.std_mode_t <- function(T3, U, mode) {
  d <- dim(T3)
  if (mode == 1) {
    out <- array(0, c(ncol(U), d[2], d[3]))
    for (k in seq_len(d[3])) out[, , k] <- crossprod(U, T3[, , k, drop = FALSE][, , 1])
  } else if (mode == 2) {
    out <- array(0, c(d[1], ncol(U), d[3]))
    for (k in seq_len(d[3])) out[, , k] <- T3[, , k, drop = FALSE][, , 1] %*% U
  } else {
    out <- array(0, c(d[1], d[2], ncol(U)))
    for (i in seq_len(d[1])) out[i, , ] <- T3[i, , , drop = FALSE][1, , ] %*% U
  }
  out
}

#' @rdname CpAls
#' @export
TuckerHooi <- function(tensor, ranks, max_iter = 50, tol = 1e-12) {
  T3 <- .std_array(tensor)
  U <- list(.std_lead(.std_unfold(T3, 1), ranks[1]), .std_lead(.std_unfold(T3, 2), ranks[2]),
            .std_lead(.std_unfold(T3, 3), ranks[3]))
  norm2 <- sum(T3^2)
  old <- -1
  it <- 0
  for (it in seq_len(max_iter)) {
    Y <- .std_mode_t(.std_mode_t(T3, U[[2]], 2), U[[3]], 3)
    U[[1]] <- .std_lead(.std_unfold(Y, 1), ranks[1])
    Y <- .std_mode_t(.std_mode_t(T3, U[[1]], 1), U[[3]], 3)
    U[[2]] <- .std_lead(.std_unfold(Y, 2), ranks[2])
    Y <- .std_mode_t(.std_mode_t(T3, U[[1]], 1), U[[2]], 2)
    U[[3]] <- .std_lead(.std_unfold(Y, 3), ranks[3])
    gn <- sum(.std_mode_t(Y, U[[3]], 3)^2)
    if (abs(gn - old) <= tol * norm2) break
    old <- gn
  }
  G <- .std_mode_t(.std_mode_t(.std_mode_t(T3, U[[1]], 1), U[[2]], 2), U[[3]], 3)
  list(core = G, factors = U, fit = 1 - sqrt(max(norm2 - sum(G^2), 0)) / sqrt(norm2), iterations = it)
}

#' @rdname CpAls
#' @export
MatrixFactorizationAls <- function(X, rank, l2 = 0.1, max_iter = 200, tol = 1e-12) {
  X <- as.matrix(X)
  m <- nrow(X)
  n <- ncol(X)
  obs <- !is.na(X)
  X0 <- ifelse(obs, X, 0)
  e <- .std_eig_desc(crossprod(X0))
  V <- matrix(0, n, rank)
  k <- min(rank, ncol(e$vectors))
  V[, seq_len(k)] <- sweep(e$vectors[, seq_len(k), drop = FALSE], 2, sqrt(pmax(e$values[seq_len(k)], 0)), "*")
  U <- matrix(0, m, rank)
  ridge <- function(F, rows, y) {
    Fr <- F[rows, , drop = FALSE]
    solve(crossprod(Fr) + diag(l2, rank), crossprod(Fr, y[rows]))
  }
  old <- Inf
  it <- 0
  loss <- NaN
  for (it in seq_len(max_iter)) {
    for (i in seq_len(m)) U[i, ] <- if (any(obs[i, ])) ridge(V, which(obs[i, ]), X0[i, ]) else 0
    for (j in seq_len(n)) V[j, ] <- if (any(obs[, j])) ridge(U, which(obs[, j]), X0[, j]) else 0
    loss <- sum(((X0 - tcrossprod(U, V))[obs])^2) + l2 * (sum(U^2) + sum(V^2))
    if (abs(old - loss) <= tol * max(1, loss)) break
    old <- loss
  }
  list(U = U, V = V, fitted = tcrossprod(U, V), loss = loss, iterations = it)
}

.std_step <- function(x) {
  h <- length(x) %/% 2
  o <- x[2 * seq_len(h)]
  e <- x[2 * seq_len(h) - 1]
  list(s = (o + e) / sqrt(2), d = (o - e) / sqrt(2))
}

.std_inv <- function(s, d) {
  out <- numeric(2 * length(s))
  out[2 * seq_along(s) - 1] <- (s - d) / sqrt(2)
  out[2 * seq_along(s)] <- (s + d) / sqrt(2)
  out
}

#' @rdname CpAls
#' @export
HaarDwt <- function(x, levels) {
  v <- as.numeric(x)
  W <- V <- vector("list", levels)
  for (j in seq_len(levels)) {
    st <- .std_step(v)
    v <- st$s
    W[[j]] <- st$d
    V[[j]] <- v
  }
  list(details = W, smooths = V)
}

#' @rdname CpAls
#' @export
HaarMra <- function(x, levels) {
  dw <- HaarDwt(x, levels)
  rebuild <- function(level, coefs, detail) {
    out <- if (detail) .std_inv(0 * coefs, coefs) else .std_inv(coefs, 0 * coefs)
    for (r in seq_len(level - 1)) out <- .std_inv(out, 0 * out)
    out
  }
  list(details = lapply(seq_len(levels), function(j) rebuild(j, dw$details[[j]], TRUE)),
       smooth = rebuild(levels, dw$smooths[[levels]], FALSE))
}

.std_rows <- function(M) {
  st <- lapply(seq_len(nrow(M)), function(i) .std_step(M[i, ]))
  list(s = do.call(rbind, lapply(st, `[[`, "s")), d = do.call(rbind, lapply(st, `[[`, "d")))
}

.std_cols <- function(M) {
  r <- .std_rows(t(M))
  list(s = t(r$s), d = t(r$d))
}

#' @rdname CpAls
#' @export
HaarDwt2d <- function(field, levels) {
  a <- unname(as.matrix(field)) * 1
  out <- vector("list", levels)
  for (j in seq_len(levels)) {
    r <- .std_rows(a)
    c1 <- .std_cols(r$s)
    c2 <- .std_cols(r$d)
    a <- c1$s
    out[[j]] <- list(h = c1$d, v = c2$s, d = c2$d)
  }
  list(approximation = a, levels = out)
}

.std_inv2 <- function(a, h, v, d) {
  inv_cols <- function(s, dd) {
    m <- matrix(0, 2 * nrow(s), ncol(s))
    for (j in seq_len(ncol(s))) m[, j] <- .std_inv(s[, j], dd[, j])
    m
  }
  rs <- inv_cols(a, h)
  rd <- inv_cols(v, d)
  m <- matrix(0, nrow(rs), 2 * ncol(rs))
  for (i in seq_len(nrow(rs))) m[i, ] <- .std_inv(rs[i, ], rd[i, ])
  m
}

#' @rdname CpAls
#' @export
HaarMra2d <- function(field, levels) {
  dw <- HaarDwt2d(field, levels)
  up <- function(M, level) {
    for (r in seq_len(level)) M <- .std_inv2(M, 0 * M, 0 * M, 0 * M)
    M
  }
  comps <- lapply(seq_len(levels), function(j) {
    lv <- dw$levels[[j]]
    z <- 0 * lv$h
    list(h = up(.std_inv2(z, lv$h, z, z), j - 1), v = up(.std_inv2(z, z, lv$v, z), j - 1),
         d = up(.std_inv2(z, z, z, lv$d), j - 1))
  })
  list(details = comps, smooth = up(dw$approximation, levels))
}

#' @rdname CpAls
#' @export
WaveletDetrend <- function(x, levels) {
  S <- HaarMra(x, levels)$smooth
  list(trend = S, detrended = as.numeric(x) - S)
}

#' @rdname CpAls
#' @export
HarmonicRegression <- function(y, t, period, n_harmonics = 2, trend = TRUE) {
  y <- as.numeric(y)
  t <- as.numeric(t)
  X <- matrix(1, length(t), 1)
  if (trend) X <- cbind(X, t)
  for (k in seq_len(n_harmonics)) X <- cbind(X, cos(2 * pi * k * t / period), sin(2 * pi * k * t / period))
  beta <- as.numeric(solve(crossprod(X), crossprod(X, y)))
  fitted <- as.numeric(X %*% beta)
  off <- if (trend) 2 else 1
  k <- seq_len(n_harmonics)
  cc <- beta[off + 2 * k - 1]
  dd <- beta[off + 2 * k]
  list(coefficients = beta, amplitude = sqrt(cc^2 + dd^2), phase = atan2(dd, cc), fitted = fitted,
       r2 = 1 - sum((y - fitted)^2) / sum((y - mean(y))^2))
}
