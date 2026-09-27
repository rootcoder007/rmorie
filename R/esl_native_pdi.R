# SPDX-License-Identifier: AGPL-3.0-or-later
.esl14_inv_sqrt <- function(M) {
  e <- eigen(M, symmetric = TRUE)
  if (min(e$values) <= 0) stop("matrix is not positive definite (rank-deficient data?)", call. = FALSE)
  e$vectors %*% (t(e$vectors) / sqrt(e$values))
}

.esl14_spline_eval <- function(u, f, g, t) {
  m <- length(u)
  if (t <= u[1]) {
    h <- u[2] - u[1]
    d <- (f[2] - f[1]) / h - h * (2 * g[1] + g[2]) / 6
    return(c(f[1] + d * (t - u[1]), d, 0))
  }
  if (t >= u[m]) {
    h <- u[m] - u[m - 1]
    d <- (f[m] - f[m - 1]) / h + h * (g[m - 1] + 2 * g[m]) / 6
    return(c(f[m] + d * (t - u[m]), d, 0))
  }
  k <- min(floor((t - u[1]) / (u[2] - u[1])), m - 2) + 1
  h <- u[k + 1] - u[k]
  a <- t - u[k]
  b <- u[k + 1] - t
  c((a * f[k + 1] + b * f[k]) / h - a * b / 6 * ((1 + a / h) * g[k + 1] + (1 + b / h) * g[k]),
    (f[k + 1] - f[k]) / h + ((3 * a * a - h * h) * g[k + 1] - (3 * b * b - h * h) * g[k]) / (6 * h),
    (a * g[k + 1] + b * g[k]) / h)
}

.esl14_fit_tilt <- function(s, L, penalty, widen = 1.2, max_iter = 100) {
  n <- length(s)
  ctr <- (min(s) + max(s)) / 2
  half <- (max(s) - min(s)) / 2 * widen
  grid <- ctr - half + 2 * half * (0:(L - 1)) / (L - 1)
  delta <- grid[2] - grid[1]
  bin <- pmin(L - 1, pmax(0, floor((s - grid[1] + delta / 2) / delta))) + 1
  ys <- tabulate(bin, L) / n
  base <- delta * exp(-grid^2 / 2 - 0.5 * log(2 * pi))
  g <- log((ys * n + 0.1) / n / base)
  for (it in seq_len(max_iter)) {
    mu <- base * exp(g)
    sm <- .esl9_spline_smooth(grid, g + (ys - mu) / mu, 2 * penalty, mu, full = TRUE)
    done <- max(abs(sm$fit - g)) < 1e-10
    g <- sm$fit
    if (done) break
  }
  sm
}

#' Product density ICA (ProDenICA)
#'
#' ESL Algorithm 14.3 (eqs 14.89-14.96): symmetric whitening, then alternate
#' (a) for each source a penalised Poisson fit of the log tilt g on a binned
#' grid (14.93-14.94, cubic smoothing spline by IRLS) and (b) the fixed-point
#' step a_j <- E[z g'(a_j'z)] - E[g''(a_j'z)] a_j followed by symmetric
#' orthogonalisation.
#'
#' @param X Numeric matrix, N by p.
#' @param penalty Roughness penalty lambda in (14.94).
#' @param L Grid size.
#' @param A0 Optional starting directions (columns); default the identity.
#' @param max_iter,tol Stop when 1 - min_j |a_j(old)'a_j(new)| < tol.
#' @return Named list: A, sources, unmixing, whitening, mean, negentropy,
#'   negentropy_path, iterations, converged.
#' @references Hastie, T. & Tibshirani, R. (2003). Independent components
#'   analysis through product density estimation. NIPS 15, 649-656.
#' @examples
#' set.seed(1)
#' S <- cbind(runif(300, -sqrt(3), sqrt(3)), rexp(300) - 1)
#' morie_esl_prodenica(S %*% rbind(c(1, 0.4), c(0.6, 1)))$A
#' @export
morie_esl_prodenica <- function(X, penalty = 1e-4, L = 500, A0 = NULL, max_iter = 50, tol = 1e-9) {
  X <- unname(as.matrix(X))
  n <- nrow(X)
  p <- ncol(X)
  if (n < 10 || L < 10 || penalty <= 0) stop("need N >= 10, L >= 10 and penalty > 0", call. = FALSE)
  mu <- colMeans(X)
  Xc <- sweep(X, 2, mu)
  K <- .esl14_inv_sqrt(crossprod(Xc) / n)
  Z <- Xc %*% K
  A <- if (is.null(A0)) diag(p) else as.matrix(A0)
  A <- A %*% .esl14_inv_sqrt(crossprod(A))
  path <- numeric(0)
  conv <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    S <- Z %*% A
    An <- A
    C <- 0
    for (j in seq_len(p)) {
      sm <- .esl14_fit_tilt(S[, j], L, penalty)
      ev <- vapply(S[, j], function(v) .esl14_spline_eval(sm$u, sm$f, sm$gamma, v), numeric(3))
      C <- C + mean(ev[1, ])
      An[, j] <- drop(crossprod(Z, ev[2, ])) / n - mean(ev[3, ]) * A[, j]
    }
    path <- c(path, C)
    An <- An %*% .esl14_inv_sqrt(crossprod(An))
    moved <- 1 - min(abs(colSums(A * An)))
    A <- An
    if (moved < tol) {
      conv <- TRUE
      break
    }
  }
  list(A = A, sources = Z %*% A, unmixing = K %*% A, whitening = K, mean = mu, negentropy = path[length(path)],
       negentropy_path = path, iterations = it, converged = conv)
}
