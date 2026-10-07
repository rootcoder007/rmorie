# SPDX-License-Identifier: AGPL-3.0-or-later
#' Principal components by the singular value decomposition
#'
#' X = U D V' on the centred (optionally scaled) data; components are the
#' columns of V with sign fixed so the largest-magnitude loading is positive,
#' eigenvalues D^2 / (n - 1) (ESL eqs 14.49-14.55, prcomp).
#'
#' @param X Data, n by p.
#' @param k Number of components.
#' @param center,scale Centre and scale the columns.
#' @return Named list: estimate, eigenvalues, singular_values, components (k by
#'   p), scores (n by k), explained_variance_ratio, cumulative_ratio, mean, sd,
#'   centered, scaled, n, p, k.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 14.5.1.
#' @examples
#' morie_esl_pca_svd(cbind(sin(1:20), cos(1:20), sin(1:20) + cos(2 * (1:20))), 2)$eigenvalues
#' @export
morie_esl_pca_svd <- function(X, k, center = TRUE, scale = FALSE) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (n < 2) stop("PCA needs at least 2 observations.", call. = FALSE)
  if (!(k >= 1 && k <= min(n, p))) stop(sprintf("k must lie in [1, %d]; got %d.", min(n, p), as.integer(k)), call. = FALSE)
  mu <- if (center) colMeans(X) else rep(0, p)
  Z <- sweep(X, 2, mu)
  sdv <- rep(1, p)
  if (scale) {
    sdv <- apply(Z, 2, stats::sd)
    if (any(sdv == 0)) stop("a constant column cannot be scaled; drop it or set scale=FALSE.", call. = FALSE)
    Z <- sweep(Z, 2, sdv, "/")
  }
  s <- svd(Z)
  V <- s$v[, seq_len(k), drop = FALSE]
  for (j in seq_len(k)) if (V[which.max(abs(V[, j])), j] < 0) V[, j] <- -V[, j]
  eig <- s$d^2 / (n - 1)
  ratio <- eig[seq_len(k)] / sum(eig)
  list(estimate = eig[1], eigenvalues = eig[seq_len(k)], singular_values = s$d[seq_len(k)], components = t(V),
       scores = Z %*% V, explained_variance_ratio = ratio, cumulative_ratio = cumsum(ratio), mean = mu, sd = sdv,
       centered = center, scaled = scale, n = n, p = p, k = k)
}

#' Project rows onto fitted principal components
#'
#' @param model Result of morie_esl_pca_svd.
#' @param X Rows to project.
#' @return Score matrix.
#' @examples
#' m <- morie_esl_pca_svd(cbind(sin(1:20), cos(1:20)), 1)
#' morie_esl_pca_transform(m, rbind(c(0, 1)))
#' @export
morie_esl_pca_transform <- function(model, X) {
  Z <- sweep(sweep(rbind(X), 2, model$mean), 2, model$sd, "/")
  Z %*% t(model$components)
}

#' Maximum-likelihood factor analysis by EM
#'
#' Sigma = L L' + Psi (ESL eqs 14.80-14.81) fitted to the sample covariance
#' (divisor n - 1) by the EM iteration L <- S Psi^-1 L M^-1, M = I + L' Psi^-1 L;
#' unrotated.
#'
#' @param data Data matrix.
#' @param n_factors Number of factors.
#' @param max_iter,tol EM controls (largest change in the loadings).
#' @return Named list: loadings, communalities, eigenvalues, variance_explained,
#'   n_factors, rotation.
#' @references Rubin, D. B. & Thayer, D. T. (1982). Psychometrika 47, 69-76.
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(2 * i) + 0.5 * sin(i), 0.6 * sin(i) + 0.2 * cos(5 * i))
#' factor_analysis_ml(X, 1)$communalities
#' @export
factor_analysis_ml <- function(data, n_factors = 2, max_iter = 1000, tol = 1e-6) {
  .morie_arg(data, "m")
  X <- as.matrix(data)
  S <- stats::cov(X)
  e <- eigen(S, symmetric = TRUE)
  L <- e$vectors[, seq_len(n_factors), drop = FALSE] %*% diag(sqrt(pmax(e$values[seq_len(n_factors)] - 1, 0.1)), n_factors)
  psi <- pmax(diag(S) - rowSums(L^2), 1e-6)
  for (it in seq_len(max_iter)) {
    pinv <- 1 / psi
    Mi <- solve(diag(n_factors) + crossprod(L * pinv, L))
    Ln <- S %*% (L * pinv) %*% Mi
    psin <- pmax(diag(S) - rowSums(Ln * (S %*% (L * pinv) %*% Mi)), 1e-6)
    done <- max(abs(Ln - L)) < tol
    L <- Ln
    psi <- psin
    if (done) break
  }
  list(loadings = L, communalities = rowSums(L^2), eigenvalues = e$values[seq_len(n_factors)],
       variance_explained = colSums(L^2), n_factors = n_factors, rotation = "none")
}

.esl14_lcg <- function(count, seed) {
  s <- seed
  out <- numeric(count)
  for (i in seq_len(count)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    out[i] <- (s + 0.5) / 2^32
  }
  out
}

.esl14_kl <- function(X, WH) sum(ifelse(X > 0, X * log(X / WH), 0) - X + WH)

#' Non-negative matrix factorisation
#'
#' X ~ W H with W, H >= 0 by Lee-Seung multiplicative updates: Frobenius loss,
#' or loss = "kl" for the Poisson log-likelihood of ESL eq 14.73 with the
#' updates of eq 14.74 (W then H). The default start is the same deterministic
#' LCG as the Python arm.
#'
#' @param X Non-negative matrix.
#' @param k Rank.
#' @param max_iter,tol Update controls.
#' @param seed LCG seed for the default start.
#' @param loss "frobenius" or "kl".
#' @param W0,H0 Optional starting factors.
#' @return Named list: estimate, W, H, frobenius_error, relative_error,
#'   iterations, converged, n, p, k, loss, and for "kl" kl_divergence, loglik,
#'   divergence_path.
#' @references Lee, D. D. & Seung, H. S. (2001). NIPS 13, 556-562.
#' @examples
#' X <- outer(1:6, 1:5, function(i, j) abs(sin(i * j / 3)) * 3 + (i + j) %% 3)
#' morie_esl_nmf(X, 2, loss = "kl")$kl_divergence
#' @export
morie_esl_nmf <- function(X, k, max_iter = 500, tol = 1e-10, seed = 13, loss = c("frobenius", "kl"), W0 = NULL,
                          H0 = NULL) {
  loss <- match.arg(loss)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (any(X < 0)) stop("NMF needs a non-negative matrix; found a negative entry.", call. = FALSE)
  if (!(k >= 1 && k <= min(n, p))) stop(sprintf("k must lie in [1, %d]; got %d.", min(n, p), as.integer(k)), call. = FALSE)
  sc <- if (mean(X) > 0) sqrt(mean(X) / k) else 1
  W <- if (is.null(W0)) matrix(.esl14_lcg(n * k, seed) + 0.1, n, k, byrow = TRUE) * sc else matrix(W0, n, k)
  H <- if (is.null(H0)) matrix(.esl14_lcg(k * p, seed + 1) + 0.1, k, p, byrow = TRUE) * sc else matrix(H0, k, p)
  eps <- 1e-12
  normX <- sqrt(sum(X^2))
  if (normX == 0) normX <- 1
  prev <- Inf
  converged <- FALSE
  path <- numeric(0)
  it <- 0L
  for (it in seq_len(max_iter)) {
    if (loss == "frobenius") {
      H <- H * crossprod(W, X) / (crossprod(W) %*% H + eps)
      W <- W * tcrossprod(X, H) / (W %*% tcrossprod(H) + eps)
      err <- sqrt(sum((X - W %*% H)^2))
      done <- abs(prev - err) <= tol * normX
    } else {
      W <- W * tcrossprod(X / (W %*% H + eps), H) / (matrix(1, n, p) %*% t(H) + eps)
      H <- H * crossprod(W, X / (W %*% H + eps)) / (crossprod(W, matrix(1, n, p)) + eps)
      err <- .esl14_kl(X, W %*% H)
      done <- abs(prev - err) <= tol * (1 + err)
    }
    path <- c(path, err)
    if (done) {
      converged <- TRUE
      break
    }
    prev <- err
  }
  WH <- W %*% H
  fe <- sqrt(sum((X - WH)^2))
  out <- list(estimate = fe / normX, W = W, H = H, frobenius_error = fe, relative_error = fe / normX,
              iterations = it, converged = converged, n = n, p = p, k = k, loss = loss)
  if (loss == "kl") {
    out$kl_divergence <- .esl14_kl(X, WH)
    out$loglik <- sum(X * log(WH) - WH)
    out$divergence_path <- path
  }
  out
}
