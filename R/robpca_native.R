```r
# ROBPCA: robust principal components for data with outliers.
#
# Hubert, M., Rousseeuw, P. J., & Vanden Branden, K. (2005) "ROBPCA: A New
# Approach to Robust Principal Component Analysis", Technometrics 47(1),
# 64-79.

.Z975 <- 1.959963984540054

.robpca_matrix <- function(X, name = "X") {
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.numeric(X)) {
    X <- tryCatch(as.matrix(X), error = function(e) NULL)
    if (is.null(X) || !is.numeric(X))
      stop(sprintf("robpca: %s must be numeric", name))
  }
  if (is.null(dim(X)) || any(dim(X) == 0))
    stop(sprintf("robpca: %s is empty", name))
  if (any(!is.finite(X)))
    stop(sprintf("robpca: %s contains a non-finite value", name))
  storage.mode(X) <- "double"
  X
}

.robpca_eigh_desc <- function(C) {
  e <- eigen(C, symmetric = TRUE)
  ord <- order(e$values, decreasing = TRUE)
  list(values = e$values[ord],
       vectors = t(e$vectors[, ord, drop = FALSE]))
}

.robpca_det_from_chol <- function(C) {
  L <- tryCatch(chol(C), error = function(e) NULL)
  if (is.null(L)) return(0.0)
  prod(diag(L) ^ 2)
}

.robpca_mahalanobis <- function(X, mu, C) {
  invC <- tryCatch(solve(C), error = function(e) NULL)
  if (is.null(invC)) stop("singular")
  Xc <- sweep(X, 2, mu, "-")
  d2 <- rowSums((Xc %*% invC) * Xc)
  d2[d2 < 0] <- 0
  sqrt(d2)
}

univariate_mcd <- function(values, h = NULL, consistent = TRUE) {
  v <- sort(as.numeric(values))
  n <- length(v)
  if (n < 2) stop("robpca: the univariate MCD needs two values")
  if (is.null(h)) h <- (n + 2L) %/% 2L
  h <- as.integer(h)
  if (h < 2L || h > n)
    stop("robpca: h must lie in [2, n] for the univariate MCD")
  csum <- cumsum(c(0, v))
  csq <- cumsum(c(0, v * v))
  best_ss <- Inf
  best_mean <- 0
  for (i in 1:(n - h + 1L)) {
    s <- csum[i + h] - csum[i]
    q <- csq[i + h] - csq[i]
    ss <- q - s * s / h
    if (ss < best_ss) {
      best_ss <- ss
      best_mean <- s / h
    }
  }
  scale <- sqrt(max(best_ss, 0.0) / (h - 1.0))
  if (consistent && scale > 0.0) {
    a <- h / n
    denom <- pchisq(qchisq(min(a, 0.999999), 1), 3)
    if (denom > 0) scale <- scale * sqrt(a / denom)
  }
  c(loc = unname(best_mean), scale = unname(scale))
}

.robpca_directions <- function(rows, n_dirs, seed) {
  n <- nrow(rows)
  p <- ncol(rows)
  pairs <- list()
  total <- n * (n - 1L) %/% 2L
  if (total <= n_dirs) {
    for (i in 1:(n - 1L))
      for (j in (i + 1L):n)
        pairs[[length(pairs) + 1L]] <- c(i, j)
  } else {
    state <- .ghc_rng(seed)
    seen <- new.env(hash = TRUE)
    guard <- 0L
    while (length(pairs) < n_dirs && guard < 50L * n_dirs) {
      guard <- guard + 1L
      u <- .ghc_unif(state, 2L)
      i <- floor(u[1] * n) + 1L
      j <- floor(u[2] * n) + 1L
      if (i == j) next
      key <- paste(min(i, j), max(i, j), sep = ",")
      if (exists(key, envir = seen, inherits = FALSE)) next
      assign(key, TRUE, envir = seen)
      pairs[[length(pairs) + 1L]] <- c(i, j)
    }
  }
  dirs <- list()
  for (pair in pairs) {
    v <- rows[pair[1L], ] - rows[pair[2L], ]
    norm <- sqrt(sum(v * v))
    if (norm > 1e-12)
      dirs[[length(dirs) + 1L]] <- v / norm
  }
  if (length(dirs) == 0L) return(matrix(0, nrow = 0L, ncol = p))
  do.call(rbind, dirs)
}

outlyingness <- function(X, h = NULL, n_dirs = 250L, seed = 17L) {
  rows <- .robpca_matrix(X)
  n <- nrow(rows)
  if (is.null(h)) h <- (n + 2L) %/% 2L
  out <- rep(0.0, n)
  dirs <- .robpca_directions(rows, n_dirs, seed)
  for (k in seq_len(nrow(dirs))) {
    v <- dirs[k, ]
    proj <- as.numeric(rows %*% v)
    res <- univariate_mcd(proj, h = h)
    loc <- res["loc"]
    scale <- res["scale"]
    if (scale <= 1e-12)
      return(list(outl = NULL, exact_fit_direction = v))
    r <- abs(proj - loc) / scale
    out <- pmax(out, r)
  }
  list(outl = out, exact_fit_direction = NULL)
}

.robpca_c_steps <- function(rows, idx, max_iter = 100L) {
  h <- length(idx)
  cur <- idx
  X_sub <- rows[cur, , drop = FALSE]
  mu <- colMeans(X_sub)
  C <- cov(X_sub)
  detval <- .robpca_det_from_chol(C)
  for (iter in seq_len(max_iter)) {
    if (detval <= 0.0) break
    d <- tryCatch(.robpca_mahalanobis(rows, mu, C), error = function(e) NULL)
    if (is.null(d)) break
    nxt <- order(d)[1L:h]
    X_sub2 <- rows[nxt, , drop = FALSE]
    mu2 <- colMeans(X_sub2)
    C2 <- cov(X_sub2)
    det2 <- .robpca_det_from_chol(C2)
    if (det2 >= detval - 1e-15 * max(detval, 1.0)) {
      if (det2 < detval) {
        cur <- nxt; mu <- mu2; C <- C2; detval <- det2
      }
      break
    }
    cur <- nxt; mu <- mu2; C <- C2; detval <- det2
  }
  list(idx = cur, mu = mu, C = C, det = detval)
}

.robpca_fast_mcd <- function(rows, h, n_start = 250L, seed = 17L) {
  n <- nrow(rows)
  p <- ncol(rows)
  state <- .ghc_rng(seed + 1L)
  cands <- list()
  for (s in seq_len(as.integer(n_start))) {
    pick <- integer(0)
    guard <- 0L
    target <- min(p + 1L, n)
    while (length(pick) < target && guard < 100L) {
      guard <- guard + 1L
      u <- .ghc_unif(state, 1L)
      i <- floor(u[1L] * n) + 1L
      pick <- c(pick, i)
      pick <- unique(pick)
    }
    if (length(pick) < 2L) next
    sub <- sort(pick)
    res <- tryCatch({
      X_sub <- rows[sub, , drop = FALSE]
      mu <- colMeans(X_sub)
      C <- cov(X_sub)
      detval <- .robpca_det_from_chol(C)
      list(mu = mu, C = C, det = detval)
    }, error = function(e) NULL)
    if (is.null(res)) next
    if (res$det <= 0.0) {
      base <- colMeans(rows[sub, , drop = FALSE])
      d <- sqrt(rowSums((rows -
        matrix(base, nrow = n, ncol = p, byrow = TRUE)) ^ 2))
      sub <- order(d)[1L:h]
      X_sub <- rows[sub, , drop = FALSE]
      mu <- colMeans(X_sub)
      C <- cov(X_sub)
      detval <- .robpca_det_from_chol(C)
      if (detval <= 0.0) next
    }
    d <- tryCatch(.robpca_mahalanobis(rows, mu, C), error = function(e) NULL)
    if (is.null(d)) next
    cur <- order(d)[1L:h]
    for (step in 1:2) {
      X_sub <- rows[cur, , drop = FALSE]
      mu <- colMeans(X_sub)
      C <- cov(X_sub)
      if (.robpca_det_from_chol(C) <= 0.0) break
      d <- tryCatch(.robpca
