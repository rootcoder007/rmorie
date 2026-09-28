#' Eigenvector spatial filtering
#'
#' \code{MoranEigenvectors}: Moran eigenvector maps, the eigenvectors of the
#' doubly centred symmetrised weights computed in the Helmert basis of the
#' complement of the constant, with Moran's I \code{(n/S0) lambda} and the
#' positive/negative sets. \code{EigenvectorFiltering}: forward selection of
#' eigenvectors by residual Moran's I (Tiefelsdorf and Griffith 2007), AIC,
#' leave-one-out prediction error or R-squared gain, keeping at least two
#' residual degrees of freedom. \code{GetisFilter}: Getis
#' (1995) filtering. Identical to the Python arm \code{morie.fn.sfilter}
#' (eigenvector indices are 1-based here).
#'
#' @param W Spatial weights matrix.
#' @param y Response.
#' @param X Optional covariate matrix (an intercept is added).
#' @param criterion \code{"moran"}, \code{"aic"}, \code{"press"} or \code{"r2"}.
#' @param candidates \code{"positive"}, \code{"negative"} or \code{"all"}.
#' @param tol Stopping tolerance.
#' @param max_vectors Maximum number of eigenvectors.
#' @param x Positive variable to filter.
#' @return List.
#' @references Griffith, D. A. (2003). Spatial Autocorrelation and Spatial
#'   Filtering. Springer.
#'
#'   Tiefelsdorf, M. and Griffith, D. A. (2007). Semiparametric filtering of
#'   spatial autocorrelation: the eigenvector approach. Environment and
#'   Planning A 39, 1193-1221.
#'
#'   Getis, A. (1995). Spatial filtering in a regression framework. In L.
#'   Anselin and R. Florax (eds), New Directions in Spatial Econometrics,
#'   Springer, 172-185.
#' @examples
#' MoranEigenvectors(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))$moran_i
#' GetisFilter(c(2, 4, 6), rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))$filtered
#' @export
MoranEigenvectors <- function(W) {
  A <- as.matrix(W)
  n <- nrow(A)
  S <- (A + t(A)) / 2
  s0 <- sum(A)
  Q <- matrix(0, n, n - 1)
  for (k in seq_len(n - 1)) {
    cc <- 1 / sqrt(k * (k + 1))
    Q[seq_len(k), k] <- cc
    Q[k + 1, k] <- -k * cc
  }
  e <- eigen(t(Q) %*% S %*% Q, symmetric = TRUE)
  V <- Q %*% e$vectors
  for (k in seq_len(ncol(V))) {
    piv <- V[which(abs(V[, k]) > 1e-12)[1], k]
    if (piv < 0) V[, k] <- -V[, k]
  }
  mi <- n / s0 * e$values
  e0 <- -1 / (n - 1)
  list(eigenvalues = e$values, vectors = V, moran_i = mi, positive = which(mi > e0), negative = which(mi < e0))
}

.sf_moran <- function(e, A) {
  d <- e - mean(e)
  length(e) / sum(A) * sum(A * outer(d, d)) / sum(d^2)
}

#' @rdname MoranEigenvectors
#' @export
EigenvectorFiltering <- function(y, W, X = NULL, criterion = "moran", candidates = "positive", tol = 0.1,
                                 max_vectors = NULL) {
  A <- as.matrix(W)
  n <- length(y)
  Xb <- if (is.null(X)) matrix(1, n, 1) else cbind(1, as.matrix(X))
  me <- MoranEigenvectors(A)
  pool <- switch(candidates, positive = me$positive, negative = me$negative, all = seq_along(me$eigenvalues))
  E <- me$vectors
  tss <- sum((y - mean(y))^2)
  fit <- function(sel) {
    X2 <- cbind(Xb, E[, sel, drop = FALSE])
    XtX <- crossprod(X2)
    beta <- solve(XtX, crossprod(X2, y))
    res <- as.vector(y - X2 %*% beta)
    rss <- sum(res^2)
    h <- rowSums((X2 %*% solve(XtX)) * X2)
    list(beta = as.vector(beta), res = res, rss = rss, aic = n * log(rss / n) + 2 * (ncol(X2) + 1),
         r2 = 1 - rss / tss, I = .sf_moran(res, A), press = sum((res / (1 - h))^2))
  }
  base <- fit(integer(0))
  cur <- base
  sel <- integer(0)
  limit <- if (is.null(max_vectors)) length(pool) else min(max_vectors, length(pool))
  limit <- min(limit, n - ncol(Xb) - 2)
  while (length(sel) < limit) {
    if (criterion == "moran" && abs(cur$I) < tol) break
    cands <- setdiff(pool, sel)
    fits <- lapply(cands, function(k) fit(c(sel, k)))
    key <- vapply(fits, function(f) switch(criterion, moran = abs(f$I), aic = f$aic, press = f$press, r2 = -f$r2), 0)
    b <- which(key == min(key))
    b <- b[which.min(cands[b])]
    f <- fits[[b]]
    if (criterion == "aic" && f$aic >= cur$aic) break
    if (criterion == "press" && f$press >= cur$press) break
    if (criterion == "r2" && f$r2 - cur$r2 < tol) break
    sel <- c(sel, cands[b])
    cur <- f
  }
  list(selected = sel, coefficients = cur$beta, residual_moran = cur$I, aic = cur$aic, r2 = cur$r2,
       filter_r2 = cur$r2 - base$r2, residuals = cur$res)
}

#' @rdname MoranEigenvectors
#' @export
GetisFilter <- function(x, W) {
  A <- as.matrix(W)
  diag(A) <- 0
  n <- length(x)
  wi <- rowSums(A)
  g <- as.vector(A %*% x) / (sum(x) - x)
  f <- ifelse(g > 0, x * (wi / (n - 1)) / g, NaN)
  list(filtered = f, spatial = x - f)
}
