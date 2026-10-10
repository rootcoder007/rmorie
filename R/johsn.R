# SPDX-License-Identifier: AGPL-3.0-or-later

# Osterwald-Lenum (1992) critical values as tabulated in urca::ca.jo():
# [r-row (1..11 = number of non-cointegrated directions), level
# (10pct, 5pct, 1pct), statistic (1 = max-eigen, 2 = trace)].
.morie_johansen_cv <- list(
  none = array(c(
    6.5, 12.91, 18.9, 24.78, 30.84, 36.25, 42.06, 48.43, 54.01, 59, 65.07,
    8.18, 14.9, 21.07, 27.14, 33.32, 39.43, 44.91, 51.07, 57, 62.42, 68.27,
    11.65, 19.19, 25.75, 32.14, 38.78, 44.59, 51.3, 57.07, 63.37, 68.61,
    74.36, 6.5, 15.66, 28.71, 45.23, 66.49, 85.18, 118.99, 151.38, 186.54,
    226.34, 269.53, 8.18, 17.95, 31.52, 48.28, 70.6, 90.39, 124.25, 157.11,
    192.84, 232.49, 277.39, 11.65, 23.52, 37.22, 55.43, 78.87, 104.2,
    136.06, 168.92, 204.79, 246.27, 292.65
  ), c(11, 3, 2)),
  const = array(c(
    7.52, 13.75, 19.77, 25.56, 31.66, 37.45, 43.25, 48.91, 54.35, 60.25,
    66.02, 9.24, 15.67, 22, 28.14, 34.4, 40.3, 46.45, 52, 57.42, 63.57,
    69.74, 12.97, 20.2, 26.81, 33.24, 39.79, 46.82, 51.91, 57.95, 63.71,
    69.94, 76.63, 7.52, 17.85, 32, 49.65, 71.86, 97.18, 126.58, 159.48,
    196.37, 236.54, 282.45, 9.24, 19.96, 34.91, 53.12, 76.07, 102.14, 131.7,
    165.58, 202.92, 244.15, 291.4, 12.97, 24.6, 41.07, 60.16, 84.45, 111.01,
    143.09, 177.2, 215.74, 257.68, 307.64
  ), c(11, 3, 2)),
  trend = array(c(
    10.49, 16.85, 23.11, 29.12, 34.75, 40.91, 46.32, 52.16, 57.87, 63.18,
    69.26, 12.25, 18.96, 25.54, 31.46, 37.52, 43.97, 49.42, 55.5, 61.29,
    66.23, 72.72, 16.26, 23.65, 30.34, 36.65, 42.36, 49.51, 54.71, 62.46,
    67.88, 73.73, 79.23, 10.49, 22.76, 39.06, 59.14, 83.2, 110.42, 141.01,
    176.67, 215.17, 256.72, 303.13, 12.25, 25.32, 42.44, 62.99, 87.31, 114.9,
    146.76, 182.82, 222.21, 263.42, 310.81, 16.26, 30.45, 48.45, 70.05,
    96.58, 124.75, 158.49, 196.08, 234.41, 279.07, 327.45
  ), c(11, 3, 2))
)

# Native Johansen reduced-rank regression (Johansen 1988, 1991) written
# to reproduce urca::ca.jo() (no season / dumvar) step for step:
# VECM in levels-lag-K ("longrun") or lag-1 ("transitory") form,
# deterministic terms by `ecdet`, concentrated moment matrices, the
# Cholesky-whitened symmetric eigenproblem, and eigenvectors normalised
# so the first row is 1.  Test statistics are returned in urca's order
# (H0: r <= P-1 first, r = 0 last).
.morie_johansen_native <- function(x, K = 2L, ecdet = c("none", "const", "trend"),
                                   spec = c("longrun", "transitory")) {
  ecdet <- match.arg(ecdet)
  spec <- match.arg(spec)
  x <- as.matrix(x)
  if (is.null(colnames(x))) colnames(x) <- paste0("y", seq_len(ncol(x)))
  colnames(x) <- make.names(colnames(x))
  K <- as.integer(K)
  if (K < 2L) stop("K must be at least 2.", call. = FALSE)
  if (anyNA(x)) stop("x contains missing values.", call. = FALSE)
  P <- ncol(x)
  arrsel <- P
  N <- nrow(x)
  if (N * P < P + K * P^2 + P * (P + 1) / 2) {
    stop("Insufficient degrees of freedom.", call. = FALSE)
  }
  Z <- stats::embed(diff(x), K)
  Z0 <- Z[, 1:P, drop = FALSE]
  lagged <- Z[, -c(1:P), drop = FALSE]
  trend <- seq_len(N)
  if (spec == "longrun") {
    keep <- -c((N - K + 1):N)
    ZK <- x[keep, , drop = FALSE]
    tK <- trend[keep]
    Lnot <- K
  } else {
    ZK <- x[-N, , drop = FALSE][K:(N - 1), , drop = FALSE]
    tK <- trend[-N][K:(N - 1)]
    Lnot <- 1L
  }
  knames <- paste0(colnames(x), ".l", Lnot)
  if (ecdet == "const") {
    ZK <- cbind(ZK, 1)
    knames <- c(knames, "constant")
    Z1 <- lagged
    P <- P + 1L
    idx <- 0:(P - 2L)
  } else if (ecdet == "none") {
    Z1 <- cbind(1, lagged)
    idx <- 0:(P - 1L)
  } else {
    ZK <- cbind(ZK, tK)
    knames <- c(knames, paste0("trend.l", Lnot))
    Z1 <- cbind(1, lagged)
    P <- P + 1L
    idx <- 0:(P - 2L)
  }
  colnames(ZK) <- knames
  N <- nrow(Z0)
  M00 <- crossprod(Z0) / N
  M11 <- crossprod(Z1) / N
  MKK <- crossprod(ZK) / N
  M01 <- crossprod(Z0, Z1) / N
  M0K <- crossprod(Z0, ZK) / N
  MK0 <- crossprod(ZK, Z0) / N
  M10 <- crossprod(Z1, Z0) / N
  M1K <- crossprod(Z1, ZK) / N
  MK1 <- crossprod(ZK, Z1) / N
  M11inv <- solve(M11)
  S00 <- M00 - M01 %*% M11inv %*% M10
  S0K <- M0K - M01 %*% M11inv %*% M1K
  SK0 <- MK0 - MK1 %*% M11inv %*% M10
  SKK <- MKK - MK1 %*% M11inv %*% M1K
  Ctemp <- chol(SKK, pivot = TRUE)
  oo <- order(attr(Ctemp, "pivot"))
  C <- t(Ctemp[, oo])
  Cinv <- solve(C)
  S00inv <- solve(S00)
  valeigen <- eigen(Cinv %*% SK0 %*% S00inv %*% S0K %*% t(Cinv))
  lambda <- valeigen$values
  Vorg <- t(Cinv) %*% valeigen$vectors
  V <- sapply(seq_len(P), function(j) Vorg[, j] / Vorg[1, j])
  V <- matrix(V, nrow = nrow(Vorg))
  dimnames(V) <- dimnames(Vorg) <- list(knames, knames)
  W <- S0K %*% V %*% solve(t(V) %*% SKK %*% V)
  PI <- S0K %*% solve(SKK)
  dimnames(W) <- dimnames(PI) <- list(paste0(colnames(x), ".d"), knames)
  trace <- rev(vapply(idx, function(r) -N * sum(log(1 - lambda[(r + 1):P])),
                      numeric(1)))
  maxeig <- rev(vapply(idx, function(r) -N * log(1 - lambda[r + 1]),
                       numeric(1)))
  cv_tab <- function(j) {
    if (arrsel > 11) return(NULL)
    cv <- round(.morie_johansen_cv[[ecdet]][1:arrsel, , j, drop = FALSE], 2)
    cv <- matrix(cv, nrow = arrsel)
    dimnames(cv) <- list(c(paste0("r <= ", (arrsel - 1):1, " |")[seq_len(arrsel - 1)],
                           "r = 0  |"),
                         c("10pct", "5pct", "1pct"))
    cv
  }
  list(lambda = lambda, trace = trace, maxeig = maxeig,
       cval_trace = cv_tab(2L), cval_eigen = cv_tab(1L),
       V = V, Vorg = Vorg, W = W, PI = PI, N = N, K = K,
       ecdet = ecdet, spec = spec)
}

#' Johansen trace test for cointegration
#'
#' Native Johansen (1988, 1991) reduced-rank regression on a VECM with
#' \code{K = max(k_ar_diff + 1, 2)} lags in levels, an unrestricted
#' constant (\code{ecdet = "none"}) and the long-run parameterisation.
#' Computed with rmorie's own code, which reproduces
#' \code{urca::ca.jo(x, type = "trace", ecdet = "none", K = max(k_ar_diff
#' + 1, 2))} (statistics, eigenvalues and normalised eigenvectors) to
#' rounding error; no external package is used.  Critical values are the
#' Osterwald-Lenum (1992) table used by urca (available for up to 11
#' series).
#'
#' @param x Numeric matrix (T x k) of I(1) candidate series.
#' @param k_ar_diff Number of lagged differences. Default 1.
#' @return Named list with \code{eigenvalues, trace_stat, crit_values,
#'   rank, n, k, method}.  \code{trace_stat} and the rows of
#'   \code{crit_values} (columns \code{10pct, 5pct, 1pct}) are ordered as
#'   in urca: H0 \eqn{r \le k-1} first, \eqn{r = 0} last; \code{rank} is
#'   the number of trace statistics exceeding their 5 percent critical
#'   value.  The list carries an attribute \code{"johansen"} holding the
#'   maximal-eigenvalue statistics (\code{max_eigen_stat},
#'   \code{max_eigen_crit}), the eigenvectors normalised on the first
#'   variable (\code{eigenvectors}, urca's \code{V}) and unnormalised
#'   (\code{eigenvectors_raw}, urca's \code{Vorg}), the loading matrix
#'   \code{W}, \code{PI}, and the lag order \code{K}.
#' @references Johansen, S. (1988). Statistical analysis of cointegration
#'   vectors. Journal of Economic Dynamics and Control 12, 231-254.
#'   Osterwald-Lenum, M. (1992). Oxford Bulletin of Economics and
#'   Statistics 54, 461-472.
#' @examples
#' set.seed(1)
#' w <- cumsum(rnorm(100))
#' Y <- cbind(w + rnorm(100), 0.5 * w + rnorm(100))
#' morie_johansen_cointegration(Y)$trace_stat
#' @export
morie_johansen_cointegration <- function(x, k_ar_diff = 1) {
  Y <- as.matrix(x)
  if (nrow(Y) < ncol(Y)) Y <- t(Y)
  Tt <- nrow(Y)
  k <- ncol(Y)
  if (Tt < 20 || k < 2) stop("Need T>=20, k>=2.")
  if (is.null(colnames(Y))) colnames(Y) <- paste0("y", seq_len(k))
  K <- max(k_ar_diff + 1, 2)
  jo <- .morie_johansen_native(Y, K = K, ecdet = "none", spec = "longrun")
  cval <- jo$cval_trace
  rank <- if (is.null(cval)) NA_integer_ else
    sum(jo$trace > cval[, "5pct"])
  out <- list(
    eigenvalues = jo$lambda,
    trace_stat = jo$trace,
    crit_values = cval,
    rank = rank,
    n = Tt, k = k,
    method = "Johansen trace test (native reduced-rank regression)"
  )
  attr(out, "johansen") <- list(
    max_eigen_stat = jo$maxeig, max_eigen_crit = jo$cval_eigen,
    eigenvectors = jo$V, eigenvectors_raw = jo$Vorg,
    W = jo$W, PI = jo$PI, K = jo$K, ecdet = jo$ecdet, spec = jo$spec
  )
  out
}
