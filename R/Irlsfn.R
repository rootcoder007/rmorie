# SPDX-License-Identifier: AGPL-3.0-or-later
#' One outer iteration of iteratively reweighted least squares
#'
#' Holland and Welsch's scheme alternates a weighted least-squares solve
#' with a recomputation of the weights from scaled residuals.  This
#' function performs exactly one solve for a supplied weight vector and
#' also returns the Huber weights the next iteration would use, so a
#' caller can iterate.
#'
#' Formula: beta = (X' W X)^-1 X' W y.
#'
#' @param y Response vector.
#' @param X Covariate block; an intercept column is prepended.  May be
#'   \code{NULL} for an intercept-only fit.
#' @param weights Non-negative case weights; ones give plain OLS.
#' @return List with \code{estimate}, \code{coef}, \code{se},
#'   \code{se_coef}, \code{sigma2}, \code{wrss}, \code{next_weights},
#'   \code{scale}, \code{p}, \code{n}, \code{method}.
#' @references Holland and Welsch (1977), Robust regression using
#'   iteratively reweighted least-squares, Communications in Statistics
#'   -- Theory and Methods 6(9):813-827.
#'   \doi{10.1080/03610927708827533}
#' @export
#' @examples
#' Irlsfn(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2,
#' 3, 4, 5, 6, 7, 8))
Irlsfn <- function(y, X, weights) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("irls_solver: y is empty")
  Z <- .s03design(X, n)
  if (nrow(Z) != n) stop("irls_solver: X and y have different lengths")
  w <- if (!is.null(weights)) .s03vec(weights) else rep(1, n)
  if (length(w) != n) stop("irls_solver: weights and y have different lengths")
  if (any(w < 0)) stop("irls_solver: weights must be non-negative")
  p <- ncol(Z)
  if (n <= p) stop("irls_solver: need more observations than parameters")
  ZtWZ <- matrix(0, p, p)
  ZtWy <- numeric(p)
  for (i in seq_len(n)) {
    for (a in seq_len(p)) {
      ZtWy[a] <- ZtWy[a] + Z[i, a] * w[i] * yv[i]
      for (b in seq_len(p)) ZtWZ[a, b] <- ZtWZ[a, b] + Z[i, a] * w[i] * Z[i, b]
    }
  }
  beta <- .s03cholsolve(ZtWZ, ZtWy)
  resid <- yv - as.numeric(.s03matvec(Z, beta))
  wss <- sum(w * resid^2)
  sigma2 <- wss / (n - p)
  inv <- lapply(seq_len(p), function(j) .s03cholsolve(ZtWZ, as.numeric(seq_len(p) == j)))
  se <- vapply(seq_len(p), function(j) sqrt(sigma2 * inv[[j]][j]), 0)
  s <- .s03mad(resid)
  if (s <= 0) s <- 1
  u <- abs(resid / s)
  nxt <- ifelse(u <= 1.345, 1, 1.345 / u)
  ei <- if (p > 1L) 2L else 1L
  .t1_result(estimate = beta[ei], coef = beta, se = se[ei], se_coef = se,
             sigma2 = sigma2, wrss = wss, next_weights = nxt, scale = s,
             p = p, n = n,
             method = "beta = (X' W X)^-1 X' W y, Holland & Welsch (1977)")
}
