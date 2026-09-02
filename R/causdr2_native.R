# SPDX-License-Identifier: AGPL-3.0-or-later
#
# DML2 partially linear regression (Causdr2). Bit-identical mirror of
# src/morie/fn/causdr2.py; folds reproduce the Python arm exactly via
# set.seed + sample.int.

#' Double/debiased ML for the partially linear model (DML2)
#'
#' Estimates theta in Y = theta D + g(X) + U with the Robinson
#' partialling-out Neyman-orthogonal score
#' \eqn{\psi(W; \theta, \eta) = (Y - l(X) - \theta(D - m(X)))(D - m(X))}
#' (Chernozhukov et al. 2018, Eq. 4.4), l(X) = E(Y|X), m(X) = E(D|X)
#' fit by OLS on each fold complement, and the pooled DML2 moment
#' (their Definition 3.2):
#' theta = sum(Vhat (Y - lhat)) / sum(Vhat^2) with Vhat = D - mhat.
#' The variance is the Theorem 3.2 plug-in, se = sqrt(sigma2 / n) with
#' sigma2 = mean(psi^2) / mean(Vhat^2)^2. The K folds come from a
#' seeded R sample.int shuffle, positions assigned round-robin.
#'
#' @param y Outcome, length n.
#' @param d Treatment (continuous or binary).
#' @param X Control matrix, n rows.
#' @param K Number of cross-fitting folds; K = 1 disables
#'   cross-fitting (anchoring use only).
#' @param seed Seed for the fold shuffle.
#' @return List with \code{estimate}, \code{se}, \code{K}, \code{n},
#'   \code{folds}, \code{method}.
#' @references Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo,
#'   E., Hansen, C., Newey, W. and Robins, J. (2018), Double/debiased
#'   machine learning for treatment and structural parameters, The
#'   Econometrics Journal 21(1), C1-C68, doi:10.1111/ectj.12097,
#'   Eq. 4.4, Definition 3.2, Theorem 3.2; local copy
#'   fetched-wave3/chernozhukov-etal-2018-double-debiased-machine-learning-EJ21.pdf.
#' @export
#' @examples
#' Causdr2(y = c(1, 2, 3, 4, 5, 6, 7, 8), d = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5))
Causdr2 <- function(y, d, X, K = 2L, seed = 1L) {
  yv <- as.numeric(y); dv <- as.numeric(d)
  Xa <- as.matrix(X); storage.mode(Xa) <- "double"
  n <- length(yv)
  if (nrow(Xa) != n || length(dv) != n) {
    stop("y, d, X must have matching first dimension", call. = FALSE)
  }
  K <- as.integer(K)
  if (K < 1L || K > n) stop("K must lie in 1..n", call. = FALSE)
  ## The nuisance regressions add their own intercept below, so a
  ## constant column in X makes the design singular. Without this the
  ## failure surfaces from solve() as a bare "system is exactly
  ## singular", which says nothing about which argument was wrong.
  ## Matches the Python arm's check.
  const_col <- which(apply(Xa, 2L, function(z) max(z) - min(z)) == 0)
  if (length(const_col) > 0L) {
    stop(sprintf(paste0("causdr2: column %d of X is constant; the ",
                        "nuisance regressions add their own intercept, ",
                        "so do not pass one"), const_col[1L]),
         call. = FALSE)
  }
  Dg <- cbind(1, Xa)
  if (K == 1L) {
    folds <- rep(0L, n)
  } else {
    set.seed(seed)
    perm <- sample.int(n)
    folds <- integer(n)
    folds[perm] <- (seq_len(n) - 1L) %% K
  }
  ols <- function(A, b) as.vector(solve(crossprod(A), crossprod(A, b)))
  lhat <- numeric(n); mhat <- numeric(n)
  for (k in seq_len(K) - 1L) {
    tr <- if (K > 1L) which(folds != k) else seq_len(n)
    te <- which(folds == k)
    Dtr <- Dg[tr, , drop = FALSE]
    bl <- ols(Dtr, yv[tr])
    bm <- ols(Dtr, dv[tr])
    lhat[te] <- as.vector(Dg[te, , drop = FALSE] %*% bl)
    mhat[te] <- as.vector(Dg[te, , drop = FALSE] %*% bm)
  }
  v <- dv - mhat
  ry <- yv - lhat
  denom <- sum(v * v)
  if (denom == 0) stop("treatment residual is identically zero", call. = FALSE)
  theta <- sum(v * ry) / denom
  psi <- (ry - theta * v) * v
  J <- -denom / n
  sigma2 <- mean(psi * psi) / (J * J)
  list(estimate = theta, se = sqrt(sigma2 / n), K = K, n = n,
       folds = folds + 1L,
       method = "Chernozhukov et al. (2018) DML2, partialling-out score Eq. 4.4")
}
