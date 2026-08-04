# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shared numeric helpers for the tail1 batch
#'
#' Internal only. These mirror \code{morie.fn._tail1core} on the Python
#' side so the two arms can be compared value-for-value. Base R already
#' has the linear algebra and the distribution functions, so the mirror
#' is mostly a naming shim; the parts that are not (sign-fixed
#' eigenvectors, the minstd stream) are the parts that decide whether
#' cross-language parity holds at all.
#'
#' @name tail1_core
#' @keywords internal
NULL

.t1_vec <- function(x) as.numeric(unlist(x))

.t1_mat <- function(X) {
  if (is.matrix(X)) return(matrix(as.numeric(X), nrow = nrow(X)))
  if (is.data.frame(X)) return(as.matrix(X))
  matrix(as.numeric(X), ncol = 1L)
}

.t1_eigsym <- function(A) {
  e <- eigen((A + t(A)) / 2, symmetric = TRUE)
  V <- e$vectors
  for (j in seq_len(ncol(V))) {
    k <- which.max(abs(V[, j]))
    if (V[k, j] < 0) V[, j] <- -V[, j]
  }
  list(values = e$values, vectors = V)
}

.t1_lstsq <- function(X, y) {
  # Minimum-norm least squares via the SVD, matching numpy.linalg.lstsq
  # and so the Python arm's _lstsq.
  #
  # This used qr.coef with the NA coefficients of aliased columns mapped
  # to 0 -- a basic solution, not the minimum-norm one. Both minimise the
  # residual (identical 0.6892 on a design whose third column is the sum
  # of the first two), but they are different points of the solution set,
  # so the arms disagreed outright. Worse, the basic solution depends on
  # column order: permuting that design to (3, 1, 2) moved the answer
  # from (0.55, 1.35, 0) to (1.35, -0.80, 0). The minimum-norm solution
  # is unique and permutation-invariant.
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); k <- ncol(X)
  sv <- svd(X)
  eps <- .Machine$double.eps
  cut <- if (length(sv$d)) max(sv$d) * eps * max(n, k) else 0
  keep <- sv$d > cut
  dinv <- ifelse(keep, 1 / sv$d, 0)
  beta <- as.numeric(sv$v %*% (dinv * crossprod(sv$u, y)))
  fitted <- as.numeric(X %*% beta)
  resid <- y - fitted
  # Moore-Penrose (X'X)+ = V diag(1/d^2) V'. Both this and beta are
  # invariant to the sign convention of the singular vectors, so LAPACK's
  # choice here and ours in Python cannot make the arms differ.
  d2inv <- ifelse(keep, 1 / sv$d^2, 0)
  xtxinv <- sv$v %*% (d2inv * t(sv$v))
  list(beta = beta, fitted = fitted, resid = resid, xtxinv = xtxinv)
}

MASS_ginv <- function(M) {
  s <- svd(M)
  d <- ifelse(s$d > 1e-12, 1 / s$d, 0)
  s$v %*% diag(d, length(d)) %*% t(s$u)
}

.t1_hatdiag <- function(X, xtxinv) {
  X <- as.matrix(X)
  rowSums((X %*% xtxinv) * X)
}

.t1_cbind1 <- function(X) cbind(1, as.matrix(X))

.t1_sd <- function(x) stats::sd(as.numeric(x))

# Lehmer minstd -- identical stream to the Python arm.
.t1_lcg <- function(seed = 1) {
  s <- as.numeric(seed) %% 2147483647
  if (s <= 0) s <- 1
  e <- new.env(parent = emptyenv())
  e$s <- s
  e$unif <- function() {
    e$s <- (48271 * e$s) %% 2147483647
    e$s / 2147483647
  }
  e$norm <- function() stats::qnorm(e$unif())
  e$rademacher <- function() if (e$unif() < 0.5) 1 else -1
  e
}

.t1_result <- function(...) {
  out <- list(...)
  class(out) <- c("morie_rich_result", "list")
  out
}
