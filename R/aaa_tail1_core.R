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
  X <- as.matrix(X); y <- as.numeric(y)
  qrX <- qr(X)
  beta <- as.numeric(qr.coef(qrX, y))
  beta[is.na(beta)] <- 0
  fitted <- as.numeric(X %*% beta)
  resid <- y - fitted
  R <- qr.R(qrX)
  Rinv <- tryCatch(solve(R), error = function(e) MASS_ginv(R))
  list(beta = beta, fitted = fitted, resid = resid,
       xtxinv = Rinv %*% t(Rinv))
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
