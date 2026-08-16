# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared kernels for the meta-analysis modules: weighted least squares
# with its covariance, and the contrast design matrix of a treatment
# network. Both are needed by more than one module.

# Weighted least squares. cov is (X' W X)^{-1}, the model-based
# covariance -- correct when the weights really are inverse variances,
# which is the whole premise of inverse-variance meta-analysis.
#' Weighted least squares. cov is (X\' W X)^{-1}, the model-based
#'
#' covariance -- correct when the weights really are inverse variances,
#' which is the whole premise of inverse-variance meta-analysis.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param w See Usage.
#' @return A list with \code{beta}, \code{cov}, \code{A}.
#' @export
.ma_wls <- function(X, y, w) {
  X <- as.matrix(X)
  p <- ncol(X)
  A <- crossprod(X * w, X)
  beta <- as.numeric(.s03ridgesolve(A, as.numeric(crossprod(X * w, y)), 1e-12))
  cv <- vapply(seq_len(p), function(j) {
    e <- numeric(p); e[j] <- 1; as.numeric(.s03ridgesolve(A, e, 1e-12))
  }, numeric(p))
  list(beta = beta, cov = matrix(cv, p, p), A = A)
}

# Contrast design matrix of a treatment network. Treatments are sorted
# and the smallest is the reference, fixed at zero; a study comparing t2
# against t1 contributes +1 in the column of t2 and -1 in that of t1.
#' Contrast design matrix of a treatment network. Treatments are sorted
#'
#' and the smallest is the reference, fixed at zero; a study comparing
#' t2 against t1 contributes +1 in the column of t2 and -1 in that of
#' t1.
#'
#' @param design See Usage.
#' @return A list with \code{X}, \code{treats}, \code{T}.
#' @export
.ma_net_design <- function(design) {
  D <- as.matrix(design)
  if (ncol(D) != 2L)
    stop("design must have two columns: baseline, comparator")
  treats <- sort(unique(as.integer(D)))
  T <- length(treats)
  if (T < 2L) stop("a network needs at least two treatments")
  n <- nrow(D)
  X <- matrix(0, n, T - 1L)
  pb <- match(as.integer(D[, 1]), treats)
  pc <- match(as.integer(D[, 2]), treats)
  for (i in seq_len(n)) {
    if (pb[i] > 1L) X[i, pb[i] - 1L] <- X[i, pb[i] - 1L] - 1
    if (pc[i] > 1L) X[i, pc[i] - 1L] <- X[i, pc[i] - 1L] + 1
  }
  list(X = X, treats = treats, T = T)
}
