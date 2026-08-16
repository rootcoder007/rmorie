# Moore-Penrose pseudo-inverse, MASS::ginv's rule: relative cutoff
# tol * d[1] with tol = sqrt(.Machine$double.eps), i.e. ~1.49e-8 of the
# largest singular value. Kept DISTINCT from .morie_pinv (rcond * max(s),
# rcond = 1e-15) because they are two different published conventions and
# code ported from MASS::ginv must keep MASS's answer. Mirrors
# morie.fn._array_core.ginv exactly.
#' Moore-Penrose pseudo-inverse, MASS::ginv\'s rule: relative cutoff
#'
#' tol * d[1] with tol = sqrt(.Machine$double.eps), i.e. ~1.49e-8 of the
#' largest singular value. Kept DISTINCT from .morie_pinv (rcond *
#' max(s), rcond = 1e-15) because they are two different published
#' conventions and code ported from MASS::ginv must keep MASS\'s answer.
#' Mirrors morie.fn._array_core.ginv exactly.
#'
#' @param X A matrix; passed to \code{dim}.
#' @param tol Numeric; combined arithmetically in the body.
#' @return The value of \code{%*%}.
#' @export
MASS_ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  X <- as.matrix(X)
  s <- svd(X)
  positive <- s$d > max(tol * s$d[1L], 0)
  if (!any(positive)) {
    return(array(0, dim(X)[2L:1L]))
  }
  s$v[, positive, drop = FALSE] %*%
    ((1 / s$d[positive]) * t(s$u[, positive, drop = FALSE]))
}

#' .morie_pinv
#'
#' A step of the tail1_core implementation. Called by \code{.schab_disjunctive_kriging}, \code{t3ols}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M See Usage.
#' @param rcond Numeric; combined arithmetically in the body. Defaults to \code{1e-15}.
#' @return The value of \code{%*%}.
#' @export
.morie_pinv <- function(M, rcond = 1e-15) {
  s <- svd(M)
  cutoff <- rcond * max(s$d)
  dinv <- ifelse(s$d > cutoff, 1 / s$d, 0)
  s$v %*% diag(dinv, length(dinv)) %*% t(s$u)
}

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

#' .t1_vec
#'
#' A step of the tail1_core implementation. Called by \code{.ecfp_percol}, \code{Admmlasso}, \code{Advielbo} and 154 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.t1_vec <- function(x) as.numeric(unlist(x))

#' .t1_mat
#'
#' A step of the tail1_core implementation. Called by \code{.ecfp_bonds}, \code{Admmlasso}, \code{Advielbo} and 45 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @return A matrix, from \code{matrix}.
#' @export
.t1_mat <- function(X) {
  if (is.matrix(X)) return(matrix(as.numeric(X), nrow = nrow(X)))
  if (is.data.frame(X)) return(as.matrix(X))
  matrix(as.numeric(X), ncol = 1L)
}

#' .t1_eigsym
#'
#' A step of the tail1_core implementation. Called by \code{Clrpca}, \code{Eigcent}, \code{Lapeig} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{t}.
#' @return A list with \code{values}, \code{vectors}.
#' @export
.t1_eigsym <- function(A) {
  e <- eigen((A + t(A)) / 2, symmetric = TRUE)
  V <- e$vectors
  for (j in seq_len(ncol(V))) {
    k <- which.max(abs(V[, j]))
    if (V[k, j] < 0) V[, j] <- -V[, j]
  }
  list(values = e$values, vectors = V)
}

#' Minimum-norm least squares via the SVD, matching numpy.linalg.lstsq
#'
#' and so the Python arm\'s _lstsq.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y A matrix; passed to \code{crossprod}.
#' @return A list with \code{beta}, \code{fitted}, \code{resid}, \code{xtxinv}.
#' @export
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


#' .t1_hatdiag
#'
#' A step of the tail1_core implementation. Called by \code{Dfbetas}, \code{Dffitsols}, \code{Olsnormeq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param xtxinv A matrix; passed to \code{\%*\%}.
#' @return The value of \code{rowSums}.
#' @export
.t1_hatdiag <- function(X, xtxinv) {
  X <- as.matrix(X)
  rowSums((X %*% xtxinv) * X)
}

#' .t1_cbind1
#'
#' A step of the tail1_core implementation. Called by \code{Bayeslogit}, \code{Dfbetas}, \code{Dffitsols} and 9 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @return The value of \code{cbind}.
#' @export
.t1_cbind1 <- function(X) cbind(1, as.matrix(X))

#' .t1_sd
#'
#' A step of the tail1_core implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{stats::sd}.
#' @export
.t1_sd <- function(x) stats::sd(as.numeric(x))

# Lehmer minstd -- identical stream to the Python arm.
#' Lehmer minstd -- identical stream to the Python arm
#'
#' A step of the tail1_core implementation. Called by \code{.btdir_rows}, \code{.btmbb_reps}, \code{.kvmse_rotation} and 32 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return The value of \code{e}, as built in the body.
#' @export
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

#' .t1_result
#'
#' A step of the tail1_core implementation. Called by \code{Admixq}, \code{Admmlasso}, \code{Advielbo} and 735 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ... Passed through.
#' @return The value of \code{out}, as built in the body.
#' @export
.t1_result <- function(...) {
  out <- list(...)
  class(out) <- c("morie_rich_result", "list")
  out
}
