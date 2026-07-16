# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native replacements for the MASS utilities used across the package
# (feat/native-specializations, module 30). MASS::ginv (Moore-Penrose
# pseudo-inverse, 54 call sites) and MASS::mvrnorm (multivariate normal
# sampling) are reproduced exactly -- same SVD / eigen algorithm and, for
# mvrnorm, the same RNG consumption order, so results match MASS to
# machine precision (and bit-for-bit under a common seed).

#' Internal: Moore-Penrose generalized inverse (native MASS::ginv)
#'
#' Exact re-implementation of \code{MASS::ginv} for real matrices: the
#' SVD pseudo-inverse with the same singular-value tolerance rule.
#' @noRd
.morie_ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  if (length(dim(X)) > 2L || !is.numeric(X)) {
    stop("'X' must be a numeric matrix", call. = FALSE)
  }
  if (!is.matrix(X)) X <- as.matrix(X)
  s <- svd(X)
  pos <- s$d > max(tol * s$d[1L], 0)
  if (all(pos)) {
    s$v %*% (1 / s$d * t(s$u))
  } else if (!any(pos)) {
    array(0, dim(X)[2L:1L])
  } else {
    s$v[, pos, drop = FALSE] %*%
      ((1 / s$d[pos]) * t(s$u[, pos, drop = FALSE]))
  }
}

#' Native multivariate normal sampling (reproduces MASS::mvrnorm)
#'
#' Draws from \eqn{N(\mu, \Sigma)} using the symmetric eigen
#' decomposition, matching \code{MASS::mvrnorm} exactly -- including its
#' RNG consumption (\code{rnorm(p * n)} in the same order), so under a
#' common seed the draws are identical.
#'
#' @param n Number of samples.
#' @param mu Mean vector (length p).
#' @param Sigma p x p covariance matrix.
#' @param tol Tolerance for the positive-definiteness check.
#' @param empirical If TRUE, force the sample mean/covariance to match
#'   \code{mu}/\code{Sigma} exactly (as in MASS).
#' @return A length-p vector when \code{n == 1}, else an \code{n x p}
#'   matrix.
#' @references Venables, W. N., & Ripley, B. D. (2002). \emph{Modern
#'   Applied Statistics with S}. Springer.
#' @examples
#' set.seed(1)
#' morie_mvrnorm(3, mu = c(0, 0), Sigma = diag(2))
#' @export
morie_mvrnorm <- function(n = 1, mu, Sigma, tol = 1e-6,
                          empirical = FALSE) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) {
    stop("'Sigma' is not positive definite")
  }
  X <- matrix(stats::rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) drop(X) else t(X)
}
