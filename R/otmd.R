# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ground cost that measures distance in units of the data's own spread
#'
#' Euclidean cost silently declares the coordinates equally important and
#' uncorrelated; on any real feature set that is false, and the transport
#' plan inherits the lie. Whitening by the covariance removes both the
#' scale and the correlation, so the cost is invariant to any full-rank
#' affine recoding of the data.
#'
#' Formula: \eqn{_ij = (x_i - y_j) prime Sigma^{-1} (x_i - y_j)}.
#'
#' @param X Source points, n by d.
#' @param Y Target points, m by d.
#' @param Sigma Covariance, symmetric positive definite.
#' @return List with \code{C}, \code{cost}, \code{n}, \code{m}, \code{d}.
#' @references De Maesschalck, R., Jouan-Rimbaud, D. and Massart, D. L.
#'   (2000). Chemometrics and Intelligent Laboratory Systems 50(1):1-18.
#'   \doi{10.1016/S0169-7439(99)00047-7}.
#' @export
Otmd <- function(X, Y, Sigma) {
  A <- as.matrix(X); B <- as.matrix(Y); S <- as.matrix(Sigma)
  d <- ncol(A)
  if (ncol(B) != d || nrow(S) != d || ncol(S) != d)
    stop("Sigma must be d by d and match both point clouds")
  Si <- vapply(seq_len(d), function(j) {
    e <- numeric(d); e[j] <- 1; .s03cholsolve(S, e)
  }, numeric(d))
  n <- nrow(A); m <- nrow(B)
  C <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    dv <- A[i, ] - B[j, ]
    C[i, j] <- as.numeric(t(dv) %*% Si %*% dv)
  }
  r <- .ot_emd(rep(1 / n, n), rep(1 / m, m), C)
  .t1_result(C = C, cost = r$cost, n = n, m = m, d = d,
             method = "Mahalanobis ground cost")
}
