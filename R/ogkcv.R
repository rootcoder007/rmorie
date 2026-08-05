# SPDX-License-Identifier: AGPL-3.0-or-later
#' Orthogonalized Gnanadesikan-Kettenring robust scatter
#'
#' The Gnanadesikan-Kettenring identity turns a covariance into a pair
#' of scale estimates,
#' \code{cov(u, v) = (sigma(u + v)^2 - sigma(u - v)^2) / 4}, so a
#' robust scale gives a robust covariance entry by entry. The catch is
#' that a matrix built this way need not be positive semi-definite,
#' which makes it useless as a scatter. Maronna & Zamar's fix is to
#' build it once, take its eigenvectors as a new basis, and rebuild the
#' scatter DIAGONALLY in that basis:
#' (1) \code{y_j = x_j / sigma_j} with \code{sigma_j} the MAD;
#' (2) \code{U_jk} by the GK identity on the standardized columns,
#'     \code{U_jj = 1};
#' (3) \code{U = E Lambda E'}, \code{z = E' y};
#' (4) \code{Gamma = diag(sigma(z_l)^2)}, \code{Sigma = A Gamma A'}
#'     with \code{A = diag(sigma) E}, positive semi-definite by
#'     construction;
#' (5) location \code{mu = A m(z)}, \code{m} the coordinatewise median.
#'
#' Determinism: MAD and medians only, and the shared sign-fixed Jacobi
#' eigensolver, so the basis is identical in both language arms.
#'
#' @param y Either the full data matrix (n by p) or, when \code{X} is
#'   given, its first column.
#' @param X Remaining columns; when supplied the data matrix is
#'   \code{[y, X]} and the robust regression coefficient
#'   \code{Sigma_xx^-1 Sigma_xy} is returned as well.
#' @return List with \code{sigma}, \code{location}, \code{scales},
#'   \code{estimate}, \code{eigenvalues}, \code{det}, \code{beta}
#'   (only when \code{X} is given), \code{n}, \code{p}.
#' @references Maronna, R. A. & Zamar, R. H. (2002). Robust estimates
#'   of location and dispersion for high-dimensional datasets.
#'   Technometrics, 44(4), 307-317. doi:10.1198/004017002188618509
#'   Gnanadesikan, R. & Kettenring, J. R. (1972). Robust estimates,
#'   residuals, and outlier detection with multiresponse data.
#'   Biometrics, 28(1), 81-124.
#' @export
Ogkcv <- function(y, X = NULL) {
  M <- if (is.null(X)) as.matrix(y) else cbind(as.numeric(y), as.matrix(X))
  n <- nrow(M)
  if (n == 0L) stop("Ogkcv: data matrix is empty")
  p <- ncol(M)
  if (n < 2L) stop("Ogkcv: need at least two observations")
  sig <- vapply(seq_len(p), function(j) .s03mad(M[, j]), 0)
  if (any(sig <= 0)) stop("Ogkcv: a column has zero robust scale")
  Y <- M / rep(sig, each = n)
  U <- diag(1, p)
  if (p > 1L) {
    for (j in seq_len(p - 1L)) {
      for (k in (j + 1L):p) {
        a <- .s03mad(Y[, j] + Y[, k])
        b <- .s03mad(Y[, j] - Y[, k])
        U[j, k] <- (a * a - b * b) / 4
        U[k, j] <- U[j, k]
      }
    }
  }
  eg <- .s03jacobi(U)
  E <- eg$vectors
  Z <- Y %*% E
  gam <- vapply(seq_len(p), function(l) .s03mad(Z[, l])^2, 0)
  med <- vapply(seq_len(p), function(l) .s03median(Z[, l]), 0)
  A <- diag(sig, p) %*% E
  S <- A %*% diag(gam, p) %*% t(A)
  mu <- as.numeric(A %*% med)
  out <- list(sigma = S, location = mu, scales = sig, estimate = S[1, 1],
              eigenvalues = eg$values, det = prod(gam) * prod(sig^2),
              n = n, p = p,
              method = "Orthogonalized Gnanadesikan-Kettenring scatter")
  if (!is.null(X) && p > 1L) {
    out$beta <- as.numeric(solve(S[-1, -1, drop = FALSE], S[-1, 1]))
  }
  do.call(.t1_result, out)
}
