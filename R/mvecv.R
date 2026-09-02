# SPDX-License-Identifier: AGPL-3.0-or-later
#' Robust regression through the minimum volume ellipsoid scatter
#'
#' Rousseeuw, P. J. (1985), "Multivariate estimation with high breakdown
#' point", in Mathematical Statistics and Applications, Vol. B, Reidel,
#' 283-297.  The MVE criterion is the one stated in the stub docstring, the
#' smallest-volume ellipsoid covering h points, and it is applied here to the
#' JOINT matrix Z = \[X | y\], exactly as Mcdcv applies the MCD, so that a
#' scatter estimator becomes a regression estimator:
#' beta = Sigma_XX^-1 Sigma_Xy, alpha = mu_y - beta mu_X.  The inflation factor
#' m2 multiplies Sigma_XX and Sigma_Xy alike, so it cancels out of beta; the
#' coefficients depend only on the SHAPE of the ellipsoid, not its size.
#'
#' That cancellation, with the affine equivariance of the MVE, gives this
#' module its anchors, none of which runs through the search: adding a constant
#' to y moves alpha by exactly that constant and leaves beta untouched;
#' multiplying y by a constant multiplies both by exactly that constant; and
#' multiplying a predictor column by a constant divides its coefficient by
#' exactly that constant.
#'
#' The search itself is anchored separately, in Mvedet, against the closed-form
#' univariate MVE, the shortest interval containing h points, which is the
#' shortest-half construction of Rousseeuw (1984) Theorem 2, p. 873.
#'
#' @param y n responses.
#' @param X n-by-q predictor matrix without an intercept column.
#' @param h coverage; defaults to \[(n + p + 1)/2\] with p = q + 1.
#' @param n_starts cap on the number of (p+1)-subsets enumerated.
#' @return list: estimate, coef, intercept, center, cov, cov_raw, m2, subset,
#'   covered, h, n, p, method.
#' @keywords internal
#' @examples
#' Mvecv(c(1, 2, 3, 4, 5), matrix(c(1, 2, 3, 4, 6), 5, 1), 4)$coef
#' @export
Mvecv <- function(y, X, h = NULL, n_starts = 100000L) {
  yy <- .s03vec(y)
  Xm <- .s03mat(X)
  n <- length(yy)
  if (n == 0L) stop("min_volume_ellipsoid: y is empty")
  if (nrow(Xm) != n) stop("min_volume_ellipsoid: X must have one row per response")
  q <- ncol(Xm)
  if (q == 0L) stop("min_volume_ellipsoid: X has no columns")
  Z <- cbind(Xm, yy)
  r <- Mvedet(Z, h, n_starts)
  S <- r$cov
  mu <- r$center
  p <- q + 1L
  Sxx <- S[seq_len(q), seq_len(q), drop = FALSE]
  Sxy <- S[seq_len(q), p]
  beta <- .rslusolve(Sxx, Sxy)
  if (is.null(beta)) stop("min_volume_ellipsoid: the predictor block of the MVE scatter is singular")
  alpha <- mu[p]
  for (a in seq_len(q)) alpha <- alpha - beta[a] * mu[a]
  list(estimate = r$estimate, coef = beta, intercept = alpha, center = mu,
       cov = S, cov_raw = r$cov_raw, m2 = r$m2, subset = r$subset,
       covered = r$covered, h = r$h, n = n, p = p,
       method = "Rousseeuw (1985) MVE of [X | y]; beta = Sigma_XX^-1 Sigma_Xy, alpha = mu_y - beta mu_X")
}
