# SPDX-License-Identifier: AGPL-3.0-or-later
#' Robust regression through the minimum covariance determinant scatter
#'
#' Rousseeuw, P. J. (1984), "Least median of squares regression", Journal of
#' the American Statistical Association 79(388), 871-880, and Rousseeuw, P. J.
#' (1985), "Multivariate estimation with high breakdown point", Reidel,
#' 283-297.  The MCD criterion is the one stated in the stub docstring,
#' argmin over subsets H of size h of det(cov(X\[H\])), and it is applied here to
#' the JOINT matrix Z = \[X | y\].  That is the standard way a high-breakdown
#' scatter estimator becomes a high-breakdown regression estimator: partition
#' the MCD scatter of (X, y) and read the coefficients off the population
#' regression formula beta = Sigma_XX^-1 Sigma_Xy, alpha = mu_y - beta mu_X.
#' Applying the MCD to X alone would ignore y and could not produce a
#' regression at all, which is why y enters the criterion.
#'
#' The consistency factor cancels out of beta and alpha, since it multiplies
#' Sigma_XX and Sigma_Xy alike; it is still reported.
#'
#' The construction has an exact consequence used as this module's anchor.
#' With h = n the MCD subset is the whole sample, the scatter is the ordinary
#' sample covariance, and the formula above is then identically the ordinary
#' least squares fit with an intercept.
#'
#' @param y n responses.
#' @param X n-by-q predictor matrix WITHOUT an intercept column.
#' @param h subset size; defaults to \[(n + p + 1)/2\] with p = q + 1.
#' @param max_subsets refuse rather than enumerate more than this many.
#' @return list: estimate, coef, intercept, center, cov_raw, cov, factor,
#'   subset, h, n, p, method.
#' @keywords internal
#' @examples
#' Mcdcv(c(1, 2, 3, 4, 5), matrix(c(1, 2, 3, 4, 5), 5, 1), 5)$coef
#' @export
Mcdcv <- function(y, X, h = NULL, max_subsets = 200000) {
  yy <- .s03vec(y)
  Xm <- .s03mat(X)
  n <- length(yy)
  if (n == 0L) stop("min_covariance_determinant: y is empty")
  if (nrow(Xm) != n) stop("min_covariance_determinant: X must have one row per response")
  q <- ncol(Xm)
  if (q == 0L) stop("min_covariance_determinant: X has no columns")
  Z <- cbind(Xm, yy)
  p <- q + 1L
  hh <- if (is.null(h)) .rsmcdh(n, p) else as.integer(h)
  if (hh <= p) stop("min_covariance_determinant: h must exceed p = q + 1")
  if (hh > n) stop("min_covariance_determinant: h cannot exceed the number of observations")
  total <- .rsnchoosek(n, hh)
  if (total > max_subsets) stop(sprintf("min_covariance_determinant: %d subsets exceeds max_subsets", total))
  best_idx <- NULL
  best_det <- NULL
  for (idx in .rscombos(n, hh)) {
    mc <- .rsmeancov(Z, idx)
    d <- .rscovdet(mc$S)
    if (is.null(best_det) || d < best_det) { best_det <- d
    best_idx <- idx }
  }
  mc <- .rsmeancov(Z, best_idx)
  S <- mc$S
  mu <- mc$mu
  Sxx <- S[seq_len(q), seq_len(q), drop = FALSE]
  Sxy <- S[seq_len(q), p]
  beta <- .rslusolve(Sxx, Sxy)
  if (is.null(beta)) stop("min_covariance_determinant: the predictor scatter of the best subset is singular")
  alpha <- mu[p]
  for (a in seq_len(q)) alpha <- alpha - beta[a] * mu[a]
  c0 <- .rsconsistency(hh, n, p)
  list(estimate = best_det, coef = beta, intercept = alpha, center = mu,
       cov_raw = S, cov = S * c0, factor = c0,
       subset = as.numeric(best_idx - 1L), h = hh, n = n, p = p,
       method = "MCD of [X | y] by exhaustive enumeration; beta = Sigma_XX^-1 Sigma_Xy, alpha = mu_y - beta mu_X")
}
