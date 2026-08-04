# SPDX-License-Identifier: AGPL-3.0-or-later

#' CCC multivariate GARCH (Bollerslev 1990)
#'
#' Constant Conditional Correlation MGARCH on a panel of return series.
#' The conditional covariance factorises as \eqn{H_t = D_t R D_t}, with
#' \eqn{D_t = diag(\sqrt{h_{1t}}, \ldots, \sqrt{h_{kt}})} carrying the
#' time variation and \eqn{R} held constant.
#'
#' Because \eqn{R} does not move, the Gaussian likelihood separates: the
#' marginals are fitted series by series with
#' \code{\link{morie_garch_fit}}, and the maximum-likelihood \eqn{R} is
#' then simply the sample correlation of the standardised residuals. No
#' numerical search is needed for the second step. That closed form is
#' the point of the constant-correlation restriction, and it is what
#' \code{\link{morie_dcc_multivariate_garch}} relaxes.
#'
#' Native throughout; no multivariate GARCH package is used.
#'
#' @param x Numeric matrix of returns (T x k), k >= 2.
#' @return Named list with \code{R} (constant correlation matrix),
#'   \code{sigmas} (T x k conditional standard deviations),
#'   \code{conditional_variance} (T x k), \code{loglik}, \code{n},
#'   \code{k}, \code{method}.
#' @references Bollerslev T (1990). Modelling the coherence in short-run
#'   nominal exchange rates: a multivariate generalized ARCH model.
#'   \emph{Review of Economics and Statistics}, 72(3), 498-505.
#' @seealso \code{\link{morie_dcc_multivariate_garch}}
#' @examples
#' morie_ccc_multivariate_garch(x = matrix(rnorm(150), 50, 3))
#' @export
morie_ccc_multivariate_garch <- function(x) {
  X <- as.matrix(x)
  if (nrow(X) < ncol(X)) X <- t(X)
  n <- nrow(X)
  k <- ncol(X)
  if (n < 30 || k < 2) stop("Need n>=30, k>=2.")

  H <- matrix(NA_real_, n, k)
  Z <- matrix(NA_real_, n, k)
  for (j in seq_len(k)) {
    rj <- X[, j] - mean(X[, j])
    g <- morie_garch_fit(rj)
    H[, j] <- g$conditional_variance
    Z[, j] <- rj / sqrt(H[, j] + 1e-12)
  }

  R <- stats::cor(Z)
  ld <- determinant(R, logarithm = TRUE)
  # determinant() reports sign = +1 for a singular matrix, so a sign test
  # alone misses it; modulus == -Inf catches singularity before solve().
  if (ld$sign <= 0 || !is.finite(ld$modulus)) {
    stop("Standardised residuals give a singular correlation matrix.")
  }
  Rinv <- solve(R)

  # log L = -0.5 sum_t [ k log 2pi + log|H_t| + eps_t' H_t^-1 eps_t ],
  # with log|H_t| = log|R| + sum_j log h_jt and the quadratic form in z.
  quad <- rowSums((Z %*% Rinv) * Z)
  ll <- -0.5 * sum(k * log(2 * pi) + as.numeric(ld$modulus) +
    rowSums(log(H)) + quad)

  list(
    R = R,
    sigmas = sqrt(H),
    conditional_variance = H,
    loglik = ll,
    n = n, k = k,
    method = "CCC(1,1) two-step Gaussian QMLE"
  )
}
