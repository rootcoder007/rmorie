# SPDX-License-Identifier: AGPL-3.0-or-later
#' Quantile regression with a Dirichlet-process scale mixture
#'
#' Kottas & Gelfand's point is that fixing the error distribution to a
#' single asymmetric Laplace is stronger than the quantile restriction
#' requires; the tau-quantile restriction is preserved by any scale
#' mixture, so a Dirichlet process is placed on the mixing distribution
#' of the scale. The location parameter is unaffected, which is why the
#' regression coefficient can be read off the check-loss fit,
#' \eqn{_hat = argmin sum rho_tau(y_i - x_i primeb)} with
# prime \code{rho_tau(u) = u (tau - 1{u < 0})}, and only the error model
#' changes. The fit uses the shared fixed-iteration IRLS helper, which
#' takes exactly the same path in both language arms.
#'
#' The scale part is closed form: the ALD maximum likelihood scale is
#' \code{sigma_hat = (1/n) sum rho_tau(r_i)}, and the DP mixing
#' distribution induces \code{E\[K_n\] = sum alpha / (alpha + i - 1)}
#' distinct scales a priori.
#'
#' @param y Response, length n.
#' @param X Design WITHOUT an intercept column; one is added.
#' @param tau Quantile in (0, 1).
#' @param alpha DP concentration for the scale mixture, positive.
#' @param niter IRLS iterations.
#' @param eps Residual floor in the IRLS weight.
#' @return List with \code{beta}, \code{estimate}, \code{sigma},
#'   \code{check_loss}, \code{e_k}, \code{e_k_digamma}, \code{tau},
#'   \code{alpha}, \code{n}, \code{p}.
#' @references Kottas, A. & Gelfand, A. E. (2001). Bayesian
#'   semiparametric median regression modeling. Journal of the American
#'   Statistical Association, 96(456), 1458-1468.
#'   doi:10.1198/016214501753382363
#' @export
Npbqr <- function(y, X, tau = 0.5, alpha = 1, niter = 40L, eps = 1e-3) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("Npbqr: y is empty")
  t <- as.numeric(tau)
  if (!(t > 0 && t < 1)) stop("Npbqr: tau must lie in (0, 1)")
  a <- as.numeric(alpha)
  if (a <= 0) stop("Npbqr: alpha must be positive")
  Xm <- .t1_cbind1(X)
  if (nrow(Xm) != n) stop("Npbqr: X and y have different lengths")
  p <- ncol(Xm)
  beta <- .hrz2_qirls(Xm, yv, rep(1, n), t, niter = as.integer(niter),
                      eps = as.numeric(eps))
  r <- as.numeric(yv - Xm %*% beta)
  loss <- sum(r * (t - as.numeric(r < 0)))
  ek <- sum(a / (a + seq_len(n) - 1))
  .t1_result(beta = beta, estimate = beta[1], sigma = loss / n,
             check_loss = loss, e_k = ek,
             e_k_digamma = a * (.s03digamma(a + n) - .s03digamma(a)),
             tau = t, alpha = a, n = n, p = p,
             method = "Quantile regression with a DP scale mixture (Kottas-Gelfand 2001)")
}
