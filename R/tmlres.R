# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE with an estimated second-order remainder
#'
#' A first-order TMLE leaves a second-order remainder behaving like the
#' PRODUCT of the two nuisance errors: it vanishes if either model is
#' right, but when both are only approximately right at slow rates it
#' does not vanish fast enough for root-n inference.  The residual
#' estimator estimates that remainder directly with a U-statistic over
#' pairs, using a finite-dimensional projection kernel
#' \code{K_k(x, x') = phi(x)' Omega^{-1} phi(x')},
#' \code{Omega = n^{-1} sum_i phi(X_i) phi(X_i)'}, and adds it back:
#' \code{IF22 = -[n(n-1)]^{-1} sum_{i != j} a_i K_k(X_i, X_j) b_j} with
#' \code{a_i = (D_i - g_i)/[g_i(1 - g_i)]} and
#' \code{b_j = H_j (y_j - Q*_j)}.
#'
#' The basis is \code{[1, X, X^2]} elementwise; \code{k} is the tuning
#' parameter of the whole method.  When the outcome regression fits
#' exactly the residuals are zero and the correction is identically
#' zero -- the cheapest check of the sign convention.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @return List with \code{estimate}, \code{se}, \code{psi1},
#'   \code{if22}, \code{k_basis}, \code{n}.
#' @references Robins, J., Li, L., Mukherjee, R., Tchetgen Tchetgen, E.
#'   & van der Vaart, A. (2017). Annals of Statistics 45(5).
#' @export
#' @examples
#' Tmlres(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlres <- function(y, D, X) {
  yv <- as.numeric(y); Dv <- as.numeric(D); n <- length(yv)
  if (n < 3L || length(Dv) != n)
    stop("Tmlres: y and D must share one length >= 3")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlres: X must have one row per subject")
  W <- cbind(1, Xm)
  base <- .s4_tmle(yv, Dv, W)
  g <- base$g; H <- base$H; psi1 <- base$psi
  qb <- .s4_ols(cbind(Dv, W), yv)$beta
  Qobs <- as.numeric(cbind(Dv, W) %*% qb)
  resid <- yv - Qobs - base$eps * H
  phi <- cbind(1, Xm, Xm * Xm)
  k <- ncol(phi)
  Om <- crossprod(phi) / n + diag(1e-8, k)
  Oi <- solve(Om)
  a <- (Dv - g) / (g * (1 - g))
  b <- H * resid
  Kk <- phi %*% Oi %*% t(phi)
  tot <- sum(outer(a, b) * Kk) - sum(a * b * diag(Kk))
  if22 <- -tot / (n * (n - 1))
  est <- psi1 + if22
  ic <- base$ic
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = est, se = se, psi1 = psi1, if22 = if22,
             k_basis = k, n = n,
             method = "TMLE with an estimated second-order remainder")
}
