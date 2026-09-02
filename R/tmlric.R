# SPDX-License-Identifier: AGPL-3.0-or-later
#' Targeted maximum likelihood for a rare binary outcome
#'
#' A sample drawn on a rare outcome carries no information about how
#' rare it is, so the estimator has to be told. Known-prevalence
#' case-control weights enter both the initial fit and the targeting
#' step. IRLS runs a fixed number of iterations and the fluctuation is
#' the closed-form linear one, so nothing here depends on a tolerance.
#'
#' Formula: \code{H(D, X) = D / g(X) - (1 - D) / (1 - g(X))};
#' \code{Q* = Q + eps H} with
#' \code{eps = sum w H (y - Q) / sum w H^2}; \code{psi} is the weighted
#' mean of \code{Q*(1, X) - Q*(0, X)}.
#'
#' @param y Binary outcome.
#' @param D Binary treatment.
#' @param X Baseline covariates; an intercept is added.
#' @param prevalence Known population prevalence of \code{y = 1}.
#' @return List with \code{estimate}, \code{se}, \code{eps}, \code{n}.
#' @references Tran, L., Petersen, M., Schwab, J. & van der Laan, M. J.
#'   (2018). Robust variance estimation and inference for causal effect
#'   estimation. arXiv:1810.03030. Case-control weights follow van der
#'   Laan (2008), IJB 4(1):17.
#' @export
#' @examples
#' Tmlric(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = 5L, X = c(1, 2, 3, 4, 5, 6, 7, 8), prevalence = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlric <- function(y, D, X, prevalence) {
  y <- as.numeric(y)
  D <- as.numeric(D)
  n <- length(y)
  Xm <- cbind(1, as.matrix(X))
  q0 <- as.numeric(prevalence)
  n1 <- sum(y > 0.5)
  n0 <- n - n1
  w <- ifelse(y > 0.5, q0 * n / n1, (1 - q0) * n / n0)
  gb <- .s4_glmbin(Xm, D)
  g <- .s4_clip(.s4_expit(as.numeric(Xm %*% gb)), 0.01, 0.99)
  qdes <- cbind(D, Xm)
  qb <- .s4_glmbin(qdes, y)
  qhat <- function(d) .s4_clip(.s4_expit(as.numeric(cbind(d, Xm) %*% qb)), 1e-6, 1 - 1e-6)
  Q <- qhat(D)
  Q1 <- qhat(rep(1, n))
  Q0 <- qhat(rep(0, n))
  H <- D / g - (1 - D) / (1 - g)
  den <- sum(w * H * H)
  eps <- if (den != 0) sum(w * H * (y - Q)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  Qs <- Q + eps * H
  sw <- sum(w)
  psi <- sum(w * (Q1s - Q0s)) / sw
  ic <- w * (H * (y - Qs) + Q1s - Q0s - psi) / (sw / n)
  se <- if (n > 1) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, n = n,
             method = "Case-control-weighted TMLE, rare outcome")
}
