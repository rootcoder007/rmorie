# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE whose initial outcome model uses baseline covariates only
#'
#' Restricting the initial fit to pre-treatment information keeps
#' post-randomisation quantities out of Q, so the targeting step removes
#' residual imbalance on its own rather than sharing the job with an
#' adjustment that would open a collider path.
#'
#' Formula: \code{H = D/g - (1 - D)/(1 - g)},
#' \code{eps = sum H (y - Q) / sum H^2}, \code{Q* = Q + eps H},
#' \code{psi = mean\[Q*(1, W) - Q*(0, W)\]}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Pre-treatment covariates.
#' @param baseline Baseline level of the outcome.
#' @return List with \code{estimate}, \code{se}, \code{eps}, \code{n}.
#' @references Tsiatis, A. A. et al. (2008). Statistics in Medicine
#'   27:4658-4677; van der Laan, M. J. & Rubin, D. (2006). IJB 2(1):11.
#' @export
#' @examples
#' Tmlbas(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = 5L, X = c(1, 2, 3, 4, 5, 6, 7, 8), baseline = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlbas <- function(y, D, X, baseline) {
  yv <- as.numeric(y); Dv <- as.numeric(D); bl <- as.numeric(baseline)
  n <- length(yv)
  W <- cbind(1, as.matrix(X), bl)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  des <- cbind(Dv, W)
  qb <- .t1_lstsq(des, yv)$beta
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Q <- ifelse(Dv > 0.5, Q1, Q0)
  H <- Dv / g - (1 - Dv) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Q)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  Qs <- Q + eps * H
  psi <- sum(Q1s - Q0s) / n
  ic <- H * (yv - Qs) + Q1s - Q0s - psi
  se <- if (n > 1) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, n = n,
             method = "TMLE with a pre-treatment-only initial outcome model")
}
