# SPDX-License-Identifier: AGPL-3.0-or-later
#' Natural direct and indirect effects, both targeted
#'
#' Two nuisance fits and two fluctuations, one for the outcome and one
#' for the mediator. Targeting both buys the double robustness: a
#' mis-specified mediator model can be rescued by a correct outcome model
#' and the other way round, which a product-of-coefficients estimator
#' cannot do.
#'
#' Formula: \code{NDE = E\[Y(1, M_0) - Y(0, M_0)\]},
#' \code{NIE = E\[Y(1, M_1) - Y(1, M_0)\]}.
#'
#' @param Y Outcome.
#' @param X Binary treatment.
#' @param M Mediator.
#' @param Cc Baseline covariates.
#' @return List with \code{estimate}, \code{nie}, \code{total}, \code{se}, \code{eps}, \code{n}.
#' @references Zheng, W. & van der Laan, M. J. (2012). IJB 8(1):1-40.
#' @export
#' @examples
#' TmlMd(Y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), M = 5L, Cc = c(1,
#' 2, 3, 4, 5, 6, 7, 8))
TmlMd <- function(Y, X, M, Cc) {
  yv <- as.numeric(Y)
  Dv <- as.numeric(X)
  Mv <- as.numeric(M)
  n <- length(yv)
  W <- cbind(1, as.matrix(Cc))
  ref <- which(Dv <= 0.5)
  trt <- which(Dv > 0.5)
  m0b <- .s4_ols(W[ref, , drop = FALSE], Mv[ref])$beta
  m1b <- .s4_ols(W[trt, , drop = FALSE], Mv[trt])$beta
  M0 <- as.numeric(W %*% m0b)
  M1 <- as.numeric(W %*% m1b)
  qb <- .s4_ols(cbind(Dv, Mv, W), yv)$beta
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  H <- Dv / g - (1 - Dv) / (1 - g)
  Q <- as.numeric(cbind(Dv, Mv, W) %*% qb)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Q)) / den else 0
  Q1M0 <- as.numeric(cbind(1, M0, W) %*% qb) + eps / g
  Q0M0 <- as.numeric(cbind(0, M0, W) %*% qb) - eps / (1 - g)
  Q1M1 <- as.numeric(cbind(1, M1, W) %*% qb) + eps / g
  nde <- sum(Q1M0 - Q0M0) / n
  nie <- sum(Q1M1 - Q1M0) / n
  Qs <- Q + eps * H
  ic <- H * (yv - Qs) + Q1M0 - Q0M0 - nde
  se <- if (n > 1) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = nde, nie = nie, total = nde + nie, se = se,
             eps = eps, n = n,
             method = "TMLE for natural direct and indirect effects")
}
