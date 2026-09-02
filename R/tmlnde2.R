# SPDX-License-Identifier: AGPL-3.0-or-later
#' Direct effect with the mediator drawn from a fixed reference arm
#'
#' Fixing the mediator distribution at the reference arm rather than at
#' each person own counterfactual value is what makes this identified
#' when an exposure-induced confounder sits between mediator and outcome.
#' The mediator model is fitted on the reference arm and imposed on all.
#'
#' Formula: \code{E[Y(1, M_a*) - Y(0, M_a*)]}, targeted with
#' \code{H = D/g - (1 - D)/(1 - g)}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param M Mediator.
#' @param X Baseline covariates.
#' @param a_ref Arm whose mediator distribution is imposed.
#' @return List with \code{estimate}, \code{se}, \code{eps}, \code{m_shift}, \code{n}.
#' @references Vansteelandt, S. & Daniel, R. M. (2017). Epidemiology
#'   28:258-265.
#' @export
#' @examples
#' Tmlnde2(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = 5L, M = 5L, X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlnde2 <- function(y, D, M, X, a_ref = 0) {
  yv <- as.numeric(y); Dv <- as.numeric(D); Mv <- as.numeric(M); n <- length(yv)
  Xm <- cbind(1, as.matrix(X))
  ref <- which(abs(Dv - a_ref) < 0.5)
  mb <- .s4_ols(Xm[ref, , drop = FALSE], Mv[ref])$beta
  Mhat <- as.numeric(Xm %*% mb)
  qb <- .s4_ols(cbind(Dv, Mv, Xm), yv)$beta
  gb <- .s4_glmbin(Xm, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(Xm %*% gb)), 0.025, 0.975)
  H <- Dv / g - (1 - Dv) / (1 - g)
  Q <- as.numeric(cbind(Dv, Mv, Xm) %*% qb)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Q)) / den else 0
  Q1 <- as.numeric(cbind(1, Mhat, Xm) %*% qb) + eps / g
  Q0 <- as.numeric(cbind(0, Mhat, Xm) %*% qb) - eps / (1 - g)
  psi <- sum(Q1 - Q0) / n
  Qs <- Q + eps * H
  ic <- H * (yv - Qs) + Q1 - Q0 - psi
  se <- if (n > 1) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps,
             m_shift = sum(Mhat - Mv) / n, n = n,
             method = "TMLE for an interventional direct effect")
}
