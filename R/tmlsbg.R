# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the average treatment effect inside a named subgroup
#'
#' A subgroup effect is not the full-sample effect restricted to a few
#' rows: the target changes, so the clever covariate changes with it.
#' Conditioning on \code{S = 1} divides the efficient influence function
#' by \code{P(S = 1)} and zeroes it outside the subgroup, giving
#' \code{H = I(S = 1)/P(S = 1) * \[D/g(X) - (1 - D)/(1 - g(X))\]}.
#'
#' The nuisance models are fitted on the whole sample -- the point of not
#' subsetting first -- while the target parameter is the subgroup one.
#' With \code{S} all ones this reduces exactly to the full-sample TMLE.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param subgroup 1 for subgroup membership, 0 otherwise.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_sub}, \code{n}.
#' @references Chernozhukov, V., Demirer, M., Duflo, E. &
#'   Fernandez-Val, I. (2025). Econometrica 93(4):1121-1164; van der
#'   Laan, M. J. & Rubin, D. (2006). IJB 2(1):11.
#' @export
#' @examples
#' Tmlsbg(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4,
#' 5, 6, 7, 8), subgroup = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlsbg <- function(y, D, X, subgroup) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  Sv <- as.numeric(subgroup)
  n <- length(yv)
  if (n == 0L || length(Dv) != n || length(Sv) != n)
    stop("Tmlsbg: y, D and subgroup must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlsbg: X must have one row per subject")
  ps <- sum(Sv) / n
  if (ps <= 0) stop("Tmlsbg: the subgroup is empty")
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  qb <- .s4_ols(cbind(Dv, W), yv)$beta
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Qobs <- ifelse(Dv > 0.5, Q1, Q0)
  H <- Sv / ps * (Dv / g - (1 - Dv) / (1 - g))
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1s <- Q1 + eps * Sv / (ps * g)
  Q0s <- Q0 - eps * Sv / (ps * (1 - g))
  psi <- sum(Sv * (Q1s - Q0s)) / (ps * n)
  ic <- H * (yv - Qobs - eps * H) + Sv / ps * (Q1s - Q0s - psi)
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, n_sub = sum(Sv), n = n,
             method = "TMLE for the average treatment effect within a subgroup")
}
