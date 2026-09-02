# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the ATE with a missing-at-random outcome
#'
#' Missingness is a second intervention node.  Writing \code{Delta = 1}
#' for an observed outcome, the joint intervention is "set \code{A = a}
#' and \code{Delta = 1}", so the clever covariate carries both
#' probabilities,
#' \code{H = Delta/pi(A, X) * \[A/g(X) - (1 - A)/(1 - g(X))\]}, with
#' \code{pi(a, X)} evaluated at the counterfactual arm.  The initial
#' outcome regression is fitted on complete cases only, and the
#' fluctuation \code{Q*(a, X) = Q(a, X) + eps/(g_a(X) pi(a, X))} uses the
#' arm-specific denominator.  With no missingness \code{pi == 1} and the
#' estimator collapses onto the standard point-treatment TMLE.
#'
#' @param y Outcome; entries with \code{missing = 1} are ignored.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param missing 1 if the outcome is missing, 0 if observed.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_obs}, \code{n}.
#' @references Rotnitzky, A., Robins, J. M. & Scharfstein, D. O. (1998).
#'   JASA 93(444):1321-1339; Bang, H. & Robins, J. M. (2005). Biometrics
#'   61(4):962-973.
#' @export
#' @examples
#' set.seed(1)
#' r <- Tmlmda(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10), missing = rnorm(10)); TRUE
Tmlmda <- function(y, D, X, missing) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  mv <- as.numeric(missing)
  n <- length(yv)
  if (n == 0L || length(Dv) != n || length(mv) != n)
    stop("Tmlmda: y, D and missing must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlmda: X must have one row per subject")
  delta <- 1 - mv
  if (sum(delta) < 2) stop("Tmlmda: fewer than two observed outcomes")
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  pb <- .s4_glmbin(cbind(Dv, W), delta)
  pihat <- function(a) .s4_clip(.s4_expit(as.numeric(cbind(a, W) %*% pb)), 0.025, 1)
  obs <- which(delta > 0.5)
  qb <- .s4_ols(cbind(Dv, W)[obs, , drop = FALSE], yv[obs])$beta
  qhat <- function(a) as.numeric(cbind(a, W) %*% qb)
  Q1 <- qhat(1)
  Q0 <- qhat(0)
  Qobs <- ifelse(Dv > 0.5, Q1, Q0)
  pi_obs <- ifelse(Dv > 0.5, pihat(1), pihat(0))
  H <- delta / pi_obs * (Dv / g - (1 - Dv) / (1 - g))
  den <- sum(H * H)
  num <- sum((H * (yv - Qobs))[delta > 0.5])
  eps <- if (den != 0) num / den else 0
  Q1s <- Q1 + eps / (g * pihat(1))
  Q0s <- Q0 - eps / ((1 - g) * pihat(0))
  psi <- sum(Q1s - Q0s) / n
  resid <- ifelse(delta > 0.5, yv - Qobs - eps * H, 0)
  ic <- H * resid + Q1s - Q0s - psi
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps,
             n_obs = sum(delta), n = n,
             method = "TMLE for the ATE under a missing-at-random outcome")
}
