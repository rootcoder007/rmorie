# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the value of the estimated optimal individualized rule
#'
#' The rule is read off the blip rather than the outcome regression,
#' because only its sign matters: \code{d*(V) = I(B(V) > 0)} with
#' \code{B(V) = E\[Y(1) - Y(0) | V\]}.  With the rule fixed the target is a
#' mean under a known deterministic regime, so the clever covariate is
#' \code{H = I(D = d*(V)) / g_{d*(V)}(W)} and a linear fluctuation
#' \code{Q* = Q + eps H}, \code{eps = sum H (y - Q)/sum H^2}, solves the
#' efficient score.  \code{psi = mean_i \[Q(d_i, W_i) + eps/g_{d_i}(W_i)\]}.
#'
#' The reported SE is the non-uniform influence-curve SE; it is honest
#' away from the non-uniqueness boundary \code{B(V) = 0}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param W Covariates entering the nuisance models.
#' @param X Covariates the rule may depend on.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_treated}, \code{n}.
#' @references Luedtke, A. R. & van der Laan, M. J. (2016). Annals of
#'   Statistics 44(2):713-742.
#' @export
#' @examples
#' Tmlitr(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4,
#' 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlitr <- function(y, D, W, X) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  n <- length(yv)
  if (n == 0L || length(Dv) != n)
    stop("Tmlitr: y and D must share one length")
  Wm <- as.matrix(W)
  Vm <- as.matrix(X)
  if (nrow(Wm) != n || nrow(Vm) != n)
    stop("Tmlitr: W and X must have one row per subject")
  Wd <- cbind(1, Wm)
  gb <- .s4_glmbin(Wd, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(Wd %*% gb)), 0.025, 0.975)
  des <- cbind(Dv, Wd, Dv * Wm)
  qb <- .s4_ols(des, yv)$beta
  qhat <- function(d) as.numeric(cbind(d, Wd, d * Wm) %*% qb)
  blip <- qhat(1) - qhat(0)
  bd <- cbind(1, Vm)
  bfit <- .s4_ols(bd, blip)$fitted
  rule <- ifelse(bfit > 0, 1, 0)
  gd <- ifelse(rule > 0.5, g, 1 - g)
  H <- ifelse(abs(Dv - rule) < 0.5, 1, 0) / gd
  Q1 <- qhat(1)
  Q0 <- qhat(0)
  Qd <- ifelse(rule > 0.5, Q1, Q0)
  Qobs <- ifelse(Dv > 0.5, Q1, Q0)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Qds <- Qd + eps / gd
  psi <- sum(Qds) / n
  ic <- H * (yv - Qobs - eps * H) + Qds - psi
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps,
             n_treated = sum(rule), n = n,
             method = "TMLE for the value of the estimated optimal individualized rule")
}
