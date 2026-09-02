# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the marginal recurrent-event rate ratio
#'
#' With recurrent events the natural marginal summary is the RATE, not a
#' hazard ratio: the mean function of the counting process is identified
#' without any assumption on the dependence between a subject's
#' successive events, which is what makes the Lin-Wei-Yang-Ying rate
#' model usable when that dependence is unknown.  Each subject
#' contributes \code{N_i/T_i}; the outcome regression is on that rate,
#' the clever covariate is \code{H = D/g - (1 - D)/(1 - g)}, and
#' \code{mu_a = mean_i Q*(a, X_i)}, \code{estimate = mu_1/mu_0}.  The SE
#' is the delta-method combination of the two arms' influence curves, so
#' it is a ratio SE, not a difference SE.
#'
#' @param time Follow-up duration of each subject; must be positive.
#' @param event Number of events observed for each subject.
#' @param D Binary treatment.
#' @param X Baseline covariates.
#' @return List with \code{estimate}, \code{se}, \code{mu1}, \code{mu0},
#'   \code{eps}, \code{n}.
#' @references Lin, D. Y., Wei, L. J., Yang, I. & Ying, Z. (2000). JRSS
#'   B 62(4):711-730; van der Laan, M. J. & Rubin, D. (2006). IJB
#'   2(1):11.
#' @export
#' @examples
#' Tmlrec(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlrec <- function(time, event, D, X) {
  tv <- as.numeric(time)
  ev <- as.numeric(event)
  Dv <- as.numeric(D)
  n <- length(tv)
  if (n == 0L || length(ev) != n || length(Dv) != n)
    stop("Tmlrec: time, event and D must share one length")
  if (any(tv <= 0)) stop("Tmlrec: follow-up time must be positive")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlrec: X must have one row per subject")
  rate <- ev / tv
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  qb <- .s4_ols(cbind(Dv, W), rate)$beta
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Qobs <- ifelse(Dv > 0.5, Q1, Q0)
  H <- Dv / g - (1 - Dv) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (rate - Qobs)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  mu1 <- sum(Q1s) / n
  mu0 <- sum(Q0s) / n
  if (mu0 == 0) stop("Tmlrec: the control-arm rate is zero; no rate ratio")
  r <- rate - Qobs - eps * H
  ic1 <- Dv / g * r + Q1s - mu1
  ic0 <- (1 - Dv) / (1 - g) * r + Q0s - mu0
  ic <- ic1 / mu0 - mu1 * ic0 / (mu0 * mu0)
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = mu1 / mu0, se = se, mu1 = mu1, mu0 = mu0, eps = eps, n = n,
             method = "TMLE for the marginal recurrent-event rate ratio")
}
