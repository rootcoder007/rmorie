# SPDX-License-Identifier: AGPL-3.0-or-later
#' RCT-assisted TMLE for the trial-population ATE
#'
#' The asymmetry is deliberate.  Observational rows sharpen the outcome
#' regression, because a wrong Q is repaired by the targeting step; they
#' do not enter the clever covariate, because their treatment mechanism
#' is unknown and a wrong g would discard the one thing the trial
#' guarantees.  So g is the trial's own empirical randomisation
#' probability and \code{H = I(trial)/P(trial) * \[D/g - (1 - D)/(1 - g)\]},
#' making the target the ATE in the TRIAL population.  Biased
#' observational rows cost efficiency, not consistency.
#'
#' Rows are stacked trial-first: \code{D} and \code{X} must have
#' \code{length(y_rct) + length(y_obs)} rows in that order.
#'
#' @param y_rct Trial outcomes.
#' @param y_obs Observational outcomes.
#' @param D Binary treatment, trial rows first.
#' @param X Covariates, trial rows first.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{g_rct}, \code{n_rct}, \code{n_obs}, \code{n}.
#' @references Athey, S., Chetty, R., Imbens, G. W. & Kang, H. (2025).
#'   Review of Economic Studies 93(4):2284-2312; van der Laan, M. J. &
#'   Rubin, D. (2006). IJB 2(1):11.
#' @export
Tmlrct <- function(y_rct, y_obs, D, X) {
  y1 <- as.numeric(y_rct); y2 <- as.numeric(y_obs)
  yv <- c(y1, y2); Dv <- as.numeric(D)
  n1 <- length(y1); n <- length(yv)
  if (n1 < 2L) stop("Tmlrct: need at least two trial rows")
  if (length(Dv) != n) stop("Tmlrct: D must have one entry per stacked row")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlrct: X must have one row per stacked row")
  Sv <- c(rep(1, n1), rep(0, n - n1))
  W <- cbind(1, Xm, Sv)
  g0 <- .s4_clip(sum(Dv[seq_len(n1)]) / n1, 0.025, 0.975)
  qb <- .s4_ols(cbind(Dv, W), yv)$beta
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Qobs <- ifelse(Dv > 0.5, Q1, Q0)
  pt <- n1 / n
  H <- Sv / pt * (Dv / g0 - (1 - Dv) / (1 - g0))
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1s <- Q1 + eps * Sv / (pt * g0)
  Q0s <- Q0 - eps * Sv / (pt * (1 - g0))
  psi <- sum((Q1s - Q0s)[seq_len(n1)]) / n1
  ic <- H * (yv - Qobs - eps * H) + Sv / pt * (Q1s - Q0s - psi)
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, g_rct = g0,
             n_rct = n1, n_obs = n - n1, n = n,
             method = "RCT-assisted TMLE for the trial-population ATE")
}
