# SPDX-License-Identifier: AGPL-3.0-or-later
#' Targeted marginal odds ratio, with inference on the log scale
#'
#' The MARGINAL odds ratio is not the conditional one: odds ratios are
#' non-collapsible. Inference is on the log scale because the sampling
#' distribution of a ratio is badly skewed.
#'
#' Formula: psi = \[mu1/(1-mu1)\] / \[mu0/(1-mu0)\];
#'   IC_logOR = IC_1/(mu1(1-mu1)) - IC_0/(mu0(1-mu0));
#'   CI = exp(log psi -+ z se_log)
#'
#' @param Y Binary outcome.
#' @param A Binary treatment.
#' @param QAW,Q1W,Q0W Initial outcome predictions.
#' @param g1W Initial propensity.
#' @param gbound Propensity truncation level.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{log_or}, \code{se_log},
#'   \code{ci_lower}, \code{ci_upper}, \code{p_value}, \code{mu1},
#'   \code{mu0}, \code{n}.
#' @references Verified against the CRAN package tmle 2.1.1 (Gruber & van
#'   der Laan), whose calcParameters sets OR$psi <-
#'   mu1/(1-mu1)/(mu0/(1-mu0)) and builds IC.logOR on the log scale.
#' @export
Tmleor <- function(Y, A, QAW, Q1W, Q0W, g1W, gbound = 0.025, level = 0.95) {
  Y <- .t1_vec(Y)
  A <- .t1_vec(A)
  n <- length(Y)
  QAW <- .t1_vec(QAW)
  Q1W <- .t1_vec(Q1W)
  Q0W <- .t1_vec(Q0W)
  g1W <- .t1_vec(g1W)
  if (any(c(length(A), length(QAW), length(Q1W), length(Q0W),
            length(g1W)) != n))
    stop("every argument must have one entry per observation")
  if (any(!(A %in% c(0, 1)))) stop("A must be binary 0/1")
  if (any(Y < 0 | Y > 1)) stop("Y must lie in [0, 1]")
  if (n < 2L) stop("at least two observations are required")
  fit <- .b1_target(Y, A, QAW, Q1W, Q0W, g1W, gbound)
  cv <- .b1_curves(Y, A, fit)
  mu1 <- cv$mu1
  mu0 <- cv$mu0
  if (mu1 <= 0 || mu1 >= 1 || mu0 <= 0 || mu0 >= 1)
    stop("a targeted mean hit 0 or 1; the odds ratio is undefined")
  psi <- (mu1 / (1 - mu1)) / (mu0 / (1 - mu0))
  ic <- cv$ic1 / (mu1 * (1 - mu1)) - cv$ic0 / (mu0 * (1 - mu0))
  sel <- sqrt(stats::var(ic) / n)
  lp <- log(psi)
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = psi, log_or = lp, se_log = sel,
             ci_lower = exp(lp - z * sel), ci_upper = exp(lp + z * sel),
             p_value = if (sel > 0) 2 * stats::pnorm(abs(lp / sel), lower.tail = FALSE) else 0,
             mu1 = mu1, mu0 = mu0, n = as.numeric(n),
             method = "TMLE marginal odds ratio, inference on the log scale")
}
