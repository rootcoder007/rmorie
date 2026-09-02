# SPDX-License-Identifier: AGPL-3.0-or-later
#' Targeted estimate of the marginal risk difference E\[Y_1\] - E\[Y_0\]
#'
#' The targeted estimate solves the efficient score equation, so its
#' influence curve gives an honest standard error where the plug-in mean
#' of a machine-learning fit does not. \code{psi_init} shows the size of
#' the correction.
#'
#' Formula: fluctuate on H1 = A/g1(W), H0 = (1-A)/g0(W);
#'   psi = mean(Q*(1,W)) - mean(Q*(0,W));
#'   se = sqrt(var(IC)/n)
#'
#' @param Y Outcome in \[0, 1\].
#' @param A Binary treatment.
#' @param QAW,Q1W,Q0W Initial outcome predictions.
#' @param g1W Initial propensity.
#' @param gbound Propensity truncation level.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{p_value}, \code{mu1}, \code{mu0},
#'   \code{psi_init}, \code{epsilon}, \code{ic_mean}, \code{n}.
#' @references Verified against the reference implementation in the CRAN
#'   package tmle 2.1.1 (Gruber & van der Laan), its fluctuation glm and
#'   calcParameters influence curves. That package is van der Laan's
#'   group's own software for van der Laan & Rose (2011), Targeted
#'   Learning.
#' @export
Tmlerd <- function(Y, A, QAW, Q1W, Q0W, g1W, gbound = 0.025, level = 0.95) {
  Y <- .t1_vec(Y); A <- .t1_vec(A); n <- length(Y)
  QAW <- .t1_vec(QAW); Q1W <- .t1_vec(Q1W); Q0W <- .t1_vec(Q0W)
  g1W <- .t1_vec(g1W)
  if (any(c(length(A), length(QAW), length(Q1W), length(Q0W),
            length(g1W)) != n))
    stop("every argument must have one entry per observation")
  if (any(!(A %in% c(0, 1)))) stop("A must be binary 0/1")
  if (any(Y < 0 | Y > 1)) stop("Y must lie in [0, 1]")
  if (n < 2L) stop("at least two observations are required")
  fit <- .b1_target(Y, A, QAW, Q1W, Q0W, g1W, gbound)
  cv <- .b1_curves(Y, A, fit)
  ic <- cv$ic1 - cv$ic0
  psi <- cv$mu1 - cv$mu0
  se <- sqrt(stats::var(ic) / n)
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = psi, se = se, ci_lower = psi - z * se,
             ci_upper = psi + z * se,
             p_value = if (se > 0) 2 * stats::pnorm(abs(psi / se), lower.tail = FALSE) else 0,
             mu1 = cv$mu1, mu0 = cv$mu0,
             psi_init = mean(Q1W) - mean(Q0W), epsilon = fit$epsilon,
             ic_mean = mean(ic), n = as.numeric(n),
             method = "TMLE marginal risk difference")
}
