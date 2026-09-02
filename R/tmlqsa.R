# SPDX-License-Identifier: AGPL-3.0-or-later
#' How far the initial fit was from solving the efficient score equation
#'
#' The initial fit generally leaves a non-zero empirical score; the
#' targeted fit must drive it to numerical zero.
#' \code{score_init_scaled} is roughly the number of standard errors of
#' bias the plug-in estimate carried.
#'
#' Formula: D*(psi)(O) = (A/g1 - (1-A)/g0)(Y - Q(A,W))
#'   + Q(1,W) - Q(0,W) - psi; targeting solves the empirical mean to 0
#'
#' @param Y,A Outcome in \[0, 1\] and binary treatment.
#' @param QAW,Q1W,Q0W Initial outcome predictions.
#' @param g1W Initial propensity.
#' @param gbound Propensity truncation level.
#' @return List with \code{score_init}, \code{score_final},
#'   \code{score_init_scaled}, \code{reduction}, \code{psi_init},
#'   \code{psi_final}, \code{shift}, \code{epsilon}, \code{n}.
#' @references Verified against the CRAN package tmle 2.1.1 (Gruber & van
#'   der Laan). The score-equation view of the targeting step is van der
#'   Laan & Rubin (2006), International Journal of Biostatistics 2(1),
#'   Article 11.
#' @export
#' @examples
#' Tmleqs(Y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), QAW = c(1, 2, 3, 4, 5, 6, 7, 8), Q1W = c(1, 2, 3, 4, 5, 6, 7, 8), Q0W = c(1, 2, 3, 4, 5, 6, 7, 8), g1W = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmleqs <- function(Y, A, QAW, Q1W, Q0W, g1W, gbound = 0.025) {
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
  if (n < 2L) stop("at least two observations are required")
  g1 <- .b1_bound(g1W, gbound, 1 - gbound)
  g0 <- 1 - g1
  psi0 <- mean(Q1W) - mean(Q0W)
  d0 <- (A / g1 - (1 - A) / g0) * (Y - QAW) + Q1W - Q0W - psi0
  s0 <- mean(d0)
  fit <- .b1_target(Y, A, QAW, Q1W, Q0W, g1W, gbound)
  cv <- .b1_curves(Y, A, fit)
  psi1 <- cv$mu1 - cv$mu0
  d1 <- (A / g1 - (1 - A) / g0) * (Y - fit$QAstar) +
    fit$Q1star - fit$Q0star - psi1
  s1 <- mean(d1)
  sd0 <- stats::sd(d0)
  .t1_result(score_init = s0, score_final = s1,
             score_init_scaled = if (sd0 > 0) s0 * sqrt(n) / sd0 else NaN,
             reduction = if (s0 != 0) abs(s1) / abs(s0) else NaN,
             psi_init = psi0, psi_final = psi1, shift = psi1 - psi0,
             epsilon = fit$epsilon, n = as.numeric(n),
             method = "Efficient-score residual before and after targeting")
}
