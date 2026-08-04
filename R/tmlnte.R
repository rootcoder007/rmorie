# SPDX-License-Identifier: AGPL-3.0-or-later
#' Total effect by TMLE, with the mediator held out of the model
#'
#' The natural total effect is the direct and indirect effects added back
#' together, so the mediator must not enter the outcome model:
#' conditioning on it would block the path being counted. The mediator is
#' used only for the contrast report.
#'
#' Formula: \code{NTE = E[Y(1) - Y(0)] = NDE + NIE}, targeted with
#' \code{H = D/g - (1 - D)/(1 - g)}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param M Mediator; reported on, not adjusted for.
#' @param X Baseline covariates.
#' @return List with \code{estimate}, \code{se}, \code{eps}, \code{nde_naive}, \code{n}.
#' @references VanderWeele (2015), Explanation in Causal Inference, ch 2;
#'   van der Laan & Rubin (2006) IJB 2(1):11.
#' @export
Tmlnte <- function(y, D, M, X) {
  W <- cbind(1, as.matrix(X))
  r <- .s4_tmle(y, D, W)
  r2 <- .s4_tmle(y, D, cbind(W, as.numeric(M)))
  .t1_result(estimate = r$psi, se = r$se, eps = r$eps, nde_naive = r2$psi,
             n = r$n, method = "TMLE for the natural total effect")
}
