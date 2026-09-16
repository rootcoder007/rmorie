# SPDX-License-Identifier: AGPL-3.0-or-later
#' Direct and indirect effects from weighted marginal models
#'
#' Weighting rather than conditioning lets the mediator model stay
#' marginal. Conditioning on a mediator-outcome confounder would also
#' condition on anything the exposure caused; the weights remove the
#' confounding while leaving the exposure effect on the mediator intact.
#'
#' Formula: weights \code{1/P(A|H)}, then weighted
#' \code{Y = th0 + th1 a + th2 m + th3 a m} and \code{M = b0 + b1 a},
#' giving \code{NDE = th1 + th3(b0 + b1 a*)} and
#' \code{NIE = b1(th2 + th3 a)}.
#'
#' @param y Outcome.
#' @param A Binary exposure.
#' @param M Mediator.
#' @param H Confounder history used for the weights.
#' @return List with \code{estimate}, \code{nie}, \code{total},
#'   \code{theta}, \code{beta}, \code{w_mean}, \code{n}.
#' @references VanderWeele & Vansteelandt (2010) Am J Epidemiol
#'   172:1339-1348; Robins, Hernan & Brumback (2000) Epidemiology
#'   11:550-560.
#' @export
Medmsm <- function(y, A, M, H) {
  yv <- as.numeric(y)
  Av <- as.numeric(A)
  Mv <- as.numeric(M)
  n <- length(yv)
  Hm <- cbind(1, as.matrix(H))
  gb <- .s4_glmbin(Hm, Av)
  g <- .s4_clip(.s4_expit(as.numeric(Hm %*% gb)), 0.025, 0.975)
  w <- ifelse(Av > 0.5, 1 / g, 1 / (1 - g))
  sw <- sqrt(w)
  theta <- .s4_ols(cbind(sw, sw * Av, sw * Mv, sw * Av * Mv), sw * yv)$beta
  beta <- .s4_ols(cbind(sw, sw * Av), sw * Mv)$beta
  nde <- theta[2] + theta[4] * beta[1]
  nie <- beta[2] * (theta[3] + theta[4])
  .t1_result(estimate = nde, nie = nie, total = nde + nie, theta = theta,
             beta = beta, w_mean = sum(w) / n, n = n,
             method = "Marginal structural mediation by IPTW")
}
