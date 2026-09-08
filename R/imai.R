# SPDX-License-Identifier: AGPL-3.0-or-later
#' Average causal mediation effects under sequential ignorability
#'
#' The contribution was a clean statement of what has to be true:
#' sequential ignorability. Everything else, including the familiar
#' product of coefficients, follows -- and when exposure and mediator
#' interact the mediation effect depends on which exposure level you read
#' it at, so both are returned.
#'
#' Formula: \code{delta(a) = b1 (th2 + th3 a)},
#' \eqn{zeta(a) = th1 + th3 (b0 + b1 a + b2 primec)}.
#'
#' @param X Treatment.
#' @param M Mediator.
#' @param Y Outcome.
#' @param Cc Optional covariates; read at their means.
#' @return List with \code{estimate}, \code{acme_0}, \code{acme_1},
#'   \code{ade_0}, \code{ade_1}, \code{total}, \code{prop_mediated}, \code{n}.
#' @references Imai, K., Keele, L. & Yamamoto, T. (2010). Statistical
#'   Science 25:51-71, equations (7) and (8).
#' @export
#' @examples
#' Imai(X = 5L, M = 5L, Y = c(1, 2, 3, 4, 5, 6, 7, 8))
Imai <- function(X, M, Y, Cc = NULL) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  bc0 <- mm$beta[1]
  if (length(mm$cbar)) bc0 <- bc0 + sum(mm$beta[2 + seq_along(mm$cbar)] * mm$cbar)
  d0 <- mm$beta[2] * mm$theta[3]
  d1 <- mm$beta[2] * (mm$theta[3] + mm$theta[4])
  z0 <- mm$theta[2] + mm$theta[4] * bc0
  z1 <- mm$theta[2] + mm$theta[4] * (bc0 + mm$beta[2])
  total <- 0.5 * (d0 + d1) + 0.5 * (z0 + z1)
  .t1_result(estimate = 0.5 * (d0 + d1), acme_0 = d0, acme_1 = d1,
             ade_0 = z0, ade_1 = z1, total = total,
             prop_mediated = if (total != 0) 0.5 * (d0 + d1) / total else NaN,
             n = length(as.numeric(Y)),
             method = "Imai-Keele-Yamamoto causal mediation")
}
