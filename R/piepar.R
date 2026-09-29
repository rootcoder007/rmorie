# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effect of shifting the exposure distribution, not eliminating it
#'
#' A total effect compares everyone exposed with everyone unexposed, a
#' comparison no policy could bring about. The population intervention
#' effect compares what happened with what would happen under a realistic
#' redistribution of exposure.
#'
#' Formula: \code{PIE = E[Y(do(X = x*))] - E[Y]}, the first term by
#' standardising the fitted outcome regression over the proposed
#' exposure distribution.
#'
#' @param y Outcome.
#' @param X Exposure in the first column, covariates after it.
#' @param intervention_dist Exposure values defining the intervened
#'   distribution.
#' @return List with \code{estimate}, \code{observed}, \code{intervened},
#'   \code{se}, \code{n}. With an intercept the PIE equals
#'   \eqn{b_1 (\bar x^* - \bar x)} and \code{se} is
#'   \eqn{|\bar x^* - \bar x|} times the OLS standard error of \eqn{b_1}.
#' @references Westreich, D. (2014). Epidemiology 25:437-440.
#' @export
#' @examples
#' Piepar(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   intervention_dist = c(1, 2, 3, 4, 5, 6, 7, 8))
Piepar <- function(y, X, intervention_dist) {
  yv <- as.numeric(y)
  Xm <- as.matrix(X)
  n <- length(yv)
  W <- cbind(1, Xm)
  fit <- .s4_ols(W, yv)
  xs <- as.numeric(intervention_dist)
  tot <- 0
  for (xv in xs) {
    Wi <- W
    Wi[, 2] <- xv
    tot <- tot + sum(as.numeric(Wi %*% fit$beta)) / n
  }
  interv <- tot / length(xs)
  obs <- sum(yv) / n
  p <- ncol(W)
  dx <- sum(xs) / length(xs) - sum(W[, 2]) / n
  se <- if (n > p) abs(dx) * sqrt(sum(fit$resid^2) / (n - p) * fit$xtxinv[2, 2]) else NaN
  .t1_result(estimate = interv - obs, observed = obs, intervened = interv,
             se = se, n = n, method = "Population intervention effect")
}
