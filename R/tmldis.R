# SPDX-License-Identifier: AGPL-3.0-or-later
#' How much of a group gap survives equalising the covariates
#'
#' A disparity is not a causal effect of group membership, so the
#' estimand is deliberately different: hold the group fixed and move the
#' covariate distribution instead. What remains after standardising to
#' the reference distribution is the residual disparity.
#'
#' Formula: \code{PAD = E[Y|S=1] - E_{X ~ P(X|S=0)}[E(Y|S=1, X)]}.
#'
#' @param y Outcome.
#' @param S_grp Binary group indicator.
#' @param X Covariates.
#' @param X_target Reference covariate rows; the S = 0 rows by default.
#' @return List with \code{estimate}, \code{crude}, \code{explained}, \code{se}, \code{n}.
#' @references VanderWeele, T. J. & Robinson, W. R. (2014). Epidemiology
#'   25:473-484.
#' @export
#' @examples
#' Tmldis(y = c(1, 2, 3, 4, 5, 6, 7, 8), S_grp = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmldis <- function(y, S_grp, X, X_target = NULL) {
  yv <- as.numeric(y); Sv <- as.numeric(S_grp); n <- length(yv)
  W <- cbind(1, as.matrix(X))
  i1 <- which(Sv > 0.5); i0 <- which(Sv <= 0.5)
  b1 <- .s4_ols(W[i1, , drop = FALSE], yv[i1])$beta
  Wt <- if (is.null(X_target)) W[i0, , drop = FALSE] else cbind(1, as.matrix(X_target))
  std <- sum(as.numeric(Wt %*% b1)) / nrow(Wt)
  mu1 <- mean(yv[i1]); mu0 <- mean(yv[i0])
  resid <- yv[i1] - as.numeric(W[i1, , drop = FALSE] %*% b1)
  se <- sqrt(sum((resid - mean(resid))^2) / (length(resid) - 1) / length(resid))
  .t1_result(estimate = mu1 - std, crude = mu1 - mu0,
             explained = (mu1 - mu0) - (mu1 - std), se = se, n = n,
             method = "Standardised disparity remaining after covariates")
}
