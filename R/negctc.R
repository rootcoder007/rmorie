# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fit the effect of D on a known-null outcome and test it for zero
#'
#' A negative control outcome is one the exposure cannot plausibly affect
#' but that shares the exposure's confounders. Its true effect is zero by
#' construction, so any effect the model finds there is bias, not signal.
#' The logic is one-directional: a non-zero estimate is evidence of
#' residual confounding or misspecification, while a zero estimate is NOT
#' evidence of unconfoundedness -- it says only that this control did not
#' detect a problem.
#'
#' The estimate is the OLS coefficient on \code{D} in
#' \code{y_neg = b0 + b_D D + X gamma + e} with the homoscedastic
#' standard error \code{sqrt(s^2 (X'X)^-1_DD)}, \code{s^2 = RSS/(n - p)},
#' and the test is the Wald z-test of \code{b_D = 0}.
#'
#' @param y_neg Negative-control outcome, length n.
#' @param D Exposure, length n.
#' @param X Optional adjustment covariates, n by q.
#' @param alpha Size of the test.
#' @return List with estimate (b_D), se, z, p_value,
#'   confounding_suspected, negct_verdict_at_5pct, ci_lower, ci_upper,
#'   alpha, n, p.
#' @references Lipsitch, Tchetgen Tchetgen and Cohen (2010), Epidemiology
#'   21(3), 383-388, \doi{10.1097/EDE.0b013e3181d61eeb}, verified against
#'   Crossref; Shi, Miao and Tchetgen Tchetgen (2020), Current
#'   Epidemiology Reports 7, 190-202, \doi{10.1007/s40471-020-00243-4}.
#'   Neither paper was in the local corpus; the estimator is ordinary
#'   least squares and the test is the standard Wald one.
#' @export
Negctc <- function(y_neg, D, X = NULL, alpha = 0.05) {
  y <- .t1_vec(y_neg); d <- .t1_vec(D); n <- length(y)
  if (n == 0L) stop("y_neg is empty")
  if (length(d) != n) stop("y_neg and D must have the same length")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("alpha must lie in (0, 1)")
  dm <- if (is.null(X)) cbind(1, d) else {
    Xm <- as.matrix(X)
    if (nrow(Xm) != n) Xm <- t(Xm)
    if (nrow(Xm) != n) stop("X must have one row per observation")
    cbind(1, d, Xm)
  }
  p <- ncol(dm)
  if (n <= p) stop("need more observations than parameters")
  fit <- .t1_lstsq(dm, y)
  rss <- sum(fit$resid^2)
  s2 <- rss / (n - p)
  v <- s2 * fit$xtxinv[2, 2]
  se <- if (v > 0) sqrt(v) else 0
  b <- fit$beta[2]
  if (se <= 0) {
    stop(paste("the exposure is collinear with the adjustment set;",
               "its coefficient has no standard error"))
  }
  z <- b / se
  pv <- 2 * stats::pnorm(-abs(z))
  zc <- stats::qnorm(1 - a / 2)
  .t1_result(estimate = b, se = se, z = z, p_value = pv,
             confounding_suspected = if (pv < a) 1 else 0,
             negct_verdict_at_5pct = if (pv < 0.05) 1 else 0,
             ci_lower = b - zc * se, ci_upper = b + zc * se,
             alpha = a, n = n, p = p,
             method = "Negative control outcome (Wald test of a known null)")
}
