# SPDX-License-Identifier: AGPL-3.0-or-later
#' Standard errors that survive the wrong variance assumption
#'
#' Least squares stays unbiased under heteroskedasticity; only its
#' standard errors are wrong. The sandwich replaces the assumed
#' \code{sigma^2 (X'X)^-1} with a middle built from the observed squared
#' residuals, so nothing about the variance has to be modelled. HC0 is
#' biased down in small samples, which is what HC1 to HC3 fix.
#'
#' Formula: \code{V = (X'X)^-1 X' Omega X (X'X)^-1},
#' \code{Omega = diag(w_i e_i^2)}; HC0 \code{w = 1}, HC1
#' \code{n/(n-p)}, HC2 \code{1/(1-h)}, HC3 \code{1/(1-h)^2}.
#'
#' @param X Design; supply your own intercept column.
#' @param y Response.
#' @param kind One of HC0, HC1, HC2, HC3.
#' @return List with \code{estimate}, \code{coef}, \code{V},
#'   \code{ols_se}, \code{n}.
#' @references White, H. (1980). Econometrica 48:817-838; MacKinnon &
#'   White (1985) J Econometrics 29:305-325.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Robcov(V, V)
Robcov <- function(X, y, kind = "HC0") {
  Xm <- as.matrix(X); yv <- as.numeric(y)
  n <- nrow(Xm); p <- ncol(Xm)
  fit <- .s4_ols(Xm, yv)
  h <- rowSums((Xm %*% fit$xtxinv) * Xm)
  w <- switch(kind, HC1 = rep(n / (n - p), n), HC2 = 1 / (1 - h),
              HC3 = 1 / (1 - h)^2, rep(1, n))
  mid <- crossprod(Xm, Xm * (w * fit$resid^2))
  V <- fit$xtxinv %*% mid %*% fit$xtxinv
  s2 <- sum(fit$resid^2) / (n - p)
  .t1_result(estimate = sqrt(diag(V)), coef = fit$beta, V = V,
             ols_se = sqrt(s2 * diag(fit$xtxinv)), n = n,
             method = "Sandwich heteroskedasticity-consistent standard errors")
}
