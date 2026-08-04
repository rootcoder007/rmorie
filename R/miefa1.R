# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fraction of missing information after multiple imputation
#'
#' Rubin (1987), Multiple Imputation for Nonresponse in Surveys, section
#' 3.1, and Schafer (1997), Analysis of Incomplete Multivariate Data,
#' section 4.3: with m imputations, within variance W and between variance
#' B, T = W + (1 + 1/m) B, r = (1 + 1/m) B / W, lambda = (1 + 1/m) B / T,
#' nu = (m - 1)(1 + 1/r)^2 and gamma = (r + 2/(nu + 3)) / (r + 1).
#' Neither book was available here as a full text; the expressions are
#' quoted in their standard published form.
#'
#' @param between between-imputation variance B.
#' @param within within-imputation variance W.
#' @param m number of imputations.
#' @return list: estimate (lambda), gamma, total, r, df, between, within,
#'   m, method.
#' @keywords internal
#' @examples
#' Mifmi(0.4, 1.2, 5)$estimate
#' @export
Mifmi <- function(between, within, m) {
  b <- as.numeric(between); w <- as.numeric(within); mm <- as.numeric(m)
  fac <- if (mm > 0) 1 + 1 / mm else NaN
  total <- w + fac * b
  lam <- if (total != 0) (fac * b) / total else NaN
  r <- if (w != 0) (fac * b) / w else Inf
  if (is.infinite(r) || mm <= 1) {
    nu <- Inf
    gamma <- lam
  } else {
    nu <- if (r > 0) (mm - 1) * (1 + 1 / r)^2 else Inf
    gamma <- (r + 2 / (nu + 3)) / (r + 1)
  }
  list(estimate = lam, gamma = gamma, total = total, r = r, df = nu,
       between = b, within = w, m = mm,
       method = "Fraction of missing information after multiple imputation")
}
