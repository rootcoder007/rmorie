# SPDX-License-Identifier: AGPL-3.0-or-later
#' Andrews sine IRLS weight function
#'
#' Formula: w(r) = sin(r/A)/(r/A) for |r| <= A pi, and 0 otherwise; w(0) = 1
#'
#' @param r Scaled residuals.
#' @param A Tuning constant; 1.339 gives 95% Gaussian efficiency.

#' @param r See Usage.
#' @param A See Usage.
#' @return List with ``weight``, ``rejected``, ``A``, ``n``.
#' @references Andrews (1974), A robust method for multiple linear regression, Technometrics 16:523-531. Not held locally; w(z) = sin(z/A)/(z/A) for |z| <= A pi with A = 1.339 is as documented by statsmodels' AndrewWave norm, the reference implementation.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Andrewswt(V)
Andrewswt <- function(r, A = 1.339) {
  r <- .t1_vec(r)
  A <- as.numeric(A)
  if (A <= 0) stop("A must be positive")
  lim <- A * pi
  w <- ifelse(abs(r) > lim, 0, ifelse(r == 0, 1, sin(r / A) / (r / A)))
  .t1_result(weight = w, rejected = sum(abs(r) > lim), A = A, n = length(r),
             method = "Andrews sine IRLS weight")
}
