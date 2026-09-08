# SPDX-License-Identifier: AGPL-3.0-or-later
#' Snijders-Bosker level-2 explained variance
#'
#' At level two the quantity predicted is a group mean, whose residual
#' variance is sigma2_u + sigma2_e / n rather than sigma2_u + sigma2_e:
#' averaging over n members shrinks the level-1 noise by n but leaves the
#' group-level term untouched.  With n = 1 this coincides exactly with the
#' level-1 measure, and as n grows it tends to 1 - sigma2_u1 / sigma2_u0.
#'
#' Formula: R2_2 = 1 - (sigma2_e1/n + sigma2_u1) / (sigma2_e0/n + sigma2_u0).
#'
#' @param sigma2_e1 Level-1 residual variance of the fitted model.
#' @param sigma2_u1 Level-2 intercept variance of the fitted model.
#' @param sigma2_e0 Level-1 residual variance of the empty model.
#' @param sigma2_u0 Level-2 intercept variance of the empty model.
#' @param n Group size; the harmonic mean of group sizes for unbalanced
#'   data.  Must be positive.
#' @return List with \code{estimate}, \code{total1}, \code{total0},
#'   \code{n}, \code{limit_large_n}, \code{reduction}, \code{method}.
#' @references Snijders, T. A. B. and Bosker, R. J. (1994). Modeled
#'   variance in two-level models. Sociological Methods and Research
#'   22(3):342-363. \doi{10.1177/0049124194022003004}
#' @examples
#' Snr2u(1, 0.5, 4, 1, 20)
#' @export
Snr2u <- function(sigma2_e1, sigma2_u1, sigma2_e0, sigma2_u0, n = 1) {
  e1 <- as.numeric(sigma2_e1)
  u1 <- as.numeric(sigma2_u1)
  e0 <- as.numeric(sigma2_e0)
  u0 <- as.numeric(sigma2_u0)
  nn <- as.numeric(n)
  if (any(c(e1, u1, e0, u0) < 0))
    stop("snijders_bosker_r2_level2: variance components must be non-negative")
  if (nn <= 0) stop("snijders_bosker_r2_level2: n must be positive")
  t1 <- e1 / nn + u1
  t0 <- e0 / nn + u0
  if (t0 <= 0)
    stop("snijders_bosker_r2_level2: baseline total variance must be positive")
  list(estimate = as.numeric(1 - t1 / t0), total1 = t1, total0 = t0, n = nn,
       limit_large_n = if (u0 > 0) as.numeric(1 - u1 / u0) else NaN,
       reduction = as.numeric(t0 - t1),
       method = "R2_2 = 1 - (s2_e1/n+s2_u1)/(s2_e0/n+s2_u0) [Snijders & Bosker 1994]")
}

# CANONICAL TEST
# stopifnot(abs(Snr2u(1, 0.5, 4, 1, 1)$estimate - Snr2(1, 0.5, 4, 1)$estimate) < 1e-15)
