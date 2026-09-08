# SPDX-License-Identifier: AGPL-3.0-or-later
#' Snijders-Bosker level-1 explained variance
#'
#' The naive multilevel analogue of R-squared can go negative when
#' predictors are added, because the separate variance components are not
#' each individually reduced.  Snijders and Bosker define explained
#' variance on the total residual variance of an individual observation,
#' sigma2_e + sigma2_u, comparing the fitted model against the empty
#' model; so defined it does not go negative for a useless predictor.
#'
#' Formula: R2_1 = 1 - (sigma2_e1 + sigma2_u1) / (sigma2_e0 + sigma2_u0).
#'
#' @param sigma2_e1 Level-1 residual variance of the fitted model.
#' @param sigma2_u1 Level-2 intercept variance of the fitted model.
#' @param sigma2_e0 Level-1 residual variance of the empty model.
#' @param sigma2_u0 Level-2 intercept variance of the empty model.
#' @return List with \code{estimate}, \code{total1}, \code{total0},
#'   \code{icc0}, \code{icc1}, \code{reduction}, \code{method}.
#' @references Snijders, T. A. B. and Bosker, R. J. (1994). Modeled
#'   variance in two-level models. Sociological Methods and Research
#'   22(3):342-363. \doi{10.1177/0049124194022003004}
#' @examples
#' Snr2(1, 0.5, 4, 1)
#' @export
Snr2 <- function(sigma2_e1, sigma2_u1, sigma2_e0, sigma2_u0) {
  e1 <- as.numeric(sigma2_e1)
  u1 <- as.numeric(sigma2_u1)
  e0 <- as.numeric(sigma2_e0)
  u0 <- as.numeric(sigma2_u0)
  if (any(c(e1, u1, e0, u0) < 0))
    stop("snijders_bosker_r2_level1: variance components must be non-negative")
  t1 <- e1 + u1
  t0 <- e0 + u0
  if (t0 <= 0)
    stop("snijders_bosker_r2_level1: baseline total variance must be positive")
  list(estimate = as.numeric(1 - t1 / t0), total1 = t1, total0 = t0,
       icc0 = as.numeric(u0 / t0),
       icc1 = if (t1 > 0) as.numeric(u1 / t1) else 0,
       reduction = as.numeric(t0 - t1),
       method = "R2_1 = 1 - (s2_e1+s2_u1)/(s2_e0+s2_u0) [Snijders & Bosker 1994]")
}

# CANONICAL TEST
# stopifnot(abs(Snr2(2, 1, 2, 1)$estimate) < 1e-15,
#           abs(Snr2(1, 0, 4, 0)$estimate - 0.75) < 1e-15)
