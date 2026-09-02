# SPDX-License-Identifier: AGPL-3.0-or-later
#' Monotone treatment selection bounds
#'
#' E\[Y(d)\] <= P(Z<=d) E\[Y|Z=d\] + P(Z>d) ymax and
#' E\[Y(d)\] >= P(Z<d) ymin + P(Z>=d) E\[Y|Z=d\].
#'
#' @param y Observed outcomes.
#' @param z Observed treatment levels.
#' @param d Level whose mean counterfactual is bounded.
#' @param ymin,ymax A priori support of the outcome.
#'
#' @return List with lower, upper, width, condmean, pbelow, pat, pabove,
#'   n, d.
#' @references Manski and Pepper (2000), Econometrica 68(4), 997-1010.
#'   Standard published form; the article could not be obtained (JSTOR
#'   returned an access stub and NBER t0224 a zero-page PDF), so it was
#'   not read; only the bound stated here is claimed.
#' @export
#' @examples
#' set.seed(1)
#' Mtsbound(y = rnorm(50), z = rbinom(50, 1, 0.5), d = rbinom(50, 1, 0.5),
#'          ymin = -3, ymax = 3)
Mtsbound <- function(y, z, d, ymin, ymax) {
  y <- .t1_vec(y); z <- .t1_vec(z); n <- length(y)
  if (length(z) != n) stop("y and z must have the same length")
  lo <- as.numeric(ymin); hi <- as.numeric(ymax)
  if (lo > hi) stop("ymin must not exceed ymax")
  d <- as.numeric(d)
  at <- which(z == d)
  if (length(at) == 0L) stop("no unit is observed at treatment level d")
  cm <- sum(y[at]) / length(at)
  pb <- sum(z < d) / n; pa <- sum(z > d) / n; pat <- length(at) / n
  ub <- (pb + pat) * cm + pa * hi
  lb <- pb * lo + (pat + pa) * cm
  .t1_result(lower = lb, upper = ub, width = ub - lb, condmean = cm,
             pbelow = pb, pat = pat, pabove = pa, n = n, d = d,
             method = "Monotone treatment selection bounds (Manski-Pepper 2000)")
}
