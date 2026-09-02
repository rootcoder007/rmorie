# SPDX-License-Identifier: AGPL-3.0-or-later
#' Monotone treatment response bounds
#'
#' L_i = y_i if z_i <= d else ymin; U_i = y_i if z_i >= d else ymax; and
#' E\[Y(d)\] lies in \[mean(L), mean(U)\].
#'
#' @param y Observed outcomes.
#' @param z Observed treatment levels.
#' @param d Treatment level whose mean counterfactual is bounded.
#' @param ymin,ymax A priori support of the outcome.
#'
#' @return List with lower, upper, width, nfixed, n, d.
#' @references Manski (1997), Econometrica 65(6), 1311-1334; Manski and
#'   Pepper (2000), Econometrica 68(4), 997-1010.  Standard published
#'   form; neither article is in the local corpus and neither could be
#'   downloaded, so neither was read.
#' @export
#' @examples
#' set.seed(1)
#' Mtrbound(y = rnorm(50), z = rbinom(50, 1, 0.5), d = rbinom(50, 1, 0.5),
#'          ymin = -3, ymax = 3)
Mtrbound <- function(y, z, d, ymin, ymax) {
  y <- .t1_vec(y)
  z <- .t1_vec(z)
  n <- length(y)
  if (length(z) != n) stop("y and z must have the same length")
  if (n == 0) stop("need at least one unit")
  lo <- as.numeric(ymin)
  hi <- as.numeric(ymax)
  if (lo > hi) stop("ymin must not exceed ymax")
  if (any(y < lo | y > hi)) stop("observed outcomes must lie in [ymin, ymax]")
  d <- as.numeric(d)
  L <- ifelse(z <= d, y, lo)
  U <- ifelse(z >= d, y, hi)
  lb <- sum(L) / n
  ub <- sum(U) / n
  .t1_result(lower = lb, upper = ub, width = ub - lb,
             nfixed = sum(z == d), n = n, d = d,
             method = "Monotone treatment response bounds (Manski 1997)")
}
