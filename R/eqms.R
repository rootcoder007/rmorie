# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean-sigma equating coefficients
#'
#' Formula: the scale linking theta_R = A theta_F + B implies
#' b_R = A b_F + B for the common items, so matching the first two
#' moments of the difficulties gives A = sd(b_R) / sd(b_F) and
#' B = mean(b_R) - A mean(b_F).
#'
#' DOCSTRING ERRATUM: the generated stub printed A = sd(b_F)/sd(b_R) and
#' B = mean(b_F) - A mean(b_R), the transformation in the opposite
#' direction from its own sibling module eqmm.  Marco's method places the
#' NEW form on the REFERENCE metric, the orientation implemented here.
#'
#' @param y Scores on the Form F metric to place on the Form R metric.
#' @param b_R Common-item difficulties on the Form R (reference) metric.
#' @param b_F The same items' difficulties on the Form F metric.
#' @param ddof Denominator correction for the standard deviations; 1
#'   gives the sample sd, 0 the population sd.
#' @return List with \code{estimate}, \code{A}, \code{B},
#'   \code{equated}, \code{n_items}, \code{n}, \code{method}.
#' @references Marco (1977), Journal of Educational Measurement
#'   14(2):139-160, doi:10.1111/j.1745-3984.1977.tb00033.x.
#' @export
Eqms <- function(y, b_R, b_F, ddof = 1) {
  bR <- as.numeric(b_R); bF <- as.numeric(b_F)
  k <- length(bR)
  if (k < 2L) stop("need at least two common items")
  if (length(bF) != k) stop("b_R and b_F must have the same length")
  ddof <- as.integer(ddof)
  if (!(ddof %in% c(0L, 1L))) stop("ddof must be 0 or 1")
  sF <- .s03sd(bF, ddof)
  if (sF <= 0) stop("b_F has zero spread; mean-sigma is undefined")
  A <- .s03sd(bR, ddof) / sF
  B <- .s03mean(bR) - A * .s03mean(bF)
  yy <- as.numeric(y)
  eq <- A * yy + B
  .t1_result(estimate = A, A = A, B = B, equated = eq, n_items = k,
             n = length(yy), method = "Mean-sigma equating coefficients")
}
