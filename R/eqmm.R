# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean-mean equating coefficients
#'
#' Formula: the scale linking theta_R = A theta_F + B carries the IRT
#' item parameters as b_R = A b_F + B and a_R = a_F / A.  Averaging each
#' relation over the common items gives A = mean(a_F) / mean(a_R) and
#' B = mean(b_R) - A mean(b_F).
#'
#' DOCSTRING ERRATUM: the generated stub printed A = mean(a_R)/mean(a_F),
#' the reciprocal.  Discrimination is inversely proportional to the scale
#' factor -- stretching the ability metric by A flattens the item
#' characteristic curve by the same factor -- so the ratio must be a_F
#' over a_R.  The stub's B was already correct and the two are mutually
#' consistent only with A as written here.
#'
#' @param y Scores on the Form F metric to place on the Form R metric.
#' @param a_R,b_R Common-item discriminations and difficulties on the
#'   Form R (reference) metric.
#' @param a_F,b_F The same items' parameters on the Form F metric.
#' @return List with \code{estimate}, \code{A}, \code{B},
#'   \code{equated}, \code{n_items}, \code{n}, \code{method}.
#' @references Loyd & Hoover (1980), Journal of Educational Measurement
#'   17(3):179-193, doi:10.1111/j.1745-3984.1980.tb00825.x.
#' @export
Eqmm <- function(y, a_R, b_R, a_F, b_F) {
  aR <- as.numeric(a_R); bR <- as.numeric(b_R)
  aF <- as.numeric(a_F); bF <- as.numeric(b_F)
  k <- length(aR)
  if (k == 0L) stop("empty input: no common items")
  if (length(bR) != k || length(aF) != k || length(bF) != k)
    stop("all item-parameter vectors must have the same length")
  if (any(aR <= 0) || any(aF <= 0)) stop("discriminations must be positive")
  A <- .s03mean(aF) / .s03mean(aR)
  B <- .s03mean(bR) - A * .s03mean(bF)
  yy <- as.numeric(y)
  eq <- A * yy + B
  .t1_result(estimate = A, A = A, B = B, equated = eq, n_items = k,
             n = length(yy), method = "Mean-mean equating coefficients")
}
