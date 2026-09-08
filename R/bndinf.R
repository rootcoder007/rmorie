# SPDX-License-Identifier: AGPL-3.0-or-later
#' Test-inversion confidence set for an interval-identified scalar
#'
#' The confidence set is the sub-level set of the moment-inequality
#' criterion, and the reported interval is its projection onto the
#' parameter line. For a scalar with two inequalities at most one can bind
#' at any parameter value, so the criterion under the null is the square of
#' a single one-sided standard normal and the cutoff is
#' \code{z_(1-alpha)^2} in closed form: no bootstrap, and therefore the
#' same number in both language arms.
#'
#' Formula: \code{CS = {theta : Q_n(theta) <= z^2}} with
#' \code{Q_n(theta) = \[sqrt(n)(mL - theta)/sL\]_+^2 +
#' \[sqrt(n)(theta - mU)/sU\]_+^2}, whose endpoints are
#' \code{mL - z sL / sqrt(n)} and \code{mU + z sU / sqrt(n)}.
#'
#' @param theta Candidate parameter values to test.
#' @param moments Interval data, an (n, 2) matrix with the lower end in
#'   column 1 and the upper end in column 2.
#' @param alpha Miss probability, default 0.05.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{grid_lower}, \code{grid_upper}, \code{n_in_set}, \code{cutoff},
#'   \code{criterion_min}, \code{n}.
#' @references Romano, J. P. and Shaikh, A. M. (2008). Inference for
#'   identifiable parameters in partially identified econometric models.
#'   Journal of Statistical Planning and Inference 138(9), 2786-2807.
#'   \doi{10.1016/j.jspi.2008.03.015}. Criterion and level set as
#'   equations (4.2) and (4.10) of Molinari, F. (2021), Handbook of
#'   Econometrics 7A (arXiv:2004.11751 pp. 89, 97).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Bndinf(V, D)
Bndinf <- function(theta, moments, alpha = 0.05) {
  grid <- as.numeric(unlist(theta))
  if (length(grid) == 0L) stop("Bndinf: theta grid is empty")
  a <- as.numeric(alpha)[1]
  if (!(a > 0 && a < 1)) stop("Bndinf: alpha must lie in (0, 1)")
  iv <- .bnd_interval(moments, "Bndinf")
  st <- .bnd_mistats(iv$yl, iv$yu)
  z <- stats::qnorm(1 - a)
  cut <- z * z
  rn <- sqrt(st$n)
  lo <- st$mL - z * st$sL / rn
  hi <- st$mU + z * st$sU / rn
  q <- vapply(grid, .bnd_crit, 0, st = st)
  inset <- q <= cut
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             grid_lower = if (any(inset)) min(grid[inset]) else NA_real_,
             grid_upper = if (any(inset)) max(grid[inset]) else NA_real_,
             n_in_set = sum(inset), cutoff = cut,
             criterion_min = min(q), n = st$n,
             method = "Inference for partially identified parameters")
}
