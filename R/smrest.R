# SPDX-License-Identifier: AGPL-3.0-or-later
#' Standardised mortality ratio, indirect standardisation
#'
#' Formula: SMR = O / E; exact Poisson limits chi2_\{alpha/2, 2O\}/2 and chi2_\{1-alpha/2, 2(O+1)\}/2, divided by E
#'
#' @param observed Observed deaths.
#' @param expected Expected deaths under the reference rates.
#' @param alpha Two-sided significance level.

#' @param observed See Usage.
#' @param expected See Usage.
#' @param alpha See Usage.
#' @return List with ``smr``, ``ci_lower``, ``ci_upper``, ``observed``, ``expected``.
#' @references Breslow and Day (1987), Statistical Methods in Cancer Research Volume II: The Design and Analysis of Cohort Studies, IARC. Not held locally; SMR = O/E with exact Poisson limits on O is the standard published form, and is what the existing morie.fn.smr implements.
#' @export
#' @examples
#' Smrind(observed = 5L, expected = 5L)
Smrind <- function(observed, expected, alpha = 0.05) {
  o <- as.numeric(observed)
  e <- as.numeric(expected)
  if (o < 0) stop("observed must be non-negative")
  if (e <= 0) stop("expected must be positive")
  lo <- if (o == 0) 0 else stats::qchisq(alpha / 2, 2 * o) / 2
  hi <- stats::qchisq(1 - alpha / 2, 2 * (o + 1)) / 2
  .t1_result(smr = o / e, ci_lower = lo / e, ci_upper = hi / e,
             observed = o, expected = e,
             method = "Standardised mortality ratio (indirect)")
}
