# SPDX-License-Identifier: AGPL-3.0-or-later
#' Design-corrected p-value for a naively computed chi-square statistic
#'
#' A statistic computed from complex-survey data while pretending the
#' sample was simple and random is inflated by roughly the design effect,
#' and its nominal p-value is correspondingly too small.  The first-order
#' correction is a single division; it is what Korn and Graubard
#' recommend when only a summary design effect is available, and it is
#' exactly Rao and Scott's first-order adjustment when the design effect
#' used is the mean generalized design effect.
#'
#' Formula: X2_adj = X2 / deff, p = P(chi2_df > X2_adj).
#'
#' @param test_stat Non-negative naive chi-square statistic.
#' @param DEFF Positive design effect; 1 returns the uncorrected p-value.
#' @param df Degrees of freedom, at least 1.
#' @return List with \code{estimate} (corrected p-value), \code{p_naive},
#'   \code{statistic}, \code{statistic_naive}, \code{deff}, \code{df},
#'   \code{inflation}, \code{method}.
#' @references Korn, E. L. and Graubard, B. I. (1999). Analysis of Health
#'   Surveys. Wiley, chapter 3. \doi{10.1002/9781118032619}
#' @examples
#' Survip(3.841458820694124, 1, 1)
#' @export
Survip <- function(test_stat, DEFF = 1, df = 1) {
  x <- as.numeric(test_stat); d <- as.numeric(DEFF); k <- as.integer(df)
  if (x < 0) stop("survey_p_value: test_stat must be non-negative")
  if (d <= 0) stop("survey_p_value: DEFF must be positive")
  if (k < 1L) stop("survey_p_value: df must be at least 1")
  adj <- x / d
  p0 <- stats::pchisq(x, k, lower.tail = FALSE)
  p1 <- stats::pchisq(adj, k, lower.tail = FALSE)
  list(estimate = as.numeric(p1), p_naive = as.numeric(p0),
       statistic = as.numeric(adj), statistic_naive = x, deff = d,
       df = k, inflation = if (p0 > 0) as.numeric(p1 / p0) else Inf,
       method = "first-order design correction X2/deff [Korn & Graubard 1999]")
}

# CANONICAL TEST
# stopifnot(abs(Survip(qchisq(0.95, 1), 1, 1)$estimate - 0.05) < 1e-9)
