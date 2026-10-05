# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Goodman-Bacon partition of the TWFE DiD (Prtdid). Thin documented
# alias of morie_did_bacon_decomposition(); the Python arm is
# morie.fn.prtdid (itself an exact-zero alias of morie.fn.gbacon).
# Anchored in test-did-parity.R against hand-computed fixture weights
# (0.3, 0.4, 0.1, 0.2) and 2x2 estimates (2.25, 1.75, 1.25, 0.25),
# with the Theorem 1 identity (weights sum to 1, recomposition equals
# the TWFE coefficient) checked as numbers.

#' Goodman-Bacon partition of the two-way fixed-effects DiD
#'
#' The TWFE DiD coefficient with staggered adoption is a weighted
#' average over the complete partition of the panel into timing-group
#' 2x2 designs (Goodman-Bacon 2021, Theorem 1, eqs. 7-9): each timing
#' cohort against never-treated units, earlier against later cohorts
#' before the later adopt, and later cohorts against already-treated
#' earlier ones -- the forbidden comparison. This function is a
#' documented alias of \code{\link{morie_did_bacon_decomposition}} and
#' returns its value unchanged.
#'
#' @param data Long-format balanced panel data frame.
#' @param outcome Name of the outcome column.
#' @param treatment Name of the absorbing 0/1 treatment column.
#' @param unit Name of the unit identifier column.
#' @param time Name of the period column.
#' @return List with \code{components} (one row per 2x2 comparison:
#'   treated cohort, control group, weight, estimate),
#'   \code{overall_estimate} (the recomposed TWFE coefficient) and
#'   \code{details}.
#' @references Goodman-Bacon, A. (2021). Difference-in-differences with variation in
#'   treatment timing. Journal of Econometrics, 225(2), 254-277.
#'   doi:10.1016/j.jeconom.2021.03.014. Implemented from Theorem 1 and eqs. (7)-(9) of
#'   NBER Working Paper 25018;
#' @seealso \code{\link{Gbtcom}} for the three-way composition by
#'   comparison type.
#' @examples
#' set.seed(21)
#' d <- expand.grid(time = 1:6, unit = 1:30)
#' first <- rep(c(3, 5, Inf), each = 10)[d$unit]
#' d$D <- as.integer(d$time >= first)
#' d$y <- d$unit / 10 + d$time / 5 + 1.5 * d$D + rnorm(nrow(d), sd = 0.2)
#' p <- Prtdid(d, "y", "D", "unit", "time")
#' p$overall_estimate
#' @export
Prtdid <- function(data, outcome, treatment, unit, time) {
  morie_did_bacon_decomposition(data, outcome, treatment, unit, time)
}
