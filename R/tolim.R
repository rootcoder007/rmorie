# SPDX-License-Identifier: AGPL-3.0-or-later

#' Distribution-free (Wilks) tolerance limits
#'
#' Closed-form Wilks (1941) tolerance-interval probability that the
#' sample interval from min(x) to max(x) covers at least `coverage` of the
#' population.  Gibbons & Chakraborti Ch 2.11.
#'
#' P(coverage of (X_(1), X_(n)) >= beta) =
#'    1 - n * beta^(n-1) + (n - 1) * beta^n
#'
#' @param x Numeric vector.
#' @param coverage Desired population coverage `beta` (default 0.90).
#' @param confidence Desired confidence (default 0.95).
#' @return Named list: lower, upper, coverage_requested,
#'   confidence_achieved, n, method.
#' @references Wilks (1941); Gibbons & Chakraborti (6e) Ch 2.11.
#' @export
#' @examples
#' morie_tolerance_limits(1:100, coverage = 0.90, confidence = 0.95)
morie_tolerance_limits <- function(x, coverage = 0.90, confidence = 0.95) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2) {
    return(list(
      lower = NA_real_, upper = NA_real_,
      coverage_requested = coverage,
      confidence_achieved = NA_real_, n = n,
      method = "Distribution-free tolerance limits (Wilks)"
    ))
  }
  beta <- coverage
  conf_ach <- 1 - n * beta^(n - 1) + (n - 1) * beta^n
  conf_ach <- max(0, min(1, conf_ach))
  # Wilks' limits are the sample extremes whatever confidence is asked
  # for, so the requested level is a claim to be checked, not a knob.
  # Saying nothing when n cannot support it hands back an interval that
  # does not have the coverage the caller specified.
  if (conf_ach < confidence) {
    warning(sprintf(
      paste("Sample size n=%d too small: achieved confidence %.4f <",
            "requested %.4f for coverage %.2f."),
      n, conf_ach, confidence, beta), call. = FALSE)
  }
  list(
    lower = min(x),
    upper = max(x),
    coverage_requested = beta,
    confidence_achieved = conf_ach,
    n = n,
    method = "Distribution-free tolerance limits (Wilks)"
  )
}
