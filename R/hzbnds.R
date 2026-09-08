# SPDX-License-Identifier: AGPL-3.0-or-later
#' Horowitz-Manski bounds and the contrast against the MAR point estimate
#'
#' Horowitz, J. L. and Manski, C. F. (2000), "Nonparametric analysis of
#' randomized experiments with missing covariate and outcome data", Journal of
#' the American Statistical Association 95(449):77-84,
#' doi:10.1080/01621459.2000.10473902; Manski, C. F. (2003), Partial
#' Identification of Probability Distributions, Springer.
#'
#' Missing at random makes E\[Y | R = 0\] = E\[Y | R = 1\], which point identifies
#' E\[Y\] at the complete-case mean.  Dropping that assumption leaves only the
#' support restriction y_min <= Y <= y_max, and the identified set becomes
#' \[m p + y_min (1 - p), m p + y_max (1 - p)\] with m = E\[Y | R = 1\] and
#' p = P(R = 1).  This function reports both, plus the two contrasts (how far
#' the MAR answer sits from each end of the identified set), which is the
#' quantity a sensitivity analysis actually wants: it is the amount of
#' departure from MAR the data cannot rule out.
#'
#' @param y Outcomes; entries at which R = 0 are never read.
#' @param R Response indicator, 1 observed and 0 missing.
#' @param y_min,y_max A priori support limits.
#' @return list: estimate, mar_estimate, lower, upper, width, contrast_lower,
#'   contrast_upper, p_observed, n, n_observed, method.
#' @keywords internal
#' @examples
#' Hzbnds(c(1, 2, 3, 0), c(1, 1, 1, 0), 0, 10)$contrast_upper
#' @export
Hzbnds <- function(y, R, y_min, y_max) {
  yy <- .s03vec(y)
  rr <- .s03vec(R)
  n <- length(yy)
  if (n == 0L) stop("horowitz_manski_bounds: y is empty")
  if (length(rr) != n) stop("horowitz_manski_bounds: y and R have different lengths")
  lo <- as.numeric(y_min)
  hi <- as.numeric(y_max)
  if (!(hi >= lo)) stop("horowitz_manski_bounds: y_max must be at least y_min")
  nobs <- 0L
  s <- 0
  for (i in seq_len(n)) {
    if (rr[i] != 0 && rr[i] != 1) stop("horowitz_manski_bounds: R must be 0 or 1")
    if (rr[i] == 1) {
      nobs <- nobs + 1L
      s <- s + yy[i]
    }
  }
  if (nobs == 0L) {
    stop("horowitz_manski_bounds: no observed outcome, the MAR estimate is undefined")
  }
  p <- nobs / n
  m <- s / nobs
  lower <- m * p + lo * (1 - p)
  upper <- m * p + hi * (1 - p)
  list(estimate = m, mar_estimate = m, lower = lower, upper = upper,
       width = upper - lower, contrast_lower = m - lower,
       contrast_upper = upper - m, p_observed = p, n = n, n_observed = nobs,
       method = "Horowitz-Manski (2000) identified set vs the MAR point estimate")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hzbnds
#' @keywords internal
#' @export
morie_horowitz_manski_bounds <- Hzbnds
