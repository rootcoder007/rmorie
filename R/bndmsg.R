# SPDX-License-Identifier: AGPL-3.0-or-later
#' Worst-case bound on a mean under a missing outcome
#'
#' Horowitz, J. L. and Manski, C. F. (2000), "Nonparametric analysis of
#' randomized experiments with missing covariate and outcome data", Journal of
#' the American Statistical Association 95(449):77-84,
#' doi:10.1080/01621459.2000.10473902; and Manski, C. F. (2003), Partial
#' Identification of Probability Distributions, Springer.
#'
#' With R = 1 when Y is observed and R = 0 when it is not, the law of total
#' expectation splits the target into an identified and an unidentified piece,
#' E\[Y\] = E\[Y | R = 1\] P(R = 1) + E\[Y | R = 0\] P(R = 0), and only the second
#' factor of the second term is unknown.  Bounding the unobserved conditional
#' mean by the a priori support \[y_min, y_max\] gives the sharp worst-case
#' interval L = E\[Y | R = 1\] P(R = 1) + y_min P(R = 0) and
#' U = E\[Y | R = 1\] P(R = 1) + y_max P(R = 0), whose width is exactly
#' (y_max - y_min) P(R = 0).  Nothing is assumed about the missingness
#' mechanism: the interval is the identified set.
#'
#' @param y Outcomes.  Entries at which R = 0 are never read.
#' @param R Response indicator, 1 for observed and 0 for missing.
#' @param y_min,y_max A priori lower and upper limits of the support of Y.
#' @return list: estimate, lower, upper, width, p_observed, mean_observed, n,
#'   n_observed, method.
#' @keywords internal
#' @examples
#' Bndmsg(c(1, 2, 3, 0), c(1, 1, 1, 0), 0, 10)$width
#' @export
Bndmsg <- function(y, R, y_min, y_max) {
  yy <- .s03vec(y)
  rr <- .s03vec(R)
  n <- length(yy)
  if (n == 0L) stop("bound_missing_outcome: y is empty")
  if (length(rr) != n) stop("bound_missing_outcome: y and R have different lengths")
  lo <- as.numeric(y_min)
  hi <- as.numeric(y_max)
  if (!(hi >= lo)) stop("bound_missing_outcome: y_max must be at least y_min")
  nobs <- 0L
  s <- 0
  for (i in seq_len(n)) {
    if (rr[i] != 0 && rr[i] != 1) stop("bound_missing_outcome: R must be 0 or 1")
    if (rr[i] == 1) {
      nobs <- nobs + 1L
      s <- s + yy[i]
    }
  }
  p <- nobs / n
  m <- if (nobs > 0L) s / nobs else 0
  lower <- m * p + lo * (1 - p)
  upper <- m * p + hi * (1 - p)
  list(estimate = 0.5 * (lower + upper), lower = lower, upper = upper,
       width = upper - lower, p_observed = p, mean_observed = m,
       n = n, n_observed = nobs,
       method = paste0("Horowitz-Manski (2000) worst-case bound; ",
                       "width = (y_max - y_min) P(R = 0)"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Bndmsg
#' @keywords internal
#' @export
morie_bound_missing_outcome <- Bndmsg
