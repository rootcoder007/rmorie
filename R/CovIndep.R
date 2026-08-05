# SPDX-License-Identifier: AGPL-3.0-or-later

#' Covariance vanishing test for independence
#'
#' near_zero is the decision abs(Cov) <= tol.  A zero covariance is
#' necessary but not sufficient for independence.
#'
#' @param x,y equal-length data vectors, n >= 2.
#' @param tol absolute tolerance on the covariance.
#' @return list(cov, near_zero).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.63).
#' @examples
#' CovIndep(c(-1, 0, 1), c(1, 0, 1))$near_zero
#' @export
CovIndep <- function(x, y, tol = 1e-09) {
  cov <- CovShort(x, y)$cov
  tol <- as.numeric(tol)
  if (length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("tol must be a single value >= 0.", call. = FALSE)
  }
  list(cov = cov, near_zero = abs(cov) <= tol)
}
