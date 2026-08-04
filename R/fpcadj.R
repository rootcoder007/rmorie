# SPDX-License-Identifier: AGPL-3.0-or-later
#' Finite population correction factor for sampling without replacement.
#'
#' The factor multiplies the VARIANCE; the standard error carries its
#' square root, returned separately because that is the one that gets
#' misapplied.
#'
#' Formula: f = n/N; fpc = (N - n)/N = 1 - f;
#'   V(ybar) = (1 - f) S^2 / n, se scales by sqrt(1 - f)
#'
#' @param N Population size (may be \code{Inf}).
#' @param n Sample size, 1 <= n <= N.
#' @return List with \code{fpc}, \code{se_factor}, \code{fraction},
#'   \code{N}, \code{n}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   2, where V(ybar) = (S^2/n)(N - n)/N for simple random sampling
#'   without replacement. Cross-checked against the reference
#'   implementation in the CRAN package samplingbook 1.2.4, whose Smean
#'   uses the variance (N - n)/N * (1/(n(n-1))) sum (y - ybar)^2.
#' @export
Fpc <- function(N, n) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1")
  N <- as.numeric(N)
  if (N < n) stop("N must be at least n")
  if (is.infinite(N)) { f <- 0; k <- 1 } else { f <- n / N; k <- (N - n) / N }
  .t1_result(fpc = k, se_factor = sqrt(k), fraction = f, N = N, n = n,
             method = "Finite population correction (1 - n/N)")
}
