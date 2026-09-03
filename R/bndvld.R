# SPDX-License-Identifier: AGPL-3.0-or-later
#' Validity check for a collection of bound assumptions
#'
#' Each maintained assumption delivers its own interval for the same scalar
#' parameter, so all of them together deliver the intersection. An empty
#' intersection refutes the assumptions jointly, which is the only sense in
#' which a partial-identification analysis can be falsified: a non-empty
#' region is consistent with the data by construction. A non-empty
#' intersection that excludes \code{theta_0} rejects the null.
#'
#' Formula: \code{L = max_i lower_i}, \code{U = min_i upper_i}; refuted iff
#' \code{L > U}; covers iff \code{L <= theta_0 <= U}.
#'
#' @param lower,upper Lower and upper end of each maintained bound, same
#'   length.
#' @param theta_0 Parameter value under test.
#' @param H0 Non-zero to test coverage of \code{theta_0} (the default);
#'   zero to report the intersection only, leaving \code{reject} at 0.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{refuted}, \code{covers}, \code{reject}, \code{n}.
#' @references Manski, C. F. (2003). Partial Identification of Probability
#'   Distributions. Springer. The refutation reading of an empty
#'   identification region is stated in Molinari, F. (2021), Handbook of
#'   Econometrics 7A (arXiv:2004.11751 p. 20).
#' @export
#' @examples
#' Bndvld(lower = c(1, 2, 3, 4, 5, 6, 7, 8), upper = c(1, 2, 3, 4, 5, 6, 7, 8), theta_0 =
#' c(1, 2, 3, 4, 5, 6, 7, 8))
Bndvld <- function(lower, upper, theta_0, H0 = 1) {
  lo <- as.numeric(unlist(lower))
  hi <- as.numeric(unlist(upper))
  if (length(lo) == 0L) stop("Bndvld: lower is empty")
  if (length(hi) != length(lo))
    stop("Bndvld: lower and upper must have the same length")
  L <- max(lo)
  U <- min(hi)
  t0 <- as.numeric(theta_0)[1]
  refuted <- if (L > U) 1 else 0
  covers <- if (L <= t0 && t0 <= U) 1 else 0
  reject <- if (as.numeric(H0)[1] != 0 && covers == 0) 1 else 0
  .t1_result(lower = L, upper = U, width = U - L, refuted = refuted,
             covers = covers, reject = reject, n = length(lo),
             method = "Validity check for bound assumptions")
}
