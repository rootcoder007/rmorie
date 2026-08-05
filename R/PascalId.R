# SPDX-License-Identifier: AGPL-3.0-or-later

#' Pascal's addition rule
#'
#' C(n,k) = C(n-1,k-1) + C(n-1,k), eq (1.28).  Eq (1.60) proves it by
#' writing the right-hand terms as (n-1)!/((k-1)!(n-k)!) and
#' (n-1)!/(k!(n-k-1)!), whose common-denominator sum is n!/(k!(n-k)!).
#'
#' @param n,k requires 1 <= k <= n - 1.
#' @return list(n, k, lhs, rhs, term_left, term_right, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.28), (1.60).
#' @examples
#' PascalId(5, 2)$lhs
#' @export
PascalId <- function(n, k) {
  n <- as.integer(n)
  k <- as.integer(k)
  if (is.na(n) || is.na(k) || n < 0L || k < 0L) {
    stop("n and k must be non-negative integers.", call. = FALSE)
  }
  if (k < 1L || k > n - 1L) {
    stop("Pascal's rule needs 1 <= k <= n - 1.", call. = FALSE)
  }
  left <- choose(n - 1L, k - 1L)
  right <- choose(n - 1L, k)
  lhs <- choose(n, k)
  if (abs(left + right - lhs) > 1e-6 * max(1, lhs)) {
    stop("Pascal's rule failed.", call. = FALSE)
  }
  list(n = as.numeric(n), k = as.numeric(k), lhs = as.numeric(lhs),
       rhs = as.numeric(left + right), term_left = as.numeric(left),
       term_right = as.numeric(right), forms_agree = 1)
}
