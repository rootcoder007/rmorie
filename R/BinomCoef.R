# SPDX-License-Identifier: AGPL-3.0-or-later

#' Unordered subgroups: the binomial coefficient
#'
#' C(N, n) = N!/(n!(N-n)!) = N_P_n / n!, eqs (1.7)-(1.8).  The ordered
#' count is returned as well and n! C(N,n) = N_P_n is checked.
#'
#' @param N,n pool size and subgroup size, 0 <= n <= N.
#' @return list(n_objects, n_picks, count, ordered_count, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.7)-(1.8).
#' @examples
#' BinomCoef(52, 5)$count
#' @export
BinomCoef <- function(N, n) {
  N <- as.integer(N)
  n <- as.integer(n)
  if (is.na(N) || is.na(n) || N < 0L || n < 0L) {
    stop("N and n must be non-negative integers.", call. = FALSE)
  }
  if (n > N) stop("n cannot exceed N.", call. = FALSE)
  count <- choose(N, n)
  ordered <- morie_partial_permutations(N, n)
  if (abs(factorial(n) * count - ordered) > 1e-6 * max(1, ordered)) {
    stop("n! C(N,n) does not equal N_P_n.", call. = FALSE)
  }
  list(n_objects = as.numeric(N), n_picks = as.numeric(n),
       count = as.numeric(count), ordered_count = as.numeric(ordered),
       forms_agree = 1)
}
