# SPDX-License-Identifier: AGPL-3.0-or-later

#' Sum of the first N integers
#'
#' 1 + 2 + ... + N = N(N+1)/2.  Eq (1.31) is the induction step: adding
#' (N+1) gives (N+1)(N+2)/2, the same formula with N -> N+1.
#'
#' @param N upper limit, N >= 0.
#' @return list(n, explicit_sum, closed_form, next_closed_form, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq. (1.31).
#' @examples
#' SumInts(6)$closed_form
#' @export
SumInts <- function(N) {
  N <- as.integer(N)
  if (is.na(N) || N < 0L) {
    stop("N must be a non-negative integer.", call. = FALSE)
  }
  explicit <- if (N == 0L) 0 else sum(as.numeric(seq_len(N)))
  closed <- as.numeric(N) * (as.numeric(N) + 1) / 2
  nxt <- (as.numeric(N) + 1) * (as.numeric(N) + 2) / 2
  if (abs(explicit - closed) > 1e-6 * max(1, closed) ||
      abs(closed + (as.numeric(N) + 1) - nxt) > 1e-6 * max(1, nxt)) {
    stop("the induction step failed.", call. = FALSE)
  }
  list(n = as.numeric(N), explicit_sum = explicit, closed_form = closed,
       next_closed_form = nxt, forms_agree = 1)
}
