# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ordered triples without repetition
#'
#' Of the N^3 ordered triples, 3N^2 - 2N have a repeated entry, so the
#' repeat-free count is N^3 - (3N^2 - 2N), which eq (1.30) asserts
#' equals the falling product N(N-1)(N-2) of eq (1.6).
#'
#' @param N number of distinct objects, N >= 0.
#' @return list(n_objects, total, with_repeat, no_repeat, falling_product,
#'   forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq. (1.30).
#' @examples
#' TripNoRep(6)$no_repeat
#' @export
TripNoRep <- function(N) {
  N <- as.integer(N)
  if (is.na(N) || N < 0L) {
    stop("N must be a non-negative integer.", call. = FALSE)
  }
  total <- as.numeric(N)^3
  with_repeat <- 3 * as.numeric(N)^2 - 2 * as.numeric(N)
  no_repeat <- total - with_repeat
  falling <- as.numeric(N) * (as.numeric(N) - 1) * (as.numeric(N) - 2)
  if (abs(no_repeat - falling) > 1e-6 * max(1, abs(falling))) {
    stop("eq (1.30) identity failed.", call. = FALSE)
  }
  list(n_objects = as.numeric(N), total = total, with_repeat = with_repeat,
       no_repeat = no_repeat, falling_product = falling, forms_agree = 1)
}
