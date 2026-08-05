# SPDX-License-Identifier: AGPL-3.0-or-later

#' Stars and bars
#'
#' Unordered picks of n objects from N types with repetition allowed:
#' N_U_n = C(n + (N-1), N-1), eq (1.16).  The equal form C(n+N-1, n) is
#' computed as a cross-check.  Worked cases: eq (1.17) n=10, N=4 -> 286;
#' eq (1.48) n=2, N=6 -> 21; eq (1.49) n=2 -> N(N+1)/2; eq (1.50) N=2 ->
#' n+1; eq (1.53) N=3 -> (n+1)(n+2)/2.
#'
#' @param n number of picks.
#' @param N number of distinct types, N >= 1.
#' @return list(n_picks, n_types, count, count_alt, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.16)-(1.17), (1.48)-(1.50), (1.53).
#' @examples
#' StarBars(10, 4)$count
#' @export
StarBars <- function(n, N) {
  count <- morie_stars_bars(n, N)
  alt <- choose(as.integer(n) + as.integer(N) - 1L, as.integer(n))
  if (abs(count - alt) > 1e-6 * max(1, count)) {
    stop("the two stars-and-bars forms disagree.", call. = FALSE)
  }
  list(n_picks = as.numeric(n), n_types = as.numeric(N),
       count = as.numeric(count), count_alt = as.numeric(alt),
       forms_agree = 1)
}
