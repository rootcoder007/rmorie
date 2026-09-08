# SPDX-License-Identifier: AGPL-3.0-or-later

#' Stars-and-bars recursion
#'
#' Grouping unordered sets by how many times one chosen letter appears
#' gives N_U_n = sum_\{j=0\}^\{n\} (N-1)_U_j -- eq (1.54) in general,
#' eq (1.51) for n=4, N=3 and eq (1.52) for general n with N=3.
#' Eq (1.55) rewrites each term with eq (1.16); the sum is returned
#' beside the closed form.
#'
#' @param n number of picks.
#' @param N number of distinct types, N >= 2.
#' @return list(n_picks, n_types, recursion_sum, closed_form, n_terms,
#'   forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.51)-(1.52), (1.54)-(1.55).
#' @examples
#' StarBRec(4, 3)$recursion_sum
#' @export
StarBRec <- function(n, N) {
  n <- as.integer(n)
  N <- as.integer(N)
  if (is.na(n) || is.na(N) || n < 0L || N < 0L) {
    stop("n and N must be non-negative integers.", call. = FALSE)
  }
  if (N < 2L) stop("the recursion needs N >= 2.", call. = FALSE)
  total <- sum(vapply(0:n, function(j) morie_stars_bars(j, N - 1L), numeric(1)))
  closed <- morie_stars_bars(n, N)
  if (abs(total - closed) > 1e-6 * max(1, closed)) {
    stop("recursion and closed form disagree.", call. = FALSE)
  }
  list(n_picks = as.numeric(n), n_types = as.numeric(N),
       recursion_sum = as.numeric(total), closed_form = as.numeric(closed),
       n_terms = as.numeric(n + 1L), forms_agree = 1)
}
