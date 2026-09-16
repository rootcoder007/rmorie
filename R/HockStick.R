# SPDX-License-Identifier: AGPL-3.0-or-later

#' Hockey-stick identity
#'
#' C(n,k) = C(n-1,k-1) + C(n-2,k-1) + ... + C(k-1,k-1), eq (1.29),
#' copied as eq (1.56) inside the stars-and-bars induction.
#'
#' @param n,k requires 1 <= k <= n.
#' @return list(n, k, stick_sum, closed_form, n_terms, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.29), (1.56).
#' @examples
#' HockStick(6, 3)$stick_sum
#' @export
HockStick <- function(n, k) {
  res <- morie_hockey_stick(n, k)
  total <- as.numeric(res[[1]])
  closed <- as.numeric(res[[2]])
  if (abs(total - closed) > 1e-6 * max(1, closed)) {
    stop("hockey-stick sum and closed form disagree.", call. = FALSE)
  }
  list(n = as.numeric(n), k = as.numeric(k), stick_sum = total,
       closed_form = closed, n_terms = as.numeric(as.integer(n) - as.integer(k) + 1L),
       forms_agree = 1)
}
