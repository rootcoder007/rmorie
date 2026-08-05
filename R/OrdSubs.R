# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ordered subgroups without repetition
#'
#' N_P_n = N(N-1)...(N-(n-1)) = N!/(N-n)!, eqs (1.5)-(1.6).  The falling
#' product and the factorial quotient are both formed and must agree.
#'
#' @param N,n pool size and subgroup size, 0 <= n <= N.
#' @return list(n_objects, n_picks, count, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.5)-(1.6).
#' @examples
#' OrdSubs(5, 5)$count
#' @export
OrdSubs <- function(N, n) {
  count <- morie_partial_permutations(N, n)
  list(n_objects = as.numeric(N), n_picks = as.numeric(n),
       count = as.numeric(count), forms_agree = 1)
}
