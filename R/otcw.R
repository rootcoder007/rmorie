# SPDX-License-Identifier: AGPL-3.0-or-later
#' Test whether a matching can be improved by re-shuffling a cycle
#'
#' Cyclical monotonicity is the combinatorial fingerprint of optimality:
#' a coupling is optimal exactly when no finite cycle of reassignments
#' lowers the cost. Checking every permutation is factorial, so the test
#' here is over all transpositions, the two-cycle case.
#'
#' Formula: for all \code{(i, j)},
#' \code{c(x_s(i), y_i) + c(x_s(j), y_j) <= c(x_s(j), y_i) + c(x_s(i), y_j)}.
#'
#' @param X,Y Point sets, kept for interface symmetry.
#' @param Cost Cost matrix with \code{Cost\[i, j\] = c(x_i, y_j)}.
#' @param perm Zero-based assignment: \code{y_i} matched to \code{x_perm\[i\]}.
#' @return List with \code{is_cm}, \code{slack}, \code{estimate}, \code{n}.
#' @references Villani, C. (2003). AMS GSM 58, theorem 2.12.
#' @export
Otcw <- function(X, Y, Cost, perm) {
  M <- as.matrix(Cost)
  p <- as.integer(round(as.numeric(perm))) + 1L
  n <- length(p)
  total <- sum(vapply(seq_len(n), function(i) M[p[i], i], 0))
  worst <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (j <= i) next
    d <- (M[p[j], i] + M[p[i], j]) - (M[p[i], i] + M[p[j], j])
    if (d < worst) worst <- d
  }
  .t1_result(is_cm = as.numeric(worst >= 0), slack = worst, estimate = total,
             n = n, method = "Cyclical monotonicity over transpositions")
}
