# SPDX-License-Identifier: AGPL-3.0-or-later
#' Asymmetric set similarity with separate penalties per side
#'
#' Human similarity judgements are not symmetric, and a symmetric
#' coefficient cannot express that. The two weights let the two kinds of
#' mismatch cost different amounts. Both weights 1 recovers
#' Jaccard-Tanimoto, both 0.5 recovers Dice.
#'
#' Formula: \code{S = |A n B| / (|A n B| + alpha |A \ B| + beta |B \ A|)}.
#'
#' @param fp_a Binary fingerprint A; non-zero counts as present.
#' @param fp_b Binary fingerprint B, same length.
#' @param alpha Weight on features unique to A.
#' @param beta Weight on features unique to B.
#' @return List with \code{estimate}, \code{common}, \code{only_a},
#'   \code{only_b}, \code{n_bits}.
#' @references Tversky, A. (1977). Features of similarity. Psychological
#'   Review 84:327-352, equation (5).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Tvsbn(V, V)
Tvsbn <- function(fp_a, fp_b, alpha = 0.5, beta = 0.5) {
  a <- as.numeric(unlist(fp_a)); b <- as.numeric(unlist(fp_b))
  common <- sum(a != 0 & b != 0)
  only_a <- sum(a != 0 & b == 0)
  only_b <- sum(a == 0 & b != 0)
  den <- common + alpha * only_a + beta * only_b
  s <- if (den > 0) common / den else NaN
  .t1_result(estimate = s, common = common, only_a = only_a, only_b = only_b,
             n_bits = length(a), method = "Tversky similarity index")
}
