# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intra-list diversity of a recommendation list
#'
#' Ziegler, McNee, Konstan and Lausen (2005), Improving recommendation
#' lists through topic diversification, WWW 14, 22-32, define the
#' intra-list similarity of a list as the sum of pairwise similarities
#' halved; the diversity is the complementary average over the C(k, 2)
#' unordered pairs, which is this module's formula line.  The 2005
#' proceedings were not retrievable here; the definition is quoted in its
#' standard published form.  The pair count is reported so the average is
#' interpretable when the list is short.
#'
#' @param list indices of the recommended items (zero-based).
#' @param sim_matrix item-item similarity.
#' @return list: estimate, ils, n_pairs, min_pair_sim, max_pair_sim,
#'   method.
#' @keywords internal
#' @examples
#' S <- matrix(c(1, 0.2, 0.5, 0.2, 1, 0.1, 0.5, 0.1, 1), 3, 3)
#' Intradiv(c(0, 1, 2), S)$estimate
#' @export
Intradiv <- function(list, sim_matrix = NULL) {
  items <- as.integer(list) + 1L
  S <- .s03mat(sim_matrix)
  kk <- length(items)
  tot <- 0; ils <- 0; np <- 0L; lo <- Inf; hi <- -Inf
  if (kk > 1L) for (a in seq_len(kk - 1L)) for (b in seq(a + 1L, kk)) {
    s <- S[items[a], items[b]]
    tot <- tot + 1 - s
    ils <- ils + s
    np <- np + 1L
    if (s < lo) lo <- s
    if (s > hi) hi <- s
  }
  list(estimate = if (np) tot / np else NaN, ils = ils, n_pairs = np,
       min_pair_sim = if (np) lo else NaN, max_pair_sim = if (np) hi else NaN,
       method = "Intra-list diversity, the complement of Ziegler et al. (2005) ILS")
}
