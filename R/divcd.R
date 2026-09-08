# SPDX-License-Identifier: AGPL-3.0-or-later
#' Divergent transition count and rate
#'
#' Formula: rate = n_divergent / n_total
#'
#' @param divergent Per-iteration 0/1 divergence indicators; a list of lists is one chain per row.

#' @param divergent See Usage.
#' @return List with ``count``, ``rate``, ``per_chain``, ``per_chain_rate``, ``any``, ``n``.
#' @references Betancourt (2017), A Conceptual Introduction to Hamiltonian Monte Carlo,
#' arXiv:1701.02434, Section 6.2: divergent transitions are 'extremely sensitive
#' identifiers' of the pathological neighbourhoods a trajectory failed to explore.
#' Verified against the paper.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Divrate(V)
Divrate <- function(divergent) {
  D <- if (is.matrix(divergent)) lapply(seq_len(nrow(divergent)), function(i) divergent[i, ])
       else if (is.list(divergent)) lapply(divergent, .t1_vec) else list(.t1_vec(divergent))
  per <- vapply(D, function(r) sum(r != 0), numeric(1))
  tot <- sum(vapply(D, length, numeric(1)))
  cnt <- sum(per)
  .t1_result(count = cnt, rate = if (tot > 0) cnt / tot else NA_real_,
             per_chain = per,
             per_chain_rate = per / vapply(D, length, numeric(1)),
             any = cnt > 0, n = tot,
             method = "Divergent transition count and rate")
}
