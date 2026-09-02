# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rank-based score that only cares where the first hit lands
#'
#' The reciprocal makes the metric top-heavy: moving the first relevant
#' item from rank two to rank one is worth as much as moving it from ten
#' to two. That suits question answering, where there is one right
#' answer, and suits recommendation badly, where there are many.
#'
#' Formula: \code{MRR = (1/|Q|) sum_q 1/rank_q}.
#'
#' @param pred_rank Ranked item ids per query, best first.
#' @param relevant Relevant item id(s) per query.
#' @return List with \code{estimate}, \code{rr}, \code{n_hit}, \code{Q}.
#' @references Voorhees, E. M. (1999). TREC-8 question answering track
#'   report, 77-82.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Mrr(V, V)
Mrr <- function(pred_rank, relevant) {
  P_ <- as.matrix(pred_rank); R_ <- as.matrix(relevant)
  Q <- nrow(P_)
  rr <- numeric(Q)
  for (q in seq_len(Q)) {
    hit <- which(P_[q, ] %in% R_[q, ])
    rr[q] <- if (length(hit)) 1 / hit[1] else 0
  }
  .t1_result(estimate = sum(rr) / Q, rr = rr, n_hit = sum(rr > 0), Q = Q,
             method = "Mean reciprocal rank")
}
