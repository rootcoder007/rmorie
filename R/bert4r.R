# SPDX-License-Identifier: AGPL-3.0-or-later
#' BERT4Rec: the masked-item objective and its evaluation
#'
#' Sun et al. (2019), BERT4Rec: sequential recommendation with
#' bidirectional encoder representations from transformer, CIKM 28,
#' 1441-1450 (arXiv:1904.06690 -- FETCHED).  The objective is the cloze
#' task: a proportion rho of the items in each sequence is replaced by
#' \[mask\] and the loss is the mean over masked positions of -log P(v_m =
#' v*_m | S'_u).  At test time exactly one \[mask\] is appended to the end
#' of the sequence, the paper's device for turning a bidirectional model
#' into a next-item recommender.
#'
#' Determinism: masked positions are chosen on a fixed stride giving the
#' fraction rho, not drawn.  The paper's random masking is a training
#' device; the loss it defines is what is computed here.
#'
#' @param seqs user sequences of zero-based item ids, one row per user.
#' @param K cut-off for HR@K and NDCG@K.
#' @param scores optional model scores at each masked position.
#' @param rho masking proportion.
#' @return list: estimate, loss, hr, ndcg, n_masked, n_items, method.
#' @keywords internal
#' @examples
#' Bertrec(matrix(c(0, 1, 2, 1, 2, 0), 2, 3, byrow = TRUE))$loss
#' @export
Bertrec <- function(seqs, K = 10, scores = NULL, rho = 0.2) {
  eps <- 1e-300
  S <- .s03mat(seqs)
  V <- 0L
  for (i in seq_len(nrow(S))) for (j in seq_len(ncol(S))) {
    if (as.integer(S[i, j]) + 1L > V) V <- as.integer(S[i, j]) + 1L
  }
  total <- 0; nm <- 0L; hits <- 0; ndcg <- 0
  for (u in seq_len(nrow(S))) {
    row <- S[u, ]
    L <- length(row)
    step <- if (as.numeric(rho) > 0) as.integer(1 / as.numeric(rho)) else L
    if (step < 1L) step <- 1L
    pos <- if (step > L) integer(0) else seq.int(step, L, by = step)
    for (j in seq_along(pos)) {
      target <- as.integer(row[pos[j]])
      if (is.null(scores)) {
        p <- if (V) 1 / V else 0
        rank <- (V + 1) / 2
      } else {
        sc <- .s03vec(scores[[u]][[j]])
        pr <- .s03softmax(sc)
        p <- pr[target + 1L]
        rank <- 1
        for (cc in seq_along(sc)) if (sc[cc] > sc[target + 1L]) rank <- rank + 1
      }
      total <- total - log(max(p, eps))
      nm <- nm + 1L
      if (rank <= as.numeric(K)) {
        hits <- hits + 1
        ndcg <- ndcg + 1 / log(rank + 1, base = 2)
      }
    }
  }
  list(estimate = if (nm) total / nm else NaN, loss = if (nm) total / nm else NaN,
       hr = if (nm) hits / nm else NaN, ndcg = if (nm) ndcg / nm else NaN,
       n_masked = nm, n_items = V,
       method = "BERT4Rec cloze loss with HR@K and NDCG@K (Sun et al. 2019)")
}
