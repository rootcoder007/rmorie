# SPDX-License-Identifier: AGPL-3.0-or-later

#' ColBERT late interaction
#'
#' Formula: sum_q max_d q_i . d_j
#'
#' Every query token keeps its own vector and is scored against its
#' best-matching document token; the sum of those maxima is the document
#' score.  Because the interaction happens after both sides are encoded,
#' document vectors can be indexed offline -- that is the "late" in late
#' interaction.  With unit-normalised vectors a document containing the
#' query exactly scores n_q, the largest value possible.
#'
#' @param query An nq x d matrix of query token embeddings.
#' @param docs A list of documents, each an nd x d matrix.
#' @return List with \code{estimate}, \code{scores}, \code{ranking},
#'   \code{best}, \code{max_sim}, \code{nq}, \code{n_docs},
#'   \code{method}.
#' @references Khattab & Zaharia (2020), ColBERT, SIGIR 2020:39-48.
#' @export
Colbrt <- function(query, docs) {
  Q <- .s03mat(query)
  nq <- nrow(Q)
  if (nq == 0L) stop("empty input: query has no tokens")
  d <- ncol(Q)
  Qn <- matrix(0, nq, d)
  for (i in seq_len(nq)) Qn[i, ] <- .clip_l2norm(Q[i, ])
  if (is.null(docs) || !length(docs))
    stop("docs must hold at least one document")
  dl <- if (is.list(docs)) docs else list(docs)
  scores <- numeric(length(dl))
  maxsim <- matrix(0, length(dl), nq)
  for (q in seq_along(dl)) {
    D <- .s03mat(dl[[q]])
    if (!nrow(D)) stop("a document has no tokens")
    if (ncol(D) != d) stop("query and document dimensions disagree")
    Dn <- matrix(0, nrow(D), d)
    for (j in seq_len(nrow(D))) Dn[j, ] <- .clip_l2norm(D[j, ])
    s <- 0
    for (i in seq_len(nq)) {
      best <- NA_real_
      for (j in seq_len(nrow(Dn))) {
        v <- 0
        for (k in seq_len(d)) v <- v + Qn[i, k] * Dn[j, k]
        if (is.na(best) || v > best) best <- v
      }
      maxsim[q, i] <- best
      s <- s + best
    }
    scores[q] <- s
  }
  order_ <- order(-scores, seq_along(scores))
  .t1_result(estimate = scores[order_[1]], scores = scores,
             ranking = order_ - 1L, best = order_[1] - 1L,
             max_sim = maxsim, nq = nq, n_docs = length(dl),
             method = "ColBERT late-interaction retrieval scoring")
}
