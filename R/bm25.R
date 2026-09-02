# SPDX-License-Identifier: AGPL-3.0-or-later
#' Okapi BM25 ranking function
#'
#' Robertson, Walker, Jones, Hancock-Beaulieu and Gatford (1995), "Okapi at
#' TREC-3", Proceedings of the Third Text REtrieval Conference (TREC-3), NIST
#' Special Publication 500-225, 109-126; the modern statement of the same
#' formula is Robertson and Zaragoza (2009), "The probabilistic relevance
#' framework: BM25 and beyond", Foundations and Trends in Information Retrieval
#' 3(4), 333-389, doi:10.1561/1500000019.  Neither full text was retrievable
#' here, so the formula is written in its standard published form,
#' score(D, Q) = sum_t IDF(t) f(t,D)(k1+1) / (f(t,D) + k1(1 - b + b |D|/avgdl)),
#' with the Robertson-Sparck Jones inverse document frequency
#' IDF(t) = ln((N - n_t + 0.5)/(n_t + 0.5)).
#'
#' The two knobs are what BM25 is.  k1 controls term-frequency saturation: at
#' k1 = 0 the term factor collapses to 1 for any non-zero count, so the score
#' is just the sum of the IDFs of the query terms that appear, a closed form
#' and the anchor.  b controls length normalisation: at b = 0 document length
#' is ignored entirely, at b = 1 the count is fully divided by relative length.
#'
#' The RSJ IDF goes negative for a term appearing in more than half the
#' collection, which can make a document score lower for containing a query
#' term.  That is a real property of the formula, not a bug, and it is why the
#' Lucene-style smoothed variant ln(1 + (N-n+0.5)/(n+0.5)) is returned
#' alongside as score_smooth_idf rather than silently substituted.
#'
#' @param docs the collection; each document a string or token vector.
#' @param query the query terms.
#' @param k1 term-frequency saturation, non-negative.
#' @param b length normalisation, in \[0, 1\].
#' @return list: scores, estimate, ranking, score_smooth_idf, idf, idf_smooth,
#'   terms, avgdl, doc_len, k1, b, N, method.
#' @keywords internal
#' @examples
#' Bm25(list("the cat sat", "a dog barked", "the cat ran"), "cat")$scores
#' @export
Bm25 <- function(docs, query, k1 = 1.2, b = 0.75) {
  dl <- lapply(if (is.list(docs)) docs else as.list(docs), .bm25_tok)
  N <- length(dl)
  if (N == 0L) stop("bm25: the collection is empty")
  q <- .bm25_tok(query)
  if (length(q) == 0L) stop("bm25: the query is empty")
  kk <- as.numeric(k1)
  if (kk < 0) stop("bm25: k1 must be non-negative")
  bb <- as.numeric(b)
  if (!(bb >= 0 && bb <= 1)) stop("bm25: b must lie in [0, 1]")
  lens <- vapply(dl, length, 0L)
  tot <- 0
  for (v in lens) tot <- tot + v
  avgdl <- tot / N
  if (avgdl <= 0) stop("bm25: every document is empty")
  terms <- sort(unique(q))
  idf <- numeric(length(terms))
  idf_s <- numeric(length(terms))
  for (ti in seq_along(terms)) {
    nt <- 0L
    for (d in dl) if (terms[ti] %in% d) nt <- nt + 1L
    ratio <- (N - nt + 0.5) / (nt + 0.5)
    idf[ti] <- log(ratio)
    idf_s[ti] <- log(1 + ratio)
  }
  scores <- numeric(N)
  scores_s <- numeric(N)
  for (i in seq_len(N)) {
    s <- 0
    ss <- 0
    norm <- kk * (1 - bb + bb * lens[i] / avgdl)
    for (ti in seq_along(terms)) {
      f <- sum(dl[[i]] == terms[ti])
      if (f == 0) next
      w <- f * (kk + 1) / (f + norm)
      s <- s + idf[ti] * w
      ss <- ss + idf_s[ti] * w
    }
    scores[i] <- s
    scores_s[i] <- ss
  }
  order0 <- order(-scores, seq_len(N)) - 1L
  list(scores = scores, estimate = scores[1], ranking = order0,
       score_smooth_idf = scores_s, idf = idf, idf_smooth = idf_s, terms = terms,
       avgdl = avgdl, doc_len = lens, k1 = kk, b = bb, N = N,
       method = "Robertson et al. (1995) Okapi BM25 with Robertson-Sparck Jones IDF")
}

#' @noRd
.bm25_tok <- function(s) {
  if (is.character(s) && length(s) == 1L) {
    t <- strsplit(tolower(s), "[[:space:]]+")[[1]]
    t[nzchar(t)]
  } else tolower(as.character(s))
}
