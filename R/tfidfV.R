# SPDX-License-Identifier: AGPL-3.0-or-later
#' Term frequency-inverse document frequency
#'
#' Salton and Buckley (1988), Term-weighting approaches in automatic text
#' retrieval, Information Processing and Management 24(5), 513-523, and
#' Sparck Jones (1972), A statistical interpretation of term specificity,
#' Journal of Documentation 28(1), 11-21, which introduced the idf factor:
#' w_(t,d) = tf(t, d) log(N / df(t)).  Neither is open access; the weight
#' is quoted in its standard published form.  A term in every document gets
#' idf = log(1) = 0 and so weight exactly zero -- the intended behaviour,
#' not a degenerate case, which is why the smoothed variant log(1 + N/df) is
#' offered separately rather than substituted silently.  Terms are sorted
#' byte-wise so the Python mirror orders them identically.
#'
#' @param docs list of tokenised documents.
#' @param smooth use log(1 + N/df) instead of log(N/df).
#' @param sublinear use 1 + log(tf) instead of tf.
#' @return list: estimate, W, vocab, idf, df, n, method.
#' @keywords internal
#' @examples
#' Tfidf(list(c("a", "b"), c("b", "c", "b")))$idf
#' @export
Tfidf <- function(docs, smooth = FALSE, sublinear = FALSE) {
  D <- lapply(docs, as.character)
  N <- length(D)
  vocab <- character(0)
  for (d in D) for (t in d) if (!(t %in% vocab)) vocab <- c(vocab, t)
  vocab <- sort(vocab, method = "radix")
  V <- length(vocab)
  df <- numeric(V)
  for (j in seq_len(V)) for (d in D) if (vocab[j] %in% d) df[j] <- df[j] + 1
  idf <- numeric(V)
  for (j in seq_len(V)) {
    idf[j] <- if (df[j] > 0) {
      if (smooth) log(1 + N / df[j]) else log(N / df[j])
    } else 0
  }
  W <- matrix(0, N, V)
  for (i in seq_len(N)) for (j in seq_len(V)) {
    tf <- 0
    for (t in D[[i]]) if (t == vocab[j]) tf <- tf + 1
    if (sublinear && tf > 0) tf <- 1 + log(tf)
    W[i, j] <- tf * idf[j]
  }
  list(estimate = if (N && V) W[1, 1] else NaN, W = W, vocab = vocab,
       idf = idf, df = df, n = N,
       method = "TF-IDF weighting (Sparck Jones 1972; Salton and Buckley 1988)")
}
