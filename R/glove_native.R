```r
# morie.fn -- function file (rootcoder007/morie)
# GloVe -- global vectors for word representation.
#
# Pennington, Socher & Manning (2014). The model is a weighted least
# squares regression on the logarithm of the word-word co-occurrence
# counts. Their eq. (8):
#
#   J = sum_{i,j=1}^{V} f(X_{ij}) (w_i^T tilde_w_j + b_i + tilde_b_j - log X_{ij})^2
#
# with the weighting function of eq. (9),
#
#   f(x) = (x / x_max)^alpha   if x < x_max
#          1                   otherwise
#
# and their stated defaults x_max = 100 and alpha = 3/4 ("we fix to
# x_max = 100 for all our experiments"; "alpha = 3/4 gives a modest
# improvement over a linear version with alpha = 1").
#
# Three details that are easy to get wrong and are in the paper:
#
# Zero counts are skipped, not weighted to zero.  f(0) = 0 is
# property 1, but log 0 is undefined, so the sum runs over nonzero
# X_{ij} only.  Evaluating the term first and multiplying by zero
# afterwards produces a NaN that then poisons every gradient.
#
# Two sets of vectors, and the sum is what you use.  The paper keeps
# w and tilde_w, notes they are equivalent up to initialisation, and
# recommends w + tilde_w as the final representation -- which also
# acts as a variance reduction.  The default here is that sum, with
# the separate matrices returned as well.
#
# The context window is harmonic.  Sec. 4.2: a token d positions away
# contributes 1/d to the count, "so that very distant word pairs are
# expected to contain less relevant information".  A flat window is
# a different model and gives different vectors.
#
# Training is AdaGrad, as in the paper, because the per-parameter step
# size matters here: word frequencies span orders of magnitude and a
# single global learning rate either crawls on rare words or diverges
# on frequent ones.
#
# References
# ----------
# Pennington, J., Socher, R. & Manning, C. D. (2014) "GloVe: Global
# Vectors for Word Representation", Proceedings of EMNLP 2014,
# 1532-1543, doi:10.3115/v1/D14-1162.  Equations (8) and (9), Sec. 4.2
# for the harmonic weighting and the AdaGrad training.
#
# Duchi, J., Hazan, E. & Singer, Y. (2011) "Adaptive subgradient methods
# for online learning and stochastic optimization", JMLR 12, 2121-2159
# -- the AdaGrad the paper uses.


# Private helper: turn a corpus into a list of token character vectors.
.glove_as_docs <- function(corpus) {
  if (is.null(corpus)) {
    stop("glove: corpus must not be None")
  }
  docs <- list()
  for (item in corpus) {
    if (is.character(item) && length(item) == 1L) {
      # A single string: split on whitespace.
      parts <- strsplit(item, "\\s+")[[1L]]
      parts <- parts[nzchar(parts)]
      docs[[length(docs) + 1L]] <- parts
    } else {
      # A vector or list of tokens: each element is a token.
      docs[[length(docs) + 1L]] <- as.character(item)
    }
  }
  if (length(docs) == 0L) {
    stop("glove: the corpus is empty")
  }
  docs
}

# Private helper: build the co-occurrence table as a sorted data frame
# with columns i, j, count.  R is 1-based, so all indices are 1-based.
.glove_cooccurrence_df <- function(corpus, window, harmonic, min_count) {
  docs <- .glove_as_docs(corpus)
  # Count token frequencies across the whole corpus.
  all_tokens <- unlist(docs, use.names = FALSE)
  token_counts <- table(all_tokens)
  # Vocab: tokens with count >= min_count, sorted.
  vocab <- sort(names(token_counts)[token_counts >= as.integer(min_count)])
  # Index: token -> 1-based position in vocab.
  index_vec <- setNames(seq_along(vocab), vocab)
  # Compute co-occurrences within the (symmetric) context window.
  w <- as.integer(window)
  if (w < 1L) {
    stop(sprintf("cooccurrence: window must be at least 1, got %r", window))
  }
  pair_list <- list()
  for (doc in docs) {
    if (length(doc) == 0L) next
    ids <- as.integer(index_vec[doc])
    n_ids <- length(ids)
    for (pos in seq_len(n_ids)) {
      lo <- max(1L, as.integer(pos) - w)
      # Guard: 1:0 in R is c(1, 0), so the loop must be skipped when empty.
      if (lo >= pos) next
      for (other in lo:(pos - 1L)) {
        j <- ids[other]
        d_ <- pos - other
        inc <- if (isTRUE(harmonic)) 1.0 / as.numeric(d_) else 1.0
        i_ <- ids[pos]
        # Symmetric pair: (i, j) and (j, i) both get the same increment.
        pair_list[[length(pair_list) + 1L]] <- c(i_ = i_, j_ = j, inc = inc)
        pair_list[[length(pair_list) + 1L]] <- c(i_ = j, j_ = i_, inc = inc)
      }
    }
  }
  if (length(pair_list) == 0L) {
    Xdf <- data.frame(i = integer(0), j = integer(0), count = numeric(0))
  } else {
    M <- do.call(rbind, pair_list)
    df <- as.data.frame(M)
    colnames(df) <- c("i", "j", "inc")
    # Aggregate duplicate (i, j) pairs by summing the increments.
    Xdf <- aggregate(inc ~ i + j, data = df, FUN = sum)
    colnames(Xdf) <- c("i", "j", "count")
    # Sort by (i, j) to match Python
