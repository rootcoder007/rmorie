# SPDX-License-Identifier: AGPL-3.0-or-later
#' Byte-pair-encoding subword tokenizer
#'
#' Sennrich, Haddow and Birch (2016), Neural machine translation of rare
#' words with subword units, ACL 54, 1715-1725 (arXiv:1508.07909 --
#' FETCHED), algorithm 1: represent every word as characters plus an
#' end-of-word marker, count all adjacent symbol pairs across the corpus,
#' merge the most frequent pair into a new symbol, and repeat.  The
#' paper's own marker </w> is used.
#'
#' Determinism: ties in the pair counts break by first appearance in a
#' fixed scan order, so the merge list is reproducible -- which is what a
#' tokenizer needs and what lets the two arms agree.
#'
#' @param corpus words (repeats allowed), or the unique word list when
#'   word_counts is given.
#' @param vocab_size number of merge operations to learn.
#' @param word_counts frequency of each word.
#' @return list: estimate, merges, counts, vocab, n_vocab, tokens, method.
#' @keywords internal
#' @examples
#' Bpetrain(c("low", "low", "lower", "newest", "newest"), 3)$merges
#' @export
Bpetrain <- function(corpus, vocab_size = 10, word_counts = NULL) {
  words <- as.character(corpus)
  if (!is.null(word_counts)) {
    freq <- as.numeric(word_counts)
  } else {
    uniq <- character(0)
    cnt <- numeric(0)
    for (w in words) {
      i <- match(w, uniq)
      if (is.na(i)) { uniq <- c(uniq, w)
      cnt <- c(cnt, 1) } else cnt[i] <- cnt[i] + 1
    }
    words <- uniq
    freq <- cnt
  }
  eow <- "</w>"
  seqs <- lapply(words, function(w) c(strsplit(w, "")[[1]], eow))
  merges <- character(0)
  counts <- numeric(0)
  for (it in seq_len(as.integer(vocab_size))) {
    pairs <- character(0)
    pc <- numeric(0)
    for (wi in seq_along(seqs)) {
      s <- seqs[[wi]]
      if (length(s) > 1L) for (j in seq_len(length(s) - 1L)) {
        p <- paste0(s[j], "\001", s[j + 1L])
        i <- match(p, pairs)
        if (is.na(i)) { pairs <- c(pairs, p)
        pc <- c(pc, freq[wi]) } else pc[i] <- pc[i] + freq[wi]
      }
    }
    if (length(pairs) == 0L) break
    best <- 1L
    if (length(pairs) > 1L) for (i in seq(2L, length(pairs))) if (pc[i] > pc[best]) best <- i
    if (pc[best] <= 1) break
    ab <- strsplit(pairs[best], "\001", fixed = TRUE)[[1]]
    a <- ab[1]
    b <- ab[2]
    merges <- c(merges, paste0(a, "|", b))
    counts <- c(counts, pc[best])
    for (wi in seq_along(seqs)) {
      s <- seqs[[wi]]
      out <- character(0)
      j <- 1L
      while (j <= length(s)) {
        if (j + 1L <= length(s) && s[j] == a && s[j + 1L] == b) {
          out <- c(out, paste0(a, b))
          j <- j + 2L
        } else {
          out <- c(out, s[j])
          j <- j + 1L
        }
      }
      seqs[[wi]] <- out
    }
  }
  vocab <- character(0)
  for (s in seqs) for (sym in s) if (!(sym %in% vocab)) vocab <- c(vocab, sym)
  vocab <- sort(vocab)
  list(estimate = as.numeric(length(merges)), merges = merges,
       counts = counts, vocab = vocab, n_vocab = length(vocab), tokens = seqs,
       method = "BPE subword learner (Sennrich et al. 2016, algorithm 1)")
}
