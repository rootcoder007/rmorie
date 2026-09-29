# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sentpc_native.R (SentencePiece, Kudo & Richardson
# 2018): whitespace escaping, greedy BPE and unigram Viterbi. The BPE
# merges are derived by hand for a corpus whose pair counts are fixed,
# and the Viterbi optimum is checked against every segmentation.

.sp <- "▁"

test_that("escape / unescape whitespace round-trip with the U+2581 marker", {
  for (fn in list(morie_sentpc_escape_whitespace, escape_whitespace)) {
    expect_identical(fn("a b  c"), paste0(.sp, "a", .sp, "b", .sp, .sp, "c"))
    expect_identical(fn("a b", add_prefix = FALSE), paste0("a", .sp, "b"))
  }
  for (fn in list(morie_sentpc_unescape_whitespace, unescape_whitespace)) {
    expect_identical(fn(paste0(.sp, "a", .sp, "b")), "a b")
    expect_identical(fn(paste0(.sp, "a"), strip_prefix = FALSE), " a")
    expect_identical(fn("ab"), "ab")
  }
  expect_identical(morie_sentpc_decode(c(paste0(.sp, "he"), "llo", paste0(.sp, "w"))), "hello w")
  expect_identical(morie_sentpc_decode(c(paste0(.sp, "a")), strip_prefix = FALSE), " a")
})

test_that("train_bpe merges the most frequent pair, first-seen on ties", {
  # units: 3 x "_ab", 1 x "_abc"; pairs (_,a)=4, (a,b)=4, (b,c)=1
  # merge 1 (_,a) wins the tie, then (_a,b)=4, then (_ab,c)=1
  corpus <- c("ab ab ab", "abc")
  for (fn in list(morie_sentpc_train_bpe, train_bpe)) {
    m <- fn(corpus, 7)
    expect_identical(m$merges, list(c(.sp, "a"), c(paste0(.sp, "a"), "b"),
                                    c(paste0(.sp, "ab"), "c")))
    expect_setequal(m$vocab, c(.sp, "a", "b", "c", paste0(.sp, "a"), paste0(.sp, "ab"),
                               paste0(.sp, "abc")))
    expect_equal(m$vocab_size, 7L)
    expect_equal(m$requested, 7L)
    # vocab_size below the alphabet: no merges
    expect_length(fn(corpus, 2)$merges, 0L)
    # merges stop once no adjacent pair is left
    expect_length(fn(corpus, 50)$merges, 3L)
    expect_error(fn(corpus, 0), "at least 1")
    expect_error(fn(character(0), 5), "no tokens")
  }
})

test_that("encode_bpe applies the merges in order and decodes back", {
  m <- morie_sentpc_train_bpe(c("ab ab ab", "abc"), 7)
  for (fn in list(morie_sentpc_encode_bpe, encode_bpe)) {
    toks <- fn("ab abc ca", m)
    expect_identical(toks, c(paste0(.sp, "ab"), paste0(.sp, "abc"), .sp, "c", "a"))
    expect_identical(morie_sentpc_decode(toks), "ab abc ca")
    expect_identical(fn("ab", m, add_prefix = FALSE), c("a", "b"))
  }
})

test_that("viterbi_segment returns the maximum-log-probability segmentation", {
  lp <- list(a = log(0.1), b = log(0.1), ab = log(0.3), abb = log(0.05), bb = log(0.2))
  lp[[.sp]] <- log(0.2)
  lp[[paste0(.sp, "a")]] <- log(0.15)
  s <- paste0(.sp, "abb")
  segs <- function(x) {
    if (!nzchar(x)) return(list(character(0)))
    out <- list()
    for (L in seq_len(nchar(x))) {
      pc <- substr(x, 1, L)
      if (!is.null(lp[[pc]])) for (r in segs(substr(x, L + 1, nchar(x)))) out <- c(out, list(c(pc, r)))
    }
    out
  }
  all <- segs(s)
  scores <- vapply(all, function(p) sum(unlist(lp[p])), 0)
  for (fn in list(morie_sentpc_viterbi_segment, viterbi_segment)) {
    r <- fn("abb", lp)
    expect_equal(r$logp, max(scores), tolerance = 1e-12)
    expect_identical(r$pieces, all[[which.max(scores)]])
    expect_equal(r$n_pieces, length(r$pieces))
    expect_identical(fn("", lp, add_prefix = FALSE)$pieces, character(0))
    expect_error(fn("abz", lp), "no segmentation covers")
  }
})
