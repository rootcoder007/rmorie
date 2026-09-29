# Coverage tests for R/fastxt_native.R (Bojanowski et al. 2017):
# character n-grams, the FNV-1a hash, subword word vectors and the
# skipgram trainer with and without hashing.

ft_corpus <- c("the cat sat on the mat", "the dog sat on the log", "a cat and a dog")

test_that("subword n-grams with boundaries and the whole word", {
  expect_equal(subwords("where", 3, 3), c("<wh", "whe", "her", "ere", "re>", "<where>"))
  expect_equal(subwords("ab", 2, 4, whole_word = FALSE), c("<a", "ab", "b>", "<ab", "ab>", "<ab>"))
  expect_equal(subwords("ab", 3, 4), c("<ab", "ab>", "<ab>"))
  expect_equal(subwords("aaa", 1, 1, boundary = FALSE), c("a", "aaa"))
  expect_error(subwords("x", 0, 3), "n_min must be at least 1")
  expect_error(subwords("x", 4, 3), "below n_min")
})

test_that("FNV-1a matches the published 32-bit test vectors", {
  expect_equal(.fnv1a(""), 2166136261)
  expect_equal(.fnv1a("a"), 3826002220)
  expect_equal(.fnv1a("foobar"), 3214735720)
  expect_equal(.gram_slot("foobar", NULL, 1000), 3214735720 %% 1000)
  expect_null(.gram_slot("zz", list(aa = 0L), NULL))
  expect_equal(.gram_slot("aa", list(aa = 3L), NULL), 3L)
})

test_that("a word vector sums its n-gram rows", {
  Z <- matrix(1:12, 4, 3)
  gi <- list("<ab" = 0L, "ab>" = 2L, "<ab>" = 3L)
  wv <- word_vector("ab", Z, gi, 3, 3)
  expect_equal(wv$v, Z[1, ] + Z[3, ] + Z[4, ])
  expect_equal(wv$hit, 3L)
  expect_equal(word_vector("zz", Z, gi, 3, 3)$hit, 0L)
})

test_that("skipgram training: vocabulary, vectors, OOV and hashing", {
  f <- fasttext(ft_corpus, dim = 4, epochs = 2, window = 2, negative = 2, seed = 3)
  expect_equal(f$vocab, sort(unique(unlist(strsplit(ft_corpus, " ")))))
  expect_equal(f$n_ngrams, length(unique(unlist(lapply(f$vocab, subwords)))))
  expect_equal(f$vectors[[2]], word_vector(f$vocab[2], f$Z, f$ngram_index)$v)
  expect_equal(f$oov("cats"), word_vector("cats", f$Z, f$ngram_index)$v)
  expect_true(any(f$oov("cats") != 0))
  expect_length(f$loss_history, 2L)
  expect_true(all(is.finite(f$loss_history)))
  expect_equal(fasttext(ft_corpus, dim = 4, epochs = 2, window = 2, negative = 2, seed = 3)$Z, f$Z)
  z0 <- fasttext(ft_corpus, dim = 4, epochs = 0, seed = 3)
  u <- .ghc_unif(.ghc_rng(3), z0$n_ngrams * 4)
  expect_equal(z0$Z, matrix((u - 0.5) * 0.125, ncol = 4, byrow = TRUE), tolerance = 1e-12)
  h <- fasttext(ft_corpus, dim = 3, epochs = 1, negative = 1, hash_buckets = 50, seed = 1)
  expect_equal(nrow(h$Z), 50L)
  expect_equal(h$vectors[[1]], colSums(h$Z[vapply(subwords(h$vocab[1]), .gram_slot, 0L, NULL, 50) + 1L, , drop = FALSE]))
  expect_equal(morie_fastxt(ft_corpus, dim = 4, epochs = 1, seed = 2)$Z, fasttext(ft_corpus, dim = 4, epochs = 1, seed = 2)$Z)
  expect_match(.fastxt_cheatsheet(), "OOV")
  expect_error(fasttext(NULL), "must not be None")
  expect_error(fasttext(list()), "corpus is empty")
  expect_error(fasttext(ft_corpus, dim = 0), "dim must be at least 1")
  expect_error(fasttext(ft_corpus, min_count = 10), "skipgram needs a context")
})
