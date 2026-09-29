# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/wrd2v_native.R (Mikolov et al. 2013a/b). Complexities
# from eqs. 4-5 of 2013a, the unigram^0.75 noise and eq. 5 subsampling
# of 2013b; with lr = 0 the output layer stays at zero, so the full
# softmax loss is log V and the negative-sampling loss (k + 1) log 2,
# and the input vectors are the initial uniform draws.

.w2_corpus <- list(c("a", "b", "c", "a"), c("b", "c", "d"), c("a", "d"))

test_that("training_complexity is N D + D log2 V (CBOW) and C (D + D log2 V)", {
  expect_equal(morie_wrd2v_training_complexity("cbow", 100, 1024, N = 8), 8 * 100 + 100 * 10)
  expect_equal(morie_wrd2v_training_complexity("skip-gram", 100, 1024, C = 10), 10 * (100 + 1000))
  expect_equal(morie_wrd2v_training_complexity("cbow", 10, 50, N = 2, hierarchical = FALSE), 20 + 500)
  expect_error(morie_wrd2v_training_complexity("glove", 1, 2), "architecture must be one of")
  expect_error(morie_wrd2v_training_complexity("cbow", 1, 2), "needs N")
  expect_error(morie_wrd2v_training_complexity("skip-gram", 1, 2), "needs C")
})

test_that("noise distribution and subsampling follow 2013b", {
  cn <- list(the = 100, cat = 10, sat = 1)
  nd <- morie_wrd2v_noise_distribution(cn)
  u <- c(cat = 10, sat = 1, the = 100)^0.75
  expect_equal(unlist(nd), u / sum(u), tolerance = 1e-12)
  expect_identical(names(nd), c("cat", "sat", "the"))
  expect_equal(unlist(morie_wrd2v_noise_distribution(cn, power = 1)), c(cat = 10, sat = 1, the = 100) / 111)
  expect_error(morie_wrd2v_noise_distribution(list(a = 0)), "no mass")
  sp <- morie_wrd2v_subsample_probability(cn, t = 0.05)
  f <- c(the = 100, cat = 10, sat = 1) / 111
  expect_equal(unlist(sp), pmax(1 - sqrt(0.05 / f), 0), tolerance = 1e-12)
  expect_error(morie_wrd2v_subsample_probability(list(a = 0)), "empty counts")
  expect_error(morie_wrd2v_subsample_probability(cn, 0), "t must be")
})

test_that("with lr = 0 the losses are log V and (k + 1) log 2", {
  f <- morie_wrd2v_wrd2v(.w2_corpus, size = 3, window = 2, lr = 0, epochs = 2, seed = 4)
  expect_identical(names(f$vectors), c("a", "b", "c", "d"))
  expect_equal(f$loss_curve, rep(log(4), 2), tolerance = 1e-12)
  e <- .ghc_rng(4)
  W0 <- matrix((.ghc_unif(e, 12L) * 2 - 1) * 0.5 / 3, 4, 3, byrow = TRUE)
  expect_equal(unname(do.call(rbind, f$vectors)), W0, tolerance = 1e-12)
  expect_equal(unlist(f$vocab), c(a = 3, b = 2, c = 2, d = 2))
  cb <- morie_wrd2v_wrd2v(.w2_corpus, size = 3, architecture = "cbow", lr = 0, epochs = 1)
  expect_equal(cb$final_loss, log(4), tolerance = 1e-12)
  ng <- morie_wrd2v_wrd2v(.w2_corpus, size = 3, loss = "neg", negative = 3, lr = 0, epochs = 1)
  expect_equal(ng$final_loss, 4 * log(2), tolerance = 1e-12)
  expect_equal(ng$negative, 3L)
  mc <- morie_wrd2v_wrd2v(.w2_corpus, size = 2, min_count = 3, epochs = 1, dynamic_window = FALSE)
  expect_identical(names(mc$vectors), "a")
})

test_that("training lowers the loss; similarity is the cosine of the vectors", {
  corpus <- rep(list(c("king", "rules", "land"), c("queen", "rules", "land"), c("dog", "barks")), 4)
  for (fn in list(morie_wrd2v_wrd2v, morie_wrd2v, morie_wrd2v_word2vec)) {
    f <- fn(corpus, size = 4, window = 2, lr = 0.1, epochs = 15, dynamic_window = FALSE)
    expect_lt(f$final_loss, f$loss_curve[1])
    a <- f$vectors$king
    b <- f$vectors$queen
    expect_equal(f$similarity("king", "queen"), sum(a * b) / sqrt(sum(a^2) * sum(b^2)), tolerance = 1e-12)
    ms <- f$most_similar("king", topn = 2)
    expect_length(ms, 2L)
    expect_gte(ms[[1]][[2]], ms[[2]][[2]])
    expect_error(f$most_similar("cat"), "not in the vocabulary")
  }
  ss <- morie_wrd2v_wrd2v(corpus, size = 2, epochs = 1, subsample = 1e-3)
  expect_true(all(is.finite(ss$loss_curve)))
  expect_error(morie_wrd2v_wrd2v(corpus, architecture = "x"), "architecture must be one of")
  expect_error(morie_wrd2v_wrd2v(corpus, loss = "nce"), "loss must be")
  expect_error(morie_wrd2v_wrd2v(corpus, architecture = "cbow", loss = "neg"), "SKIP-GRAM")
  expect_error(morie_wrd2v_wrd2v(corpus, loss = "neg", negative = 0), "negative must be")
  expect_error(morie_wrd2v_wrd2v(corpus, size = 0), "size must be")
  expect_error(morie_wrd2v_wrd2v(corpus, window = 0), "window must be")
  expect_error(morie_wrd2v_wrd2v(list(character(0))), "non-empty sentence")
  expect_error(morie_wrd2v_wrd2v(corpus, min_count = 99), "discarded every word")
})

test_that("analogy returns the nearest word to b - a + c", {
  v <- list(man = c(1, 0, 0), king = c(1, 1, 0), woman = c(0, 0, 1), queen = c(0, 1, 1),
            apple = c(0.5, -1, 0.2))
  r <- morie_wrd2v_analogy(v, "man", "king", "woman", topn = 2)
  expect_identical(r[[1]][[1]], "queen")
  expect_equal(unname(r[[1]][[2]]), 1, tolerance = 1e-12)
  tgt <- c(0, 1, 1)
  expect_equal(unname(r[[2]][[2]]), sum(tgt * v$apple) / sqrt(2 * sum(v$apple^2)), tolerance = 1e-12)
  expect_error(morie_wrd2v_analogy(v, "man", "king", "girl"), "girl is not in")
})

test_that("morie_wrd2v_cheatsheet gives both complexities", {
  s <- morie_wrd2v_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "Q = C*(D + D*log2 V)", fixed = TRUE)
})
