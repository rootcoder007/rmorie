# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sbert_native.R (Sentence-BERT, Reimers & Gurevych
# 2019): the three poolings, cosine similarity, the (u, v, |u - v|)
# classification features, the C(n, 2) vs n pass count and the cached
# siamese scoring.

.sb_T <- rbind(c(1, 0, 2), c(-1, 4, 0), c(3, 1, -2), c(0, 0, 0))

test_that("pool takes the mean, the first kept token or the coordinatewise max", {
  expect_equal(pool(.sb_T), colMeans(.sb_T))
  expect_equal(pool(.sb_T, "cls"), .sb_T[1, ])
  expect_equal(pool(.sb_T, "max"), apply(.sb_T, 2, max))
  m <- c(FALSE, TRUE, TRUE, FALSE)
  expect_equal(pool(.sb_T, "mean", mask = m), colMeans(.sb_T[2:3, ]))
  expect_equal(pool(.sb_T, "cls", mask = m), .sb_T[2, ])
  expect_equal(pool(.sb_T, "max", mask = m), apply(.sb_T[2:3, ], 2, max))
  expect_equal(pool(list(c(1, 2), c(3, 4))), c(2, 3))
  expect_error(pool(.sb_T, "sum"), "pooling must be one of")
  expect_error(pool(matrix(numeric(0), 0, 3)), "no token vectors")
  expect_error(pool(.sb_T, mask = c(TRUE, TRUE)), "2 mask entries for 4 tokens")
  expect_error(pool(.sb_T, mask = rep(FALSE, 4)), "excludes every token")
})

test_that("cosine_similarity is the normalised inner product", {
  u <- c(1, 2, 2)
  v <- c(2, 0, 0)
  expect_equal(cosine_similarity(u, v), 1 / 3, tolerance = 1e-12)
  expect_equal(cosine_similarity(u, u), 1, tolerance = 1e-12)
  expect_equal(cosine_similarity(u, -u), -1, tolerance = 1e-12)
  expect_equal(cosine_similarity(list(1, 0), c(0, 5)), 0)
  expect_error(cosine_similarity(u, c(1, 2)), "differ in length")
  expect_error(cosine_similarity(u, c(0, 0, 0)), "zero vector")
})

test_that("classification_features concatenates u, v and |u - v|", {
  u <- c(1, -2)
  v <- c(0.5, 1)
  f <- classification_features(u, v)
  expect_equal(f$features, c(1, -2, 0.5, 1, 0.5, 3))
  expect_equal(f$abs_diff, abs(u - v))
  expect_equal(f$dim, 6)
  expect_error(classification_features(u, c(1, 2, 3)), "differ in length")
})

test_that("pair_cost counts C(n, 2) cross-encoder passes against n bi-encoder ones", {
  p <- pair_cost(10000)
  expect_equal(p$cross_encoder, 10000 * 9999 / 2)
  expect_equal(p$bi_encoder, 10000)
  expect_equal(p$forward_passes, p$cross_encoder)
  expect_equal(p$speedup, 9999 / 2)
  expect_equal(pair_cost(10000, "bi-encoder")$forward_passes, 10000)
  expect_equal(pair_cost(2)$cross_encoder, 1)
  expect_error(pair_cost(1), "at least 2 sentences")
  expect_error(pair_cost(5, "poly"), "mode must be cross-encoder or bi-encoder")
})

test_that("rank_by_similarity orders the corpus by cosine without a forward pass", {
  E <- rbind(c(1, 0), c(0.7071, 0.7071), c(0, 1), c(-1, 0))
  q <- c(1, 0.1)
  r <- rank_by_similarity(q, E, top_k = 3)
  sc <- apply(E, 1, function(e) cosine_similarity(q, e))
  expect_equal(r$ranking[, "index"], order(-sc)[1:3])
  expect_equal(r$ranking[, "score"], sc[order(-sc)[1:3]], tolerance = 1e-12)
  expect_equal(r$n_corpus, 4L)
  expect_equal(r$forward_passes, 0)
  expect_equal(nrow(rank_by_similarity(q, E, top_k = 99)$ranking), 4L)
  expect_error(rank_by_similarity(q, matrix(numeric(0), 0, 2)), "corpus is empty")
})

test_that("sts_score embeds each sentence once and scores by cosine", {
  emb <- list(a = c(1, 0), b = c(0, 1), c = c(1, 1))
  seen <- character(0)
  f <- function(s) {
    seen <<- c(seen, s)
    emb[[s]]
  }
  pairs <- list(c("a", "b"), c("a", "c"), c("b", "c"), c("a", "b"))
  for (fn in list(sts_score, sentencebert, sbert, morie_sbert)) {
    seen <- character(0)
    r <- fn(pairs, f)
    expect_equal(r$scores, c(0, 1 / sqrt(2), 1 / sqrt(2), 0), tolerance = 1e-12)
    expect_equal(r$embed_calls, 3L)
    expect_equal(r$cross_encoder_calls, 4L)
    expect_equal(r$n_pairs, 4L)
    expect_identical(seen, c("a", "b", "c"))
    expect_identical(r$estimate, r$scores)
  }
})
