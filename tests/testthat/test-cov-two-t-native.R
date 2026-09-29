# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/twoT_native.R (sampling-bias-corrected two-tower
# retrieval, Yi et al. 2019). Towers are L2-normalised linear maps,
# the logQ correction is s / tau - log p, the streaming estimate is
# B <- (1 - a) B + a * gap with p = 1 / B (Algorithm 2), and the
# in-batch softmax loss is recomputed with log-sum-exp.

test_that("tower_embedding normalises W x + b", {
  W <- matrix(c(1, 0, 2, -1, 0.5, 1), 2)
  x <- c(1, 2, -1)
  z <- as.numeric(W %*% x) + c(0.5, 0)
  r <- morie_twoT_tower_embedding(x, W, c(0.5, 0))
  expect_equal(r$embedding, z / sqrt(sum(z^2)), tolerance = 1e-12)
  expect_equal(r$norm, sqrt(sum(z^2)), tolerance = 1e-12)
  expect_equal(morie_twoT_tower_embedding(x, list(W[1, ], W[2, ]), normalise = FALSE)$embedding,
               as.numeric(W %*% x))
  expect_error(morie_twoT_tower_embedding(1:2, W), "expects 3 features")
  expect_error(morie_twoT_tower_embedding(x, W, b = 1), "bias has 1")
  expect_error(morie_twoT_tower_embedding(c(0, 0, 0), W), "zero embedding")
})

test_that("corrected_logits subtract log p after the temperature", {
  r <- morie_twoT_corrected_logits(c(1, 2, 0.5), c(0.5, 0.01, 1), temperature = 0.5)
  expect_equal(r$corrected, c(2, 4, 1) - log(c(0.5, 0.01, 1)), tolerance = 1e-12)
  expect_equal(r$shift, -log(c(0.5, 0.01, 1)), tolerance = 1e-12)
  expect_error(morie_twoT_corrected_logits(1:2, 0.5), "2 scores but 1")
  expect_error(morie_twoT_corrected_logits(1, 0), "(0,1]", fixed = TRUE)
  expect_error(morie_twoT_corrected_logits(1, 0.5, 0), "temperature")
})

test_that("streaming_frequency tracks the mean gap between hits", {
  hits <- list(c(1, 2), 1, integer(0), c(1, 2), 1)
  r <- morie_twoT_streaming_frequency(hits, 5, alpha = 0.5)
  # item 1: seen at 0,1,3,4 -> B = 1, then 0.5*1+0.5*1, 0.5*1+0.5*2, 0.5*1.5+0.5*1
  expect_equal(r$B[["1"]], 1.25)
  # item 2: seen at 0,3 -> 0.5 * 1 + 0.5 * 3
  expect_equal(r$B[["2"]], 2)
  expect_equal(r$probability[["1"]], 0.8)
  expect_equal(r$n_items, 2L)
  # the step -> items map form, with gaps and more steps than entries
  m <- morie_twoT_streaming_frequency(list("2" = 7, "6" = c(7, 8)), 10, alpha = 0.25, init = 3)
  expect_equal(m$B[["7"]], 0.75 * 3 + 0.25 * 4)
  expect_equal(m$B[["8"]], 3)
  expect_equal(morie_twoT_streaming_frequency(hits[1:2], 6, 0.5)$B[["1"]], 1)
  expect_error(morie_twoT_streaming_frequency(hits, 5, alpha = 0), "step size")
})

test_that("batch_softmax_loss is the in-batch cross-entropy", {
  Q <- rbind(c(1, 0), c(0.6, 0.8), c(0, 1))
  I <- rbind(c(0.8, 0.6), c(1, 0), c(0.6, -0.8))
  S <- Q %*% t(I) / 0.1
  lse <- log(rowSums(exp(S)))
  r <- morie_twoT_batch_softmax_loss(Q, I, temperature = 0.1)
  expect_equal(r$per_example, lse - diag(S), tolerance = 1e-12)
  expect_equal(r$loss, mean(lse - diag(S)), tolerance = 1e-12)
  p <- c(0.5, 0.1, 0.9)
  Sc <- sweep(S, 2, log(p))
  rc <- morie_twoT_batch_softmax_loss(Q, I, probabilities = p, temperature = 0.1)
  expect_equal(rc$loss, mean(log(rowSums(exp(Sc))) - diag(Sc)), tolerance = 1e-12)
  expect_true(rc$corrected)
  expect_error(morie_twoT_batch_softmax_loss(Q, I[1:2, ]), "3 queries but 2 items")
  expect_error(morie_twoT_batch_softmax_loss(Q[1, , drop = FALSE], I[1, , drop = FALSE]), "at least 2")
})

test_that("retrieve ranks by corrected score; aliases agree", {
  I <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.5, 0.5))
  q <- c(1, 0.2)
  s <- as.numeric(I %*% q)
  p <- c(0.9, 0.05, 0.5, 0.5)
  for (fn in list(morie_twoT_retrieve, morie_twoT, twotower, two_tower)) {
    r <- fn(q, I, probabilities = p, top_k = 2)
    expect_identical(r$top_k, order(-(s - log(p)))[1:2])
    expect_identical(r$uncorrected_top_k, order(-s)[1:2])
    expect_true(r$changed)
  }
  u <- morie_twoT_retrieve(q, I, top_k = 10)
  expect_identical(u$top_k, order(-s))
  expect_false(u$changed)
})

test_that("morie_twoT_cheatsheet gives the correction", {
  expect_match(morie_twoT_cheatsheet(), "s^c = s - log p_j", fixed = TRUE)
})
