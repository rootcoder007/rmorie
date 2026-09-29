# NARM (Li et al. 2017): attention scores, local encoder, session vector,
# bilinear decoder and its parameter count, recomputed with matrices.

na_H <- rbind(c(0.1, 0.3), c(-0.2, 0.5), c(0.4, 0.1))
na_A1 <- matrix(c(0.5, -0.3, 0.2, 0.8), 2)
na_A2 <- matrix(c(-0.1, 0.6, 0.4, 0.2), 2)
na_v <- c(1.2, -0.7)

test_that("softmax is shift-invariant and sums to one", {
  z <- c(1, 2, 3)
  expect_equal(narm_softmax(z), exp(z) / sum(exp(z)), tolerance = 1e-15)
  expect_equal(narm_softmax(z + 1000), narm_softmax(z), tolerance = 1e-15)
  expect_identical(morie_narm(z), narm_softmax(z))
})

test_that("attention weights are a softmax of v' sigma(A1 h_t + A2 h_j)", {
  ht <- na_H[3, ]
  sc <- as.numeric(stats::plogis(sweep(na_H %*% t(na_A2), 2, as.numeric(na_A1 %*% ht), "+")) %*% na_v)
  a <- narm_attention_weights(ht, na_H, na_A1, na_A2, na_v)
  expect_equal(a, exp(sc) / sum(exp(sc)), tolerance = 1e-15)
  expect_length(narm_attention_weights(ht, na_H[0, , drop = FALSE], na_A1, na_A2, na_v), 0L)
})

test_that("local encoder, session vector and bilinear decoder", {
  al <- c(0.2, 0.5, 0.3)
  cl <- narm_local_encoder(na_H, al)
  expect_equal(cl, as.numeric(t(na_H) %*% al), tolerance = 1e-15)
  expect_error(narm_local_encoder(na_H, 1:2), "2 weights for 3")
  ct <- narm_session_repr(na_H[3, ], cl)
  expect_equal(ct, c(na_H[3, ], cl))
  E <- rbind(c(1, 0, 0.5), c(0.2, 0.3, -0.1), c(-1, 1, 0))
  B <- matrix(seq(-0.5, 0.6, length.out = 12), 3, 4)
  s <- narm_bilinear_scores(E, B, ct)
  expect_equal(s$scores, as.numeric(E %*% B %*% ct), tolerance = 1e-15)
  expect_equal(s$probabilities, narm_softmax(s$scores), tolerance = 1e-15)
  expect_error(narm_bilinear_scores(E, B, ct[-1]), "B has 4 columns")
  expect_error(narm_bilinear_scores(E[, 1:2], B, ct), "2-dimensional")
})

test_that("the bilinear decoder needs |D||H| parameters instead of |N||H|", {
  p <- narm_decoder_parameters(40000, 100, 50)
  expect_identical(p$fully_connected, 4000000L)
  expect_identical(p$bilinear, 5000L)
  expect_equal(p$ratio, 800)
  expect_error(narm_decoder_parameters(0, 1, 1), "at least 1")
  expect_match(narm_cheatsheet(), "BILINEAR", fixed = TRUE)
})
