# STAMP (Liu et al. 2018): trilinear product, session memories, MLP
# cells, attention net and softmax cross-entropy, recomputed with
# matrix algebra.

st_X <- rbind(c(0.1, 0.4, -0.2), c(0.5, -0.3, 0.2), c(-0.1, 0.2, 0.6), c(0.3, 0.3, 0.1))
st_V <- rbind(c(1, 0, 0), c(0, 1, 0), c(0.5, 0.5, 0.5), c(-1, 0.2, 0.4))
st_W <- function(k, r = 3, c = 3) matrix(sin(seq_len(r * c) + k), r, c)

test_that("trilinear product is a' (b * c)", {
  a <- c(1, 2, 3)
  b <- c(-1, 0.5, 2)
  cc <- c(0.3, 4, -1)
  expect_equal(strec_trilinear(a, b, cc), sum(a * (b * cc)), tolerance = 1e-15)
  expect_error(strec_trilinear(1:2, 1:3, 1:3), "differ in length")
})

test_that("session average and last click", {
  s <- strec_session_average(st_X)
  expect_equal(s$m_s, colMeans(st_X), tolerance = 1e-15)
  expect_identical(s$m_t, st_X[4, ])
  expect_identical(s$length, 4L)
  expect_equal(strec_session_average(list(c(1, 2), c(3, 4)))$m_s, c(2, 3))
  expect_error(strec_session_average(list()), "empty")
})

test_that("MLP cells apply W m + b then tanh or identity", {
  W <- st_W(1, 2, 3)
  m <- c(0.2, -0.5, 1)
  b <- c(0.1, -0.2)
  expect_equal(strec_mlp_cell(m, W, b), tanh(as.numeric(W %*% m + b)), tolerance = 1e-15)
  expect_equal(strec_mlp_cell(m, W, activation = "identity"), as.numeric(W %*% m), tolerance = 1e-15)
  expect_error(strec_mlp_cell(1:2, W), "expects 3 inputs")
  expect_error(strec_mlp_cell(m, W, activation = "relu"), "tanh or identity")
})

test_that("attention weights are W0 sigma(W1 x_i + W2 x_t + W3 m_s + b_a), unnormalised", {
  W1 <- st_W(1, 2)
  W2 <- st_W(2, 2)
  W3 <- st_W(3, 2)
  W0 <- c(0.7, -0.4)
  ba <- c(0.05, -0.1)
  r <- strec_attention_weights(st_X, W1, W2, W3, W0, ba)
  ms <- colMeans(st_X)
  xt <- st_X[4, ]
  al <- vapply(1:4, function(i) {
    sum(W0 * stats::plogis(as.numeric(W1 %*% st_X[i, ] + W2 %*% xt + W3 %*% ms + ba)))
  }, numeric(1))
  expect_equal(r$alpha, al, tolerance = 1e-14)
  expect_equal(r$m_a, as.numeric(t(st_X) %*% al), tolerance = 1e-14)
  expect_equal(r$sum_alpha, sum(al), tolerance = 1e-14)
  expect_error(strec_attention_weights(list(), W1, W2, W3, W0), "empty")
})

test_that("STMP and STAMP scores: sigmoid of the trilinear form, softmax over items", {
  Ws <- st_W(4)
  Wt <- st_W(5)
  r <- strec_stamp_scores(st_X, st_V, Ws, Wt)
  hs <- tanh(as.numeric(Ws %*% colMeans(st_X)))
  ht <- tanh(as.numeric(Wt %*% st_X[4, ]))
  z <- stats::plogis(as.numeric(st_V %*% (hs * ht)))
  expect_equal(r$score, z, tolerance = 1e-14)
  expect_equal(r$probability, exp(z) / sum(exp(z)), tolerance = 1e-14)
  expect_identical(r$ranking, order(-z))
  expect_identical(r$estimate, which.max(z))
  expect_identical(r$model, "STMP")
  att <- strec_attention_weights(st_X, st_W(1, 2), st_W(2, 2), st_W(3, 2), c(1, 1))
  ra <- strec_stamp(st_X, st_V, Ws, Wt, attention = att)
  hs2 <- tanh(as.numeric(Ws %*% att$m_a))
  expect_equal(ra$score, stats::plogis(as.numeric(st_V %*% (hs2 * ht))), tolerance = 1e-14)
  expect_identical(ra$model, "STAMP")
  expect_identical(morie_strec(st_X, st_V, Ws, Wt)$ranking, r$ranking)
})

test_that("cross-entropy sums the one-hot binary log-losses", {
  p <- c(0.1, 0.6, 0.3)
  expect_equal(strec_cross_entropy(p, 2), -(log(0.9) + log(0.6) + log(0.7)), tolerance = 1e-14)
  expect_identical(strec_cross_entropy(c(0, 1), 2), 0)
  expect_error(strec_cross_entropy(p, 4), "outside the item dictionary")
  expect_match(strec_cheatsheet(), "TRILINEARLY", fixed = TRUE)
})
