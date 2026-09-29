# Coverage tests for R/berte_native.R (Devlin et al. 2019): exact GELU,
# layer norm, multi-head attention with padding and causal masks, the
# post- and pre-norm encoder block and the stacked encoder.

be_blk <- function(s) {
  m <- function(r, c, k) matrix(sin(seq_len(r * c) * k + s), r, c) / 2
  list(m(4, 4, 1.1), m(4, 4, 0.7), m(4, 4, 1.9), m(4, 4, 0.3), m(6, 4, 1.3), cos(1:6 + s) / 5, m(4, 6, 0.9), sin(1:4 + s) / 5)
}
be_X <- rbind(c(0.2, -0.4, 1, 0.5), c(-1, 0.3, 0.1, 0.8), c(0.6, 0.9, -0.2, -0.5))
ln <- function(v) (v - mean(v)) / sqrt(mean((v - mean(v))^2) + 1e-12)

test_that("GELU is x Phi(x) and layer norm standardises", {
  x <- c(-3, -0.5, 0, 1, 2.5)
  expect_equal(gelu(x), 0.5 * x * (1 + 2 * pnorm(x) - 1), tolerance = 1e-12)
  expect_equal(gelu(1), pnorm(1), tolerance = 1e-12)
  v <- c(1, 4, -2, 0.5)
  expect_equal(layer_norm(v), ln(v), tolerance = 1e-12)
  expect_equal(layer_norm(v, gain = 1:4, bias = c(0, 1, 0, 1)), ln(v) * 1:4 + c(0, 1, 0, 1), tolerance = 1e-12)
  expect_error(layer_norm(numeric(0)), "empty vector")
  expect_error(layer_norm(v, gain = 1), "gain has wrong length")
  expect_error(layer_norm(v, bias = 1:2), "bias has wrong length")
})

test_that("multi-head attention, padding and causal masks", {
  Q <- be_X
  K <- be_X[3:1, ]
  V <- be_X * 2
  sm <- function(S) exp(S - apply(S, 1, max)) / rowSums(exp(S - apply(S, 1, max)))
  w <- attention_weights(Q, K, 2)
  for (h in 1:2) {
    cols <- (2 * h - 1):(2 * h)
    expect_equal(w[[h]], sm(Q[, cols] %*% t(K[, cols]) / sqrt(2)), tolerance = 1e-12)
  }
  m <- multi_head_attention(Q, K, V, 2)
  expect_equal(m$out[, 3:4], w[[2]] %*% V[, 3:4], tolerance = 1e-12)
  pm <- attention_weights(Q, K, 2, pad_mask = c(TRUE, TRUE, FALSE))
  expect_equal(pm[[1]][, 3], rep(0, 3))
  expect_equal(pm[[1]][, 1:2], sm(Q[, 1:2] %*% t(K[1:2, 1:2]) / sqrt(2)), tolerance = 1e-12)
  cm <- attention_weights(Q, K, 1, causal = TRUE)[[1]]
  expect_equal(cm[upper.tri(cm)], rep(0, 3))
  expect_equal(cm[1, 1], 1)
  expect_error(attention_weights(Q, K, 3), "not divisible by 3 heads")
})

test_that("post-norm and pre-norm encoder blocks", {
  b <- be_blk(0)
  proj <- function(M, W, bias = 0) sweep(M %*% t(W), 2, bias, "+")
  att <- function(S) {
    Q <- S %*% t(b[[1]])
    multi_head_attention(Q, S %*% t(b[[2]]), S %*% t(b[[3]]), 2)$out %*% t(b[[4]])
  }
  x1 <- t(apply(be_X + att(be_X), 1, ln))
  f <- proj(matrix(gelu(proj(x1, b[[5]], b[[6]])), 3), b[[7]], b[[8]])
  post <- t(apply(x1 + f, 1, ln))
  r <- encoder_block(be_X, b[[1]], b[[2]], b[[3]], b[[4]], b[[5]], b[[6]], b[[7]], b[[8]], 2)
  expect_equal(r$out, post, tolerance = 1e-12)
  xp <- be_X + att(t(apply(be_X, 1, ln)))
  pre <- xp + proj(matrix(gelu(proj(t(apply(xp, 1, ln)), b[[5]], b[[6]])), 3), b[[7]], b[[8]])
  rp <- encoder_block(be_X, b[[1]], b[[2]], b[[3]], b[[4]], b[[5]], b[[6]], b[[7]], b[[8]], 2, pre_norm = TRUE)
  expect_equal(rp$out, pre, tolerance = 1e-12)
})

test_that("the encoder stacks blocks and pools the first token", {
  b1 <- be_blk(0)
  b2 <- c(be_blk(1), list(gain1 = 1:4 / 2, bias1 = rep(0.1, 4)))
  e <- bert_encoder(be_X, list(b1, b2), 2, pad_mask = c(TRUE, TRUE, FALSE))
  s1 <- encoder_block(be_X, b1[[1]], b1[[2]], b1[[3]], b1[[4]], b1[[5]], b1[[6]], b1[[7]], b1[[8]], 2, pad_mask = c(TRUE, TRUE, FALSE))
  s2 <- encoder_block(s1$out, b2[[1]], b2[[2]], b2[[3]], b2[[4]], b2[[5]], b2[[6]], b2[[7]], b2[[8]], 2, pad_mask = c(TRUE, TRUE, FALSE), gain1 = 1:4 / 2, bias1 = rep(0.1, 4))
  expect_equal(e$output, s2$out)
  expect_equal(e$pooled, s2$out[1, ])
  expect_equal(e$attention[[1]], s1$weights)
  expect_equal(c(e$n_layers, e$d), c(2L, 4L))
  expect_identical(bertencoder, bert_encoder)
  expect_identical(morie_berte, bert_encoder)
})
