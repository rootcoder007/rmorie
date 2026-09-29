# Coverage tests for R/mistr_native.R (Mistral 7B, Jiang et al. 2023):
# RMSNorm, SwiGLU, RoPE, sliding-window masks, grouped-query attention
# and the decoder block, for both the morie_mistr_* and mistr_* spellings.

mistr_softmax_attn <- function(q, k, v, allowed) {
  s <- as.numeric(k[allowed, , drop = FALSE] %*% q) / sqrt(length(q))
  w <- exp(s - max(s))
  colSums(v[allowed, , drop = FALSE] * w) / sum(w)
}

mistr_rot <- function(x, pos, base = 10000) {
  d <- length(x)
  th <- base^(-(0:(d / 2 - 1)) * 2 / d)
  z <- complex(real = x[c(TRUE, FALSE)], imaginary = x[c(FALSE, TRUE)]) * exp(1i * pos * th)
  as.numeric(rbind(Re(z), Im(z)))
}

test_that("RMSNorm and SwiGLU follow their definitions", {
  x <- c(0.5, -1.2, 2.0, 0.3)
  g <- c(1, 0.5, 2, 1.5)
  for (f in list(morie_mistr_rms_norm, mistr_rms_norm)) {
    expect_equal(f(x), x / sqrt(mean(x^2) + 1e-6), tolerance = 1e-12)
    expect_equal(f(x, g, eps = 0), x / sqrt(mean(x^2)) * g, tolerance = 1e-12)
    expect_equal(f(3 * x, eps = 0), f(x, eps = 0), tolerance = 1e-12)
    expect_error(f(numeric(0)), "empty")
    expect_error(f(x, g[-1]), "gain has 3")
  }
  W1 <- matrix(c(0.2, -0.1, 0.4, 0.3, 0.5, -0.2, 0.1, 0.0, -0.3, 0.2, 0.6, 0.1), 4, 3)
  W3 <- matrix(c(0.1, 0.3, -0.2, 0.2, -0.4, 0.1, 0.5, 0.2, 0.0, 0.3, -0.1, 0.4), 4, 3)
  W2 <- matrix(c(0.3, -0.2, 0.1, 0.4, 0.2, -0.5, 0.1, 0.1, 0.3, -0.3, 0.2, 0.2), 3, 4)
  a <- as.numeric(x %*% W1)
  ref <- as.numeric((a * plogis(a) * as.numeric(x %*% W3)) %*% W2)
  expect_equal(morie_mistr_swiglu(x, W1, W2, W3), ref, tolerance = 1e-12)
  expect_equal(mistr_swiglu(x, W1, W2, W3), ref, tolerance = 1e-12)
  expect_error(morie_mistr_swiglu(x, W1, W2, W3[, 1:2]), "same width")
  expect_error(mistr_swiglu(x, W1, W2, W3[, 1:2]), "same width")
})

test_that("RoPE rotates channel pairs and preserves relative position", {
  expect_equal(morie_mistr_rope_angles(6), 10000^(-(0:2) * 2 / 6), tolerance = 1e-12)
  expect_equal(mistr_rope_angles(6, base = 100), 100^(-(0:2) * 2 / 6), tolerance = 1e-12)
  expect_error(morie_mistr_rope_angles(5), "even dimension")
  expect_error(mistr_rope_angles(5), "even dimension")
  q <- c(0.3, -0.8, 1.1, 0.4, -0.2, 0.9)
  k <- c(-0.5, 0.2, 0.7, 1.3, 0.6, -0.4)
  for (f in list(morie_mistr_apply_rope, mistr_apply_rope)) {
    expect_equal(f(q, 7), mistr_rot(q, 7), tolerance = 1e-12)
    expect_equal(sum(f(q, 9) * f(k, 4)), sum(f(q, 5) * k), tolerance = 1e-12)
    expect_equal(sqrt(sum(f(q, 3)^2)), sqrt(sum(q^2)), tolerance = 1e-12)
    expect_equal(f(q, 2, theta = c(1, 0, 0))[3:6], q[3:6])
    expect_error(f(q, 1, theta = 1:2), "2 angles for 6")
  }
})

test_that("sliding-window masks and receptive-field span", {
  for (f in list(morie_mistr_sliding_window_mask, mistr_sliding_window_mask)) {
    m <- f(6, 3)
    expect_identical(m, outer(1:6, 1:6, function(i, j) j <= i & i - j < 3))
    # non-causal: every later token plus the window behind
    expect_identical(f(4, 2, causal = FALSE), outer(1:4, 1:4, function(i, j) i - j < 2))
    expect_error(f(4, 0), "at least 1")
  }
  expect_identical(morie_mistr_attention_span(4096, 32), 131072L)
  expect_identical(mistr_attention_span(3, 5), 15L)
})

test_that("grouped-query attention equals per-head softmax attention with shared KV", {
  L <- 5
  Q <- matrix(sin(1:(L * 4)), L, 4)
  K <- matrix(cos(1:(L * 2) / 2), L, 2)
  V <- matrix(((1:(L * 2)) %% 7) / 7, L, 2)
  mask <- morie_mistr_sliding_window_mask(L, 3)
  pos <- 0:(L - 1)
  ref <- matrix(0, L, 4)
  for (h in 0:1) {
    qs <- t(vapply(1:L, function(t) mistr_rot(Q[t, 2 * h + 1:2], pos[t]), c(0, 0)))
    ks <- t(vapply(1:L, function(t) mistr_rot(K[t, ], pos[t]), c(0, 0)))
    for (i in 1:L) ref[i, 2 * h + 1:2] <- mistr_softmax_attn(qs[i, ], ks, V, which(mask[i, ]))
  }
  expect_equal(morie_mistr_grouped_query_attention(Q, K, V, 2, 1, mask = mask), ref, tolerance = 1e-12)
  expect_equal(mistr_grouped_query_attention(Q, K, V, 2, 1, mask = mask), ref, tolerance = 1e-12)
  # without RoPE and mask, head h uses the plain scaled dot product
  nr <- morie_mistr_grouped_query_attention(Q, K, V, 2, 1, positions = FALSE)
  expect_equal(nr[3, 1:2], mistr_softmax_attn(Q[3, 1:2], K, V, 1:L), tolerance = 1e-12)
  expect_equal(mistr_grouped_query_attention(Q, K, V, 2, 1, positions = FALSE), nr, tolerance = 1e-12)
  # one KV head shared by two query heads == two identical KV heads
  expect_equal(morie_mistr_grouped_query_attention(Q, cbind(K, K), cbind(V, V), 2, 2, mask = mask), ref, tolerance = 1e-12)
  expect_error(morie_mistr_grouped_query_attention(Q, K, V, 3, 2), "multiple of")
  expect_error(mistr_grouped_query_attention(Q, K[, 1, drop = FALSE], V, 2, 1), "must be 2 wide")
  expect_error(morie_mistr_grouped_query_attention(Q, K, V, 2, 1, mask = matrix(FALSE, L, L)), "attend to nothing")
})

test_that("the decoder block composes RMSNorm, attention and SwiGLU residually", {
  L <- 4
  d <- 4
  X <- matrix(sin(1:(L * d) / 3), L, d)
  mk <- function(r, c, s) matrix(sin(s * (1:(r * c))) / 2, r, c)
  Wq <- mk(4, 4, 1.1)
  Wk <- mk(4, 2, 1.7)
  Wv <- mk(4, 2, 2.3)
  Wo <- mk(4, 4, 0.7)
  W1 <- mk(4, 6, 0.9)
  W3 <- mk(4, 6, 1.3)
  W2 <- mk(6, 4, 0.5)
  n1 <- c(1, 0.8, 1.2, 0.9)
  rn <- function(M, g) t(apply(M, 1, morie_mistr_rms_norm, weight = g))
  for (norm1 in list(NULL, n1)) {
    h <- rn(X, norm1)
    a <- morie_mistr_grouped_query_attention(h %*% Wq, h %*% Wk, h %*% Wv, 2, 1,
      mask = morie_mistr_sliding_window_mask(L, 2)) %*% Wo
    x1 <- X + a
    ff <- t(apply(rn(x1, NULL), 1, morie_mistr_swiglu, W1 = W1, W2 = W2, W3 = W3))
    for (f in list(morie_mistr_mistral_block, mistr_mistral_block)) {
      b <- f(X, Wq, Wk, Wv, Wo, W1, W2, W3, 2, 1, 2, norm1 = norm1)
      expect_equal(b$output, x1 + ff, tolerance = 1e-12)
      expect_equal(b$kv_cache_entries, 2)
      expect_identical(b$attention_mask, morie_mistr_sliding_window_mask(L, 2))
    }
  }
  expect_match(mistr_cheatsheet(), "SWA")
})
