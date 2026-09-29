# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/vit2lf_native.R (log-length scaled attention,
# Chiang & Cholak 2022; scaled cosine attention and log-spaced
# relative positions, Liu et al. 2022 Swin V2). Logits, softmax,
# entropy and context are recomputed with matrix algebra.

.v2_q <- matrix(c(1, 0, 2, -1, 0.5, 1), 3, 2)
.v2_k <- matrix(c(0.3, 1, -2, 1, 0, 0.5, 2, -1), 4, 2)
.v2_v <- matrix(1:8, 4, 2)

test_that("logits scale q.k by 1/sqrt(d), log n, or a cosine temperature", {
  d <- 2
  qk <- .v2_q %*% t(.v2_k)
  expect_equal(morie_vit2lf_logits(.v2_q, .v2_k)$logits, qk / sqrt(d), tolerance = 1e-12)
  expect_equal(morie_vit2lf_logits(.v2_q, .v2_k, "logn")$logits, qk * log(4) / sqrt(d), tolerance = 1e-12)
  expect_equal(morie_vit2lf_logits(.v2_q, .v2_k, "logn", n = 9)$scale, log(9) / sqrt(2), tolerance = 1e-12)
  cosm <- qk / outer(sqrt(rowSums(.v2_q^2)), sqrt(rowSums(.v2_k^2)))
  expect_equal(morie_vit2lf_logits(.v2_q, .v2_k, "cosine", tau = 0.2)$logits, cosm / 0.2, tolerance = 1e-12)
  expect_equal(morie_vit2lf_logits(.v2_q, .v2_k, "logn_cosine", tau = 0.001)$logits, cosm * log(4) / 0.01,
               tolerance = 1e-12)
  B <- matrix(seq(-1, 1, length.out = 12), 3, 4)
  expect_equal(morie_vit2lf_logits(.v2_q, .v2_k, bias = B)$logits, qk / sqrt(d) + B, tolerance = 1e-12)
  z <- morie_vit2lf_logits(rbind(c(0, 0)), .v2_k, "cosine")$logits
  expect_equal(z, matrix(0, 1, 4))
  expect_error(morie_vit2lf_logits(.v2_q, .v2_k, "l2"), "mode must be one of")
  expect_error(morie_vit2lf_logits(.v2_q, cbind(.v2_k, 1)), "one head dimension")
  expect_error(morie_vit2lf_logits(.v2_q, .v2_k, n = 0), "positive")
  expect_error(morie_vit2lf_logits(.v2_q, .v2_k, "cosine", tau = 0), "temperature")
})

test_that("softmax respects the mask; entropy is -sum p log p", {
  L <- matrix(c(1, 2, 0, -1, 3, 0.5), 2, 3)
  W <- morie_vit2lf_softmax(L)
  expect_equal(W, exp(L) / rowSums(exp(L)), tolerance = 1e-12)
  M <- matrix(c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE), 2, 3)
  Wm <- morie_vit2lf_softmax(L, M)
  E <- exp(L) * M
  expect_equal(Wm, E / rowSums(E), tolerance = 1e-12)
  expect_equal(morie_vit2lf_entropy(Wm), -rowSums(ifelse(Wm > 0, Wm * log(Wm), 0)), tolerance = 1e-12)
  expect_equal(morie_vit2lf_entropy(matrix(c(1, 0), 1)), 0)
  expect_error(morie_vit2lf_softmax(L, matrix(FALSE, 2, 3)), "masked out entirely")
})

test_that("log_coords and relative_bias index the (2w - 1)^2 table", {
  expect_equal(morie_vit2lf_log_coords(3, -2), c(log(4), -log(3)))
  expect_equal(morie_vit2lf_log_coords(0, 0), c(0, 0))
  tab <- matrix(1:25, 5, 5)
  xy <- rbind(c(0, 0), c(1, 2), c(2, 1))
  lin <- morie_vit2lf_relative_bias(xy, tab, 3, log_spaced = FALSE)
  for (i in 1:3) for (j in 1:3) {
    expect_equal(lin[i, j], tab[xy[i, 1] - xy[j, 1] + 3, xy[i, 2] - xy[j, 2] + 3])
  }
  # log-spaced: offsets snap to the nearest log-spaced grid point; with
  # offsets inside the window the grid reproduces the linear lookup
  expect_equal(morie_vit2lf_relative_bias(xy, tab, 3), lin)
  far <- morie_vit2lf_relative_bias(rbind(c(0, 0), c(9, 0)), tab, 3)
  expect_equal(far[2, 1], tab[5, 3])
  expect_error(morie_vit2lf_relative_bias(xy, tab[1:4, ], 3), "square")
  expect_error(morie_vit2lf_relative_bias(rbind(c(0, 0), c(9, 0)), tab, 3, FALSE), "outside the table")
})

test_that("morie_vit2lf attends with the scaled weights", {
  r <- morie_vit2lf(.v2_q, .v2_k, .v2_v)
  L <- .v2_q %*% t(.v2_k) * log(4) / sqrt(2)
  W <- exp(L) / rowSums(exp(L))
  expect_equal(r$weights, W, tolerance = 1e-12)
  expect_equal(r$context, W %*% .v2_v, tolerance = 1e-12)
  expect_equal(r$max_weight, apply(W, 1, max), tolerance = 1e-12)
  expect_equal(r$estimate, mean(apply(W, 1, max)), tolerance = 1e-12)
  expect_equal(r$mean_entropy, mean(-rowSums(W * log(W))), tolerance = 1e-12)
  expect_true(is.nan(r$tau))
  expect_equal(morie_vit2lf(.v2_q, .v2_k, .v2_v, mode = "cosine", tau = 0.001)$tau, 0.01)
  expect_error(morie_vit2lf(.v2_q, .v2_k, .v2_v[1:3, ]), "one value per key")
  expect_error(morie_vit2lf(.v2_q[0, , drop = FALSE], .v2_k, .v2_v), "non-empty")
})

test_that("morie_vit2lf_cheatsheet lists the modes", {
  expect_match(morie_vit2lf_cheatsheet(), "dot, logn, cosine, logn_cosine", fixed = TRUE)
})
