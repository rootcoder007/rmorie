# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/ml_geron.R (Geron, Hands-On Machine Learning, 3rd ed.).
# The bias-variance identity, MC-dropout averages, CRC-32 hash splits
# (checked against digest's crc32 of the little-endian int64 id), the
# convolution sum, the assignment brute force and the centre-crop
# resize are all recomputed in the test.

test_that("bvdecomp splits the ensemble MSE into bias^2 + variance exactly", {
  p <- rbind(c(1, 2, 3), c(1.5, 1, 2.5), c(0.5, 2.5, 4))
  y <- c(1.2, 1.8, 3.1)
  r <- morie_bvdecomp(p, y, noisevar = 0.1)
  m <- colMeans(p)
  expect_equal(r$bias2, mean((m - y)^2), tolerance = 1e-12)
  expect_equal(r$variance, mean(colMeans(sweep(p, 2, m)^2)), tolerance = 1e-12)
  expect_equal(r$mse, mean(sweep(p, 2, y)^2), tolerance = 1e-12)
  expect_equal(r$residual, 0, tolerance = 1e-12)
  expect_equal(r$total, r$bias2 + r$variance + 0.1, tolerance = 1e-12)
  expect_error(morie_bvdecomp(p[1, , drop = FALSE], y), "2 predictor rows")
  expect_error(morie_bvdecomp(p, y[-1]), "one entry per test point")
})

test_that("mcdrop averages the softmax over passes", {
  L <- array(c(1, 0, 2, 1, -1, 0, 0.5, 3, 0, 1, 1, 2), c(2, 3, 2))
  r <- morie_mcdrop(L)
  for (i in 1:2) {
    P <- t(apply(L[i, , ], 1, function(z) exp(z) / sum(exp(z))))
    mu <- colMeans(P)
    expect_equal(r$probs[i, ], mu, tolerance = 1e-12)
    expect_equal(r$sds[i, ], sqrt(colMeans(sweep(P, 2, mu)^2)), tolerance = 1e-12)
  }
  ent <- -rowSums(r$probs * log(r$probs))
  expect_equal(r$meanentropy, mean(ent), tolerance = 1e-12)
  expect_equal(r$pred, apply(r$probs, 1, which.max) - 1L)
  expect_equal(r$meanmaxprob, mean(apply(r$probs, 1, max)), tolerance = 1e-12)
  expect_error(morie_mcdrop(matrix(1, 2, 2)), "n by T by k")
})

test_that("ttsplit and tvtsplit hash ids with CRC-32 of the int64 bytes", {
  skip_if_not_installed("digest")
  ids <- c(0, 1, 7, 42, 99, 1000, 123456, 2^31 - 1, 55, 3)
  crc <- vapply(ids, function(i) {
    lo <- i %% 2^32
    b <- as.raw(c(lo %% 256, (lo %/% 256) %% 256, (lo %/% 65536) %% 256, lo %/% 2^24, 0, 0, 0, 0))
    strtoi(substr(digest::digest(b, algo = "crc32", serialize = FALSE), 1, 4), 16L) * 65536 +
      strtoi(substr(digest::digest(b, algo = "crc32", serialize = FALSE), 5, 8), 16L)
  }, 0)
  r <- morie_ttsplit(ids, 0.3)
  expect_identical(r$test, which(crc < 0.3 * 2^32) - 1L)
  expect_identical(sort(c(r$test, r$train)), 0:9)
  expect_equal(r$ratio, r$ntest / 10)
  v <- morie_tvtsplit(ids, 0.25, 0.3)
  h <- crc / 2^32
  expect_identical(v$test, which(h < 0.3) - 1L)
  expect_identical(v$val, which(h >= 0.3 & h < 0.55) - 1L)
  expect_identical(v$train, which(h >= 0.55) - 1L)
  expect_error(morie_ttsplit(numeric(0)), "non-empty")
  expect_error(morie_ttsplit(1:3, 1), "strictly")
  expect_error(morie_ttsplit(-1), "non-negative")
  expect_error(morie_tvtsplit(1:3, 0.5, 0.5), "sum below 1")
  expect_error(morie_tvtsplit(numeric(0)), "non-empty")
})

test_that("stratsplt takes round(n_s * ratio) from each stratum", {
  s <- c("a", "b", "a", "a", "c", "b", "a", "b", "b", "b")
  r <- morie_stratsplt(s, 0.4)
  # a: 4 -> 2 (idx 0, 2); b: 5 -> 2 (idx 1, 5); c: 1 -> 0
  expect_identical(r$test, c(0L, 1L, 2L, 5L))
  expect_equal(r$ntrain, 6)
  expect_equal(r$maxdev, max(abs(c(2, 2, 0) / 4 - c(4, 5, 1) / 10)), tolerance = 1e-12)
  expect_equal(r$nstrata, 3L)
  expect_error(morie_stratsplt(character(0)), "non-empty")
  expect_error(morie_stratsplt(s, 0), "strictly")
})

test_that("convlayer computes the strided, zero-padded cross-correlation", {
  x <- array(c(1:16, (16:1) / 2), c(4, 4, 2))
  k <- array(seq(-1, 1, length.out = 3 * 3 * 2 * 2), c(3, 3, 2, 2))
  ref <- function(x, k, b, s, p) {
    xp <- array(0, dim(x) + c(2 * p, 2 * p, 0))
    xp[p + seq_len(dim(x)[1]), p + seq_len(dim(x)[2]), ] <- x
    oh <- (dim(xp)[1] - 3) %/% s + 1
    ow <- (dim(xp)[2] - 3) %/% s + 1
    z <- array(0, c(oh, ow, 2))
    for (i in 1:oh) for (j in 1:ow) for (o in 1:2) {
      z[i, j, o] <- sum(xp[(i - 1) * s + 1:3, (j - 1) * s + 1:3, ] * k[, , , o]) + b[o]
    }
    z
  }
  r <- morie_convlayer(x, k, bias = c(0.5, -1))
  expect_equal(r$z, ref(x, k, c(0.5, -1), 1, 0), tolerance = 1e-12)
  expect_equal(r$nparams, 3 * 3 * 2 * 2 + 2)
  r2 <- morie_convlayer(x, k, stride = c(2, 2), padding = c(1, 1))
  expect_equal(r2$z, ref(x, k, c(0, 0), 2, 1), tolerance = 1e-12)
  expect_equal(r2$total, sum(r2$z))
  expect_error(morie_convlayer(matrix(1, 2, 2), k), "3-D")
  expect_error(morie_convlayer(x, array(1, c(3, 3, 1, 2))), "in-channels")
  expect_error(morie_convlayer(x, k, bias = 1), "one entry per output")
  expect_error(morie_convlayer(x, k, stride = c(0, 1)), "stride must be positive")
  expect_error(morie_convlayer(array(1, c(2, 2, 2)), k), "larger than the padded")
})

test_that("trkassign finds the minimum-cost assignment by exhaustion", {
  P <- matrix(c(4, 1, 3, 2, 0, 5, 3, 2, 2), 3)
  A <- matrix(c(0, 3, 1, 2, 2, 0, 1, 0, 4), 3)
  perms <- list(1:3, c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  C <- 0.7 * P + 0.3 * A
  costs <- vapply(perms, function(p) sum(C[cbind(1:3, p)]), 0)
  r <- morie_trkassign(P, A, weight = 0.3)
  expect_equal(r$cost, min(costs), tolerance = 1e-12)
  expect_equal(r$assignment[, 2] + 1, perms[[which.min(costs)]])
  expect_equal(r$meancost, min(costs) / 3, tolerance = 1e-12)
  # two tracks, three detections: best pair of distinct columns
  R2 <- morie_trkassign(P[1:2, ])
  best <- min(outer(P[1, ], P[2, ], "+")[row(diag(3)) != col(diag(3))])
  expect_equal(R2$cost, 0.5 * best, tolerance = 1e-12)
  expect_equal(R2$nunmatcheddets, 1)
  expect_error(morie_trkassign(matrix(0, 9, 2)), "capped")
  expect_error(morie_trkassign(P, weight = 2), "weight must")
  expect_error(morie_trkassign(P, A[1:2, ]), "match the shape")
})

test_that("pretprep centre-crops, resizes by nearest neighbour and standardises", {
  img <- array(seq_len(6 * 4 * 2), c(6, 4, 2))
  r <- morie_pretprep(img, 2, c(10, 30), c(2, 4), logits = c(0.1, 2, -1, 2), topk = 3)
  # side 4, rows 2..5 (offset 1); source rows/cols 1 + (0, 2)
  for (i in 1:2) for (j in 1:2) {
    expect_equal(r$pixels[i, j, ], (img[1 + 1 + (i - 1) * 2, 1 + (j - 1) * 2, ] - c(10, 30)) / c(2, 4))
  }
  expect_equal(r$cropside, 4L)
  expect_equal(r$pixelmean, mean(r$pixels))
  expect_identical(r$pred, 1L)
  expect_identical(r$topk, c(1L, 3L, 0L))
  lg <- c(0.1, 2, -1, 2)
  expect_equal(r$topprob, exp(2) / sum(exp(lg)), tolerance = 1e-12)
  expect_error(morie_pretprep(matrix(1, 2, 2), 2, 0, 1), "3-D")
  expect_error(morie_pretprep(img, 0, c(0, 0), c(1, 1)), "positive")
  expect_error(morie_pretprep(img, 2, 0, 1), "one positive entry per channel")
})
