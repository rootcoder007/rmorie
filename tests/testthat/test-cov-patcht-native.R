# PatchTST front end (Nie et al. 2023): patching with stride, channel-
# independent and channel-mixed tokens, instance normalisation, and the
# attention-cost reduction.

test_that("patchify cuts (L - P) %/% S + 1 windows", {
  x <- 1:10
  p <- patchify(x, 4, 3)
  expect_identical(p$n_patches, 3L)
  expect_equal(p$patches, list(1:4, 4:7, 7:10))
  expect_identical(p$covers, 10L)
  q <- patchify(x, 3)
  expect_equal(q$patches, list(1:3, 4:6, 7:9))
  expect_identical(q$covers, 9L)
  expect_error(patchify(1:3, 4), "3 points but the patch length is 4")
  expect_error(patchify(x, 0), "at least 1")
  expect_error(patchify(x, 2, 0), "stride")
})

test_that("channel-independent tokens patch each column; mixed tokens patch the row sums", {
  X <- cbind(1:8, (1:8)^2)
  ci <- channel_independent_tokens(X, 4, 2)
  expect_equal(ci$tokens[[2]], patchify((1:8)^2, 4, 2)$patches)
  expect_identical(ci$n_tokens_total, 2L * 3L)
  cm <- channel_mixed_tokens(X, 4, 2)
  expect_equal(cm$tokens, patchify(1:8 + (1:8)^2, 4, 2)$patches)
  expect_error(channel_independent_tokens(matrix(numeric(0), 0, 2), 2), "empty")
  expect_error(channel_mixed_tokens(matrix(numeric(0), 0, 2), 2), "empty")
})

test_that("instance normalisation standardises by the sample sd", {
  v <- c(2, 4, 4, 5, 9)
  r <- instance_norm(v)
  expect_equal(r$normalised, (v - mean(v)) / stats::sd(v), tolerance = 1e-15)
  expect_equal(c(r$mean, r$sd), c(mean(v), stats::sd(v)), tolerance = 1e-15)
  d <- instance_norm(c(3, 3, 3))
  expect_true(d$degenerate)
  expect_equal(d$normalised, rep(0, 3))
  expect_error(instance_norm(1), "at least 2")
})

test_that("the encoder normalises each channel before patching and reports the cost", {
  set.seed(4)
  X <- cbind(stats::rnorm(24, 5, 2), stats::runif(24))
  r <- patchtst_encode(X, 6, 3)
  z1 <- (X[, 1] - mean(X[, 1])) / stats::sd(X[, 1])
  expect_equal(r$tokens[[1]], patchify(z1, 6, 3)$patches, tolerance = 1e-14)
  expect_equal(r$norm_stats[[2]]$sd, stats::sd(X[, 2]), tolerance = 1e-15)
  expect_identical(r$cost$n_patches, 7L)
  expect_equal(r$cost$reduction, 24^2 / 7^2)
  raw <- patchtst_encode(X, 6, 3, normalise = FALSE)
  expect_equal(raw$tokens[[1]], patchify(X[, 1], 6, 3)$patches)
  expect_identical(morie_patchT, patchtst_encode)
})
