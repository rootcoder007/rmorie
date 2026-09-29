# Video diffusion U-Net factorisation (Ho et al. 2022): space-only
# convolution, spatial / temporal softmax attention, the identity mask,
# the attention cost and reconstruction guidance.

vg_attend <- function(X) {
  S <- X %*% t(X) / sqrt(ncol(X))
  W <- exp(S - apply(S, 1, max))
  W <- W / rowSums(W)
  list(out = W %*% X, W = W)
}
vg_video <- list(matrix(c(1, 0.5, -1, 2, 0, 1, 0.3, -0.2, 0.7), 3),
                 matrix(c(0, 1, 1, -1, 0.5, 2, 0.1, 0.2, 0.3), 3))

test_that("space-only convolution is a per-frame valid 2D correlation", {
  K <- matrix(c(1, -1, 0.5, 2), 2)
  r <- morie_vidgen_space_only_conv(vg_video, K)
  for (f in 1:2) {
    fr <- vg_video[[f]]
    ref <- outer(1:2, 1:2, Vectorize(function(i, j) sum(fr[i:(i + 1), j:(j + 1)] * K)))
    expect_equal(r$video[[f]], ref, tolerance = 1e-12)
  }
  expect_identical(r$frames, 2L)
  expect_error(morie_vidgen_space_only_conv(vg_video, matrix(1, 4, 4)), "larger than the frame")
})

test_that("spatial attention is softmax(X X' / sqrt(d)) X within each frame", {
  r <- morie_vidgen_spatial_attention(vg_video)
  for (f in 1:2) {
    ref <- vg_attend(vg_video[[f]])
    expect_equal(r$video[[f]], ref$out, tolerance = 1e-12)
    expect_equal(r$weights[[f]], ref$W, tolerance = 1e-12)
  }
})

test_that("temporal attention attends over frames per pixel; identity is exact", {
  r <- morie_vidgen_temporal_attention(vg_video)
  for (i in 1:3) for (j in 1:3) {
    s <- matrix(c(vg_video[[1]][i, j], vg_video[[2]][i, j]), 2)
    ref <- vg_attend(s)$out
    expect_equal(c(r$video[[1]][i, j], r$video[[2]][i, j]), as.numeric(ref), tolerance = 1e-12)
  }
  id <- morie_vidgen_temporal_attention(vg_video, identity = TRUE)
  expect_identical(id$video, lapply(vg_video, function(m) m * 1))
  expect_error(morie_vidgen_temporal_attention(list()), "no frames")
  expect_error(morie_vidgen_temporal_attention(list(matrix(1, 2, 2), matrix(1, 3, 3))),
               "differ in shape")
})

test_that("as_image_model runs a block on each frame alone", {
  blk <- function(v) morie_vidgen_spatial_attention(v)
  r <- morie_vidgen_as_image_model(vg_video, blk)
  expect_equal(r$video, morie_vidgen_spatial_attention(vg_video)$video, tolerance = 1e-15)
})

test_that("attention cost: (FS)^2 against F S^2 + S F^2", {
  r <- morie_vidgen_attention_cost(16, 64)
  expect_identical(r$joint, (16L * 64L)^2)
  expect_equal(r$factorised, 16 * 64^2 + 64 * 16^2)
  expect_equal(r$ratio, (16 * 64)^2 / (16 * 64^2 + 64 * 16^2), tolerance = 1e-12)
  expect_error(morie_vidgen_attention_cost(0, 4), "positive")
})

test_that("reconstruction guidance is -w grad of the squared error", {
  xh <- list(c(0.1, 0.5, -0.3, 0.9), c(1, 2, 3, 4), c(-1, 0, 1, 0))
  obs <- list(c(0, 0.4, 0, 1), c(-1, 0.5, 1, 0.5))
  r <- morie_vidgen_reconstruction_guidance(xh, obs, c(0, 2), weight = 3)
  expect_equal(r$gradient[[1]], -3 * 2 * (xh[[1]] - obs[[1]]), tolerance = 1e-12)
  expect_equal(r$gradient[[2]], rep(0, 4))
  expect_equal(r$gradient[[3]], -3 * 2 * (xh[[3]] - obs[[2]]), tolerance = 1e-12)
  expect_equal(r$error, sum((xh[[1]] - obs[[1]])^2) + sum((xh[[3]] - obs[[2]])^2),
               tolerance = 1e-12)
  # super-resolution: the loss is on D x with D a 2x average pool, so the
  # gradient is -w 2 D' (D x - y)
  D <- matrix(c(0.5, 0.5, 0, 0, 0, 0, 0.5, 0.5), 2, byrow = TRUE)
  low <- list(c(0.2, 0.1))
  sr <- morie_vidgen_reconstruction_guidance(xh, low, 1, weight = 2,
                                             downsample = function(v) as.numeric(D %*% v))
  expect_equal(sr$gradient[[2]], as.numeric(-2 * 2 * t(D) %*% (D %*% xh[[2]] - low[[1]])),
               tolerance = 1e-9)
  expect_equal(sr$error, sum((D %*% xh[[2]] - low[[1]])^2), tolerance = 1e-12)
  expect_error(morie_vidgen_reconstruction_guidance(xh, low, 1, downsample = function(v) v),
               "does not match")
  expect_error(morie_vidgen_reconstruction_guidance(xh, obs, 0), "observed frames")
  expect_error(morie_vidgen_reconstruction_guidance(xh, obs[1], 5), "outside the sample")
  expect_error(morie_vidgen_reconstruction_guidance(xh, obs[1], 0, weight = 0), "positive")
  expect_match(morie_vidgen_cheatsheet(), "IDENTITY", fixed = TRUE)
})
