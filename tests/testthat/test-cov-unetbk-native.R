# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/unetbk_native.R (U-Net, Ronneberger, Fischer & Brox
# 2015): the valid-convolution size bookkeeping (the paper's 572 ->
# 388), mirror padding, the overlap-tile layout, the centre-cropped
# skip concatenation and the separation weight map.

test_that("valid_output_size reproduces the paper's 572 -> 388 chain", {
  r <- valid_output_size(572, depth = 4, convs_per_block = 2, kernel = 3)
  expect_equal(r$output, 388L)
  expect_equal(r$border_lost, 572L - 388L)
  expect_equal(r$skip_sizes, c(568L, 280L, 136L, 64L))
  # one level, by hand: 64 -> 60 -> pool 30 -> 26 -> up 52 -> min(52, 60) -> 48
  e <- valid_output_size(64, depth = 1)
  expect_equal(e$output, 48L)
  expect_equal(e$skip_sizes, 60L)
  # depth 0 is just the convolutions
  expect_equal(valid_output_size(10, depth = 0)$output, 6L)
  expect_equal(valid_output_size(20, depth = 1, kernel = 1)$output, 20L)
  expect_error(valid_output_size(0), "must be positive")
  expect_error(valid_output_size(8, depth = 4), "too small for this depth")
  expect_error(valid_output_size(63, depth = 1), "is odd before pooling")
})

test_that("mirror_pad reflects the border without repeating it", {
  img <- matrix(1:9, 3, 3, byrow = TRUE)
  p <- mirror_pad(img, 1)
  expect_equal(dim(p), c(5L, 5L))
  expect_equal(p[2:4, 2:4], img * 1.0)
  # row -1 mirrors row 1 and column -1 mirrors column 1
  expect_equal(p[1, 2:4], as.numeric(img[2, ]))
  expect_equal(p[5, 2:4], as.numeric(img[2, ]))
  expect_equal(p[2:4, 1], as.numeric(img[, 2]))
  expect_equal(p[1, 1], img[2, 2] * 1.0)
  expect_equal(mirror_pad(img, 0), img * 1.0)
  expect_error(mirror_pad(img, -1), "non-negative")
  expect_error(mirror_pad(img, 3), "must be smaller than the image")
})

test_that("overlap_tiles abuts outputs and overlaps inputs by the border", {
  t <- overlap_tiles(100, 60, tile = 40, border = 8)
  expect_equal(t$output_size, 24L)
  expect_equal(t$n_tiles, length(seq(0, 99, by = 24)) * length(seq(0, 59, by = 24)))
  expect_equal(t$tiles[[1]]$output_origin, c(0, 0))
  expect_equal(t$tiles[[1]]$input_origin, c(-8, -8))
  expect_equal(t$tiles[[2]]$output_origin, c(0, 24))
  expect_equal(t$tiles[[2]]$input_size, 40L)
  os <- vapply(t$tiles, function(x) x$output_origin[1], 0)
  expect_setequal(unique(os), seq(0, 99, by = 24))
  expect_error(overlap_tiles(10, 10, 0, 1), "tile must be positive")
  expect_error(overlap_tiles(10, 10, 4, 2), "consumes the whole tile")
})

test_that("skip_concat centre-crops the contracting map", {
  co <- matrix(1:36, 6, 6)
  up <- matrix(0, 2, 2)
  s <- skip_concat(up, co)
  expect_equal(s$crop_offset, c(2L, 2L))
  expect_equal(s$concatenated, cbind(up, co[3:4, 3:4]) * 1.0)
  expect_equal(s$channels, 2L)
  o <- skip_concat(matrix(0, 3, 3), matrix(1:16, 4, 4))
  expect_equal(o$crop_offset, c(0L, 0L))
  expect_equal(skip_concat(co, co)$crop_offset, c(0L, 0L))
  expect_error(skip_concat(co, up), "smaller than the upsampled one")
})

test_that("separation_weight_map raises the weight between touching instances", {
  lab <- matrix(0, 5, 5)
  lab[2, 2] <- 1
  lab[2, 4] <- 2
  w <- separation_weight_map(lab, w0 = 10, sigma = 5)
  expect_equal(w$n_instances, 2L)
  expect_equal(w$weights[2, 2], 1)
  # (2, 3) sits one pixel from each instance
  expect_equal(w$weights[2, 3], 1 + 10 * exp(-(1 + 1)^2 / 50), tolerance = 1e-12)
  # (1, 1): distances sqrt(1 + 1) to instance 1 and sqrt(1 + 9) to instance 2
  expect_equal(w$weights[1, 1], 1 + 10 * exp(-(sqrt(2) + sqrt(10))^2 / 50), tolerance = 1e-12)
  expect_equal(w$max_weight, max(w$weights))
  expect_gt(w$weights[2, 3], w$weights[5, 5])
  # one instance: nothing to separate, so every weight stays 1
  one <- separation_weight_map(matrix(c(1, 0, 0, 0), 2))
  expect_equal(one$weights, matrix(1, 2, 2))
  expect_equal(one$n_instances, 1L)
})

test_that("valid_output_size aliases agree", {
  for (fn in list(unet, unet_backbone, unetbackbone)) {
    expect_equal(fn(572)$output, 388L)
  }
})
