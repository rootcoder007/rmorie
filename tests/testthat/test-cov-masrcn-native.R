# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/masrcn_native.R (Mask R-CNN, He et al. 2017). RoIPool
# is max pooling over quantised bins, RoIAlign is the bilinear average
# over sampling points (exact on a linear feature map, where the bin
# mean equals the value at the bin centre), and the mask loss is the
# per-pixel sigmoid or softmax cross entropy.

# F[y + 1, x + 1] = 10 y + x, so a bilinear sample at (y, x) is exactly 10 y + x
.mr_F <- outer(0:5, 0:5, function(i, j) 10 * i + j) * 1.0

test_that("roi_pool quantises the box and the bins, then max-pools", {
  r <- roi_pool(.mr_F, c(0.6, 0.4, 4.2, 4.9), out_size = 2)
  expect_equal(r$quantised_box, c(0L, 0L, 4L, 4L))
  expect_equal(r$quantisation_shift, c(0.6, 0.4), tolerance = 1e-12)
  # bins are rows/cols 0..1 and 2..3 after quantisation
  expect_equal(r$pooled[[1]], c(max(.mr_F[1:2, 1:2]), max(.mr_F[1:2, 3:4])))
  expect_equal(r$pooled[[2]], c(max(.mr_F[3:4, 1:2]), max(.mr_F[3:4, 3:4])))
  s <- roi_pool(.mr_F, c(16, 16, 80, 80), out_size = 2, stride = 16)
  expect_equal(s$quantised_box, c(1L, 1L, 5L, 5L))
  expect_equal(s$pooled[[1]], c(max(.mr_F[2:3, 2:3]), max(.mr_F[2:3, 4:5])))
  expect_error(roi_pool(.mr_F, c(0.1, 0.2, 0.9, 0.8)), "collapsed under quantisation")
})

test_that("roi_align averages bilinear samples without quantising", {
  # the feature map is linear in (y, x), so a bilinear sample IS 10y + x
  box <- c(0.5, 0.75, 4.5, 4.25)
  r <- roi_align(.mr_F, box, out_size = 2, samples = 2)
  bh <- (4.5 - 0.5) / 2
  bw <- (4.25 - 0.75) / 2
  for (i in 0:1) for (j in 0:1) {
    pts <- expand.grid(a = 0:1, b = 0:1)
    yy <- 0.5 + bh * (i + (pts$a + 0.5) / 2)
    xx <- 0.75 + bw * (j + (pts$b + 0.5) / 2)
    expect_equal(r$pooled[[i + 1]][j + 1], mean(10 * yy + xx), tolerance = 1e-12)
  }
  expect_equal(r$exact_box, box)
  expect_equal(r$samples_per_bin, 4L)
  # one sample per bin lands on the bin centre
  o <- roi_align(.mr_F, box, out_size = 2, samples = 1)
  expect_equal(o$pooled[[1]][1], 10 * (0.5 + bh / 2) + (0.75 + bw / 2), tolerance = 1e-12)
  # a whole-pixel box needs no interpolation, and align is not pool
  a <- roi_align(.mr_F, c(0, 0, 4, 4), out_size = 2, samples = 1)
  expect_equal(a$pooled[[1]], c(10 * 1 + 1, 10 * 1 + 3), tolerance = 1e-12)
  # the half-pixel offset is invisible to pool (it quantises the box to
  # the same integers) but moves every align sample by half a pixel
  half <- c(0.5, 0.5, 4.5, 4.5)
  expect_equal(roi_pool(.mr_F, half, out_size = 2)$pooled, roi_pool(.mr_F, c(0, 0, 4, 4), out_size = 2)$pooled)
  expect_equal(roi_align(.mr_F, half, out_size = 2, samples = 1)$pooled[[1]][1],
               a$pooled[[1]][1] + 10 * 0.5 + 0.5, tolerance = 1e-12)
  for (fn in list(maskrcnn, mask_rcnn_segmentation)) {
    expect_equal(fn(.mr_F, box, 2, 1, 2)$pooled, r$pooled)
  }
  expect_error(roi_align(.mr_F, c(2, 2, 2, 5)), "non-positive extent")
})

test_that("alignment_error reports the sub-pixel shift scaled by the stride", {
  e <- alignment_error(.mr_F, c(16 * 0.7, 16 * 1.4, 16 * 4, 16 * 4), stride = 16)
  expect_equal(e$feature_shift, c(0.7, 0.4), tolerance = 1e-12)
  expect_equal(e$input_pixel_shift, c(0.7, 0.4) * 16, tolerance = 1e-12)
  expect_equal(e$stride, 16)
})

test_that("mask_loss is the per-pixel sigmoid or softmax cross entropy", {
  L <- matrix(c(2, -1, 0.5, -0.25), 2)
  T <- matrix(c(1, 0, 1, 0), 2)
  d <- mask_loss(L, T)
  expect_equal(d$loss, mean(-(T * log(plogis(L)) + (1 - T) * log(1 - plogis(L)))), tolerance = 1e-12)
  expect_identical(d$kind, "per-pixel sigmoid")
  s <- mask_loss(L, T, decoupled = FALSE)
  # the flattening walks rows, so the softmax is over all four logits
  p <- exp(L - max(L)) / sum(exp(L - max(L)))
  expect_equal(s$loss, -sum(T * log(p)) / 4, tolerance = 1e-12)
  expect_identical(s$kind, "per-pixel softmax")
  # a confident correct prediction costs almost nothing
  expect_lt(mask_loss(matrix(c(30, -30), 1), matrix(c(1, 0), 1))$loss, 1e-12)
  expect_error(mask_loss(L, T[1, , drop = FALSE]), "differ in shape")
})

test_that("multitask_loss adds the three heads; morie_masrcn dispatches", {
  m <- multitask_loss(0.5, 0.25, 1)
  expect_equal(m$total, 1.75)
  expect_equal(c(m$cls, m$box, m$mask), c(0.5, 0.25, 1))
  expect_equal(morie_masrcn("multitask_loss", 1, 2, 3)$total, 6)
  expect_equal(morie_masrcn("roi_pool", .mr_F, c(0, 0, 4, 4), 2)$pooled,
               roi_pool(.mr_F, c(0, 0, 4, 4), 2)$pooled)
  expect_equal(morie_masrcn("roi_align", .mr_F, c(0, 0, 4, 4), 2)$pooled,
               roi_align(.mr_F, c(0, 0, 4, 4), 2)$pooled)
  expect_equal(morie_masrcn("maskrcnn", .mr_F, c(0, 0, 4, 4), 2)$samples_per_bin, 4L)
  expect_equal(morie_masrcn("mask_rcnn_segmentation", .mr_F, c(0, 0, 4, 4), 2)$exact_box, c(0, 0, 4, 4))
  expect_equal(morie_masrcn("alignment_error", .mr_F, c(0.5, 0, 4, 4))$feature_shift, c(0.5, 0))
  expect_equal(morie_masrcn("mask_loss", matrix(0, 1, 1), matrix(1, 1, 1))$loss, log(2), tolerance = 1e-12)
  expect_match(morie_masrcn("cheatsheet")$cheatsheet, "DECOUPLED", fixed = TRUE)
  expect_error(morie_masrcn("nms"), "unknown op")
  expect_error(morie_masrcn(), "op must be one of")
})
