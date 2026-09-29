# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sdxlcd_native.R (SDXL micro-conditioning, Podell et al.
# 2023). The Fourier features interleave sin / cos at frequencies
# 2^j pi s; the conditioning vector is their concatenation, added to
# the timestep embedding; buckets hold about `pixels` pixels.

.sx_ff <- function(v, dim = 8, s = 0.001) {
  f <- 2^(0:(dim / 2 - 1)) * pi * s
  as.numeric(rbind(sin(f * v), cos(f * v)))
}

test_that("fourier_embedding interleaves sin and cos at doubling frequencies", {
  expect_equal(morie_sdxlcd_fourier_embedding(512), .sx_ff(512), tolerance = 1e-12)
  expect_equal(morie_sdxlcd_fourier_embedding(3, dim = 4, scale = 0.5), .sx_ff(3, 4, 0.5), tolerance = 1e-12)
  expect_equal(morie_sdxlcd_fourier_embedding(0, 2), c(0, 1))
  expect_error(morie_sdxlcd_fourier_embedding(1, dim = 3), "even")
})

test_that("size and crop conditioning embed each coordinate", {
  s <- morie_sdxlcd_size_conditioning(768, 1024)
  expect_equal(s$c_size, c(768, 1024))
  expect_equal(s$embedding, c(.sx_ff(768), .sx_ff(1024)), tolerance = 1e-12)
  expect_error(morie_sdxlcd_size_conditioning(0, 5), "positive")
  cr <- morie_sdxlcd_crop_conditioning(10, 0, dim = 4)
  expect_equal(cr$embedding, c(.sx_ff(10, 4), .sx_ff(0, 4)), tolerance = 1e-12)
  expect_false(cr$object_centred)
  expect_true(morie_sdxlcd_crop_conditioning()$object_centred)
  expect_error(morie_sdxlcd_crop_conditioning(-1, 0), "negative")
})

test_that("sample_crop draws offsets uniformly on the package stream", {
  c1 <- morie_sdxlcd_sample_crop(100, 80, 64, 64, .ghc_rng(3))
  e <- .ghc_rng(3)
  u <- .ghc_unif(e, 2L)
  expect_equal(c1$c_top, floor(u[1] * 37))
  expect_equal(c1$c_left, floor(u[2] * 17))
  expect_equal(morie_sdxlcd_sample_crop(64, 64, 64, 64, .ghc_rng(1)), list(c_top = 0L, c_left = 0L))
  expect_error(morie_sdxlcd_sample_crop(10, 10, 11, 5, .ghc_rng(1)), "larger than the image")
})

test_that("discarded_fraction counts images below the minimum side", {
  sz <- list(c(200, 400), c(300, 300), c(512, 100), c(1024, 768))
  d <- morie_sdxlcd_discarded_fraction(sz)
  expect_equal(d$discarded, 2)
  expect_equal(d$fraction, 0.5)
  expect_equal(d$kept_with_conditioning, 4L)
  expect_equal(morie_sdxlcd_discarded_fraction(do.call(rbind, sz), minimum = 150)$discarded, 1)
  expect_error(morie_sdxlcd_discarded_fraction(1:4), "list of pairs")
  expect_error(morie_sdxlcd_discarded_fraction(list()), "no image sizes")
})

test_that("aspect_ratio_buckets round h = sqrt(P/a), w = a h to the multiple", {
  b <- morie_sdxlcd_aspect_ratio_buckets(c(1, 16 / 9, 0.5))
  for (k in 1:3) {
    a <- c(1, 16 / 9, 0.5)[k]
    h <- round(sqrt(1024^2 / a) / 64) * 64
    w <- round(a * sqrt(1024^2 / a) / 64) * 64
    expect_equal(b$buckets[[k]]$height, h)
    expect_equal(b$buckets[[k]]$width, w)
    expect_equal(b$buckets[[k]]$pixel_error, abs(h * w - 1024^2) / 1024^2, tolerance = 1e-12)
  }
  expect_equal(b$max_pixel_error, max(vapply(b$buckets, function(x) x$pixel_error, 0)))
  expect_equal(morie_sdxlcd_aspect_ratio_buckets(1, pixels = 100, multiple = 64)$buckets[[1]]$height, 64)
  expect_equal(morie_sdxlcd_aspect_ratio_buckets(numeric(0))$max_pixel_error, 0)
  expect_error(morie_sdxlcd_aspect_ratio_buckets(-1), "positive")
})

test_that("condition_vector concatenates and adds the timestep embedding", {
  base <- c(.sx_ff(600, 4), .sx_ff(800, 4), .sx_ff(5, 4), .sx_ff(7, 4))
  v <- morie_sdxlcd_condition_vector(600, 800, 5, 7, dim = 4)
  expect_equal(v$vector, base, tolerance = 1e-12)
  expect_equal(v$width, 16L)
  te <- seq(-1, 1, length.out = 16)
  for (fn in list(morie_sdxlcd_condition_vector, morie_sdxlcd)) {
    expect_equal(fn(600, 800, 5, 7, te, dim = 4)$estimate, te + base, tolerance = 1e-12)
  }
  expect_error(morie_sdxlcd_condition_vector(600, 800, timestep_embedding = 1:3, dim = 4), "3 wide")
})

test_that("morie_sdxlcd_cheatsheet names both conditionings", {
  s <- morie_sdxlcd_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "SIZE:")
  expect_match(s, "CROP:")
})
