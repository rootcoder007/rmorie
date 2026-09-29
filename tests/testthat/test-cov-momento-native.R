# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/momento_native.R (MOMENT masked time-series
# pretraining, Goswami et al. 2024). Patches are recomputed from the
# standardised series, the loss as the mean squared error over masked
# positions only, and the task masks from their definitions.

.mo_x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
.mo_M <- cbind(c(1, 2, 3, 4, 5, 6), c(2, 2, 2, 2, 2, 2))

test_that("harmonise standardises each channel and cuts it into patches", {
  for (fn in list(morie_momento_harmonise, momento_harmonise, morie_momento)) {
    h <- fn(list(.mo_x, .mo_M), 3)
    x9 <- .mo_x[1:9]
    z <- (x9 - mean(x9)) / sd(x9)
    expect_equal(h$n_series, 3L)
    expect_equal(h$n_patches, 2L)
    expect_equal(unname(unlist(h$batch[[1]][1:2])), z[1:6], tolerance = 1e-12)
    expect_equal(unname(unlist(h$batch[[2]])), (1:6 - 3.5) / sd(1:6), tolerance = 1e-12)
    # a constant channel standardises to zeros
    expect_equal(unname(unlist(h$batch[[3]])), rep(0, 6))
    expect_equal(h$meta[[1]]$mean, mean(x9), tolerance = 1e-12)
    expect_equal(h$meta[[1]]$sd, sd(x9), tolerance = 1e-12)
    expect_equal(h$meta[[1]]$n_patches, 3L)
    raw <- fn(list(.mo_x), 4, normalise = FALSE)
    expect_equal(unname(unlist(raw$batch[[1]])), .mo_x[1:8])
    expect_error(fn(list(), 3), "no series")
    expect_error(fn(list(.mo_x), 0), "at least 1")
    expect_error(fn(list(1:2), 3), "fewer than one patch")
  }
})

test_that("mask_patches zeros the listed 0-based patches", {
  p <- list(c(1, 2), c(3, 4), c(5, 6), c(7, 8))
  for (fn in list(morie_momento_mask_patches, momento_mask_patches)) {
    m <- fn(p, c(2, 0, 2))
    expect_identical(m$mask_idx, c(0L, 2L))
    expect_identical(m$mask, c(TRUE, FALSE, TRUE, FALSE))
    expect_equal(m$masked, list(c(0, 0), c(3, 4), c(0, 0), c(7, 8)))
    expect_equal(m$mask_rate, 0.5)
    expect_equal(fn(p, 3, fill = -1)$masked[[4]], c(-1, -1))
    expect_error(fn(p, 4), "outside 0..3")
    expect_error(fn(p, integer(0)), "nothing was masked")
    expect_error(fn(p, 0:3), "every patch")
  }
})

test_that("masked_loss scores masked positions only", {
  tr <- list(c(1, 2), c(3, 4), c(5, 6))
  rc <- list(c(1.5, 2), c(0, 0), c(5, 7))
  mk <- c(TRUE, FALSE, TRUE)
  for (fn in list(morie_momento_masked_loss, momento_masked_loss)) {
    r <- fn(tr, rc, mk)
    expect_equal(r$mse, (0.25 + 0 + 0 + 1) / 4)
    expect_equal(r$n_scored, 4)
    expect_error(fn(tr, rc[1:2], mk), "agree in length")
    expect_error(fn(tr, list(1, 2, 3), mk), "differs in length")
    expect_error(fn(tr, rc, rep(FALSE, 3)), "undefined")
  }
})

test_that("task_mask puts the gap at the tail or in the interior", {
  for (fn in list(morie_momento_task_mask, momento_task_mask)) {
    expect_identical(fn(10, "forecast", 3), 7:9)
    expect_identical(fn(10, "classify", 2), 8:9)
    expect_identical(fn(10, "anomaly", 1), 9L)
    expect_identical(fn(10, "impute", 4), 3:6)
    expect_identical(fn(10, "impute", 2, start = 1), 1:2)
    expect_error(fn(10, "impute", 4, start = 8), "past the end")
    expect_error(fn(10, "embed"), "task must be one of")
    expect_error(fn(10, "forecast", 10), "span must lie")
  }
})

test_that("reconstruction_curve masks round(rate n) random patches per rate", {
  p <- lapply(1:8, function(i) c(i, -i))
  zero_rec <- function(masked, mask) masked
  r <- morie_momento_reconstruction_curve(p, zero_rec, c(0.25, 0.5), seed = 4)
  e <- .ghc_rng(4)
  for (k in 1:2) {
    m <- c(2L, 4L)[k]
    idx <- order(.ghc_unif(e, 8L))[seq_len(m)]
    expect_equal(r$mse[k], mean(unlist(p[idx])^2), tolerance = 1e-12)
  }
  expect_equal(r$rates, c(0.25, 0.5))
  set.seed(9)
  idx <- sample.int(8, 2)
  r2 <- momento_reconstruction_curve(p, zero_rec, 0.25, seed = 9)
  expect_equal(r2$mse, mean(unlist(p[idx])^2), tolerance = 1e-12)
  expect_equal(r2$curve[[1]]$n_masked, 2L)
  # extreme rates are clamped to 1 .. n - 1 masked patches
  r3 <- momento_reconstruction_curve(p, zero_rec, c(0, 1), seed = 1)
  expect_equal(r3$rates, c(1, 7) / 8)
})

test_that("momento_cheatsheet names the masked loss", {
  s <- momento_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "MASKED positions only")
})
