# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Bfassum rejects a non-function kernel", {
  expect_error(Bfassum(kernel = 3), "kernel must be NULL or a function")
})

test_that("Bfassum accepts a custom kernel and no g", {
  r <- Bfassum(kernel = function(v) stats::dnorm(v))
  expect_true(r$d1)
  expect_true(r$d2)
  expect_true(is.na(r$d3))
  expect_true(is.na(r$d4))
  expect_identical(r$monotone, "unknown")
})

test_that("Bfassum classifies monotonicity of g", {
  expect_identical(Bfassum(g = exp, h = 0.1, n = 100)$monotone, "increasing")
  expect_true(Bfassum(g = exp, h = 0.1, n = 100)$d4)
  expect_identical(Bfassum(g = function(t) -t)$monotone, "decreasing")
  expect_identical(Bfassum(g = function(t) t^2)$monotone, "neither")
})
