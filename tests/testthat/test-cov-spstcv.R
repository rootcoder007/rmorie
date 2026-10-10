# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("spstcv checks eq (9.5) on a supplied design", {
  cs <- function(h) exp(-h / 2)
  ct <- function(k) exp(-abs(k))
  coords <- matrix(c(0, 0, 1, 0, 0, 1), ncol = 2, byrow = TRUE)
  times <- c(0, 1, 2)
  r <- spstcv(1, 2, cs, ct, coords = coords, times = times)
  expect_true(r$valid)
  expect_gt(r$min_eigenvalue, 0)
  expect_null(r$warning)
  # an invalid "covariance" fails the eigenvalue check and is flagged
  bad <- spstcv(1, 2, function(h) cos(3 * h) - 0.9, ct,
                coords = coords, times = times)
  expect_false(bad$valid)
  expect_lt(bad$min_eigenvalue, 0)
  expect_match(bad$warning, "eq \\(9.5\\) fails")
})
