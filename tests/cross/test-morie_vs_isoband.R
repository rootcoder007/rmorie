# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: Contours against the isoband package (isobands). Band
# areas coincide when the field is linear (then the triangle interpolant and
# the marching-squares polygon agree).

library(testthat)
library(rmorie)

xs <- seq(0, 3, by = 0.5)
ys <- seq(0, 2, by = 0.5)
lin <- outer(ys, xs, function(y, x) 0.7 * x - 1.3 * y + 2)

test_that("band areas of a linear field equal isoband::isobands", {
  skip_if_not_installed("isoband")
  lo <- c(-1, 1, 2, 3)  # the field ranges from -0.6 to 4.1, so the bands cover the domain
  hi <- c(1, 2, 3, 5)
  ref <- isoband::isobands(xs, ys, lin, levels_low = lo, levels_high = hi)
  mine <- ContourFill(lin, xs, ys, c(lo, 5))
  area <- function(x, y) abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2
  for (k in seq_along(lo)) {
    b <- ref[[k]]
    a <- 0
    for (id in unique(b$id)) a <- a + area(b$x[b$id == id], b$y[b$id == id])
    expect_equal(mine$areas[k], a, tolerance = 1e-10)
  }
  expect_equal(sum(mine$areas), 3 * 2, tolerance = 1e-10)
})
