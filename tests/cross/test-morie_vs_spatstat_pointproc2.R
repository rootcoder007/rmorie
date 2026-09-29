# Cross test: Berman-Turner fits against spatstat ppm with the same grid quadrature.

test_that("BermanTurnerFit equals spatstat ppm with the same grid quadrature", {
  skip_if_not_installed("spatstat.model")
  suppressMessages(library(spatstat.model))
  i <- 0:39
  pts <- cbind(0.5 + 0.35 * sin(i * 1.3), 0.5 + 0.35 * cos(i * 0.7 + (i %% 3)))
  X <- spatstat.geom::ppp(pts[, 1], pts[, 2], c(0, 1), c(0, 1))
  D <- spatstat.geom::ppp(rep((0:5 + 0.5) / 6, 5), rep((0:4 + 0.5) / 5, each = 6), c(0, 1), c(0, 1))
  Q <- spatstat.geom::quadscheme(X, D, method = "grid", ntile = c(6, 5))
  ref <- ppm(Q ~ x + I(y^2))
  got <- BermanTurnerFit(pts, c(0, 1, 0, 1), function(x, y) c(x, y * y), 6, 5)
  expect_equal(got$coefficients, unname(stats::coef(ref)), tolerance = 1e-8)
})
