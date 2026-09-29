.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("deSolve")

test_that("Sirepi equals deSolve::ode with the rk4 method", {
  f <- function(t, y, p) {
    inf <- p[1] * y[1] * y[2] / 1000
    list(c(-inf, inf - p[2] * y[2], p[2] * y[2]))
  }
  tt <- seq(0, 60, by = 0.25)
  o <- deSolve::ode(c(990, 10, 0), tt, f, c(0.3, 0.1), method = "rk4")
  r <- Sirepi(990, 10, 0, 0.3, 0.1, 60, dt = 0.25)
  expect_equal(r$S, unname(o[, 2]), tolerance = 1e-12)
  expect_equal(r$I, unname(o[, 3]), tolerance = 1e-12)
})
