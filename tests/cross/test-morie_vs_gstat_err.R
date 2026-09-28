test_that("FilteredKrige equals gstat krige with an Err component", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  u <- .morie_random_uniform(90, seed = 37, stream = 0)
  P <- cbind(4 * u[1:30], 4 * u[31:60])
  z <- 1 + sin(P[, 1]) + 0.5 * P[, 2] + 0.3 * u[61:90]
  d <- data.frame(x = P[, 1], y = P[, 2], z = z)
  sp::coordinates(d) <- ~ x + y
  nd <- data.frame(x = c(1.1, 3.3, P[4, 1]), y = c(2.2, 0.7, P[4, 2]))
  sp::coordinates(nd) <- ~ x + y
  vm <- gstat::vgm(1, "Exp", 1.5, add.to = gstat::vgm(0.2, "Err", 0))
  g <- gstat::krige(z ~ 1, d, nd, vm, debug.level = 0)
  f <- FilteredKrige(z, P, sp::coordinates(nd), list(model = "Exp", psill = 1, range = 1.5), 0.2)
  expect_equal(f$prediction, g$var1.pred, tolerance = 1e-10)
  expect_equal(f$variance, g$var1.var, tolerance = 1e-10)
})
