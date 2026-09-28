test_that("IdwPredict and IdwCv equal gstat", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  u <- .morie_random_uniform(90, seed = 97, stream = 0)
  P <- cbind(6 * u[1:25], 6 * u[26:50])
  z <- 1 + P[, 1] + u[51:75]
  Q <- cbind(c(1, 3, 5), c(2, 4, 1))
  d <- data.frame(x = P[, 1], y = P[, 2], z = z)
  sp::coordinates(d) <- ~ x + y
  nd <- data.frame(x = Q[, 1], y = Q[, 2])
  sp::coordinates(nd) <- ~ x + y
  expect_equal(IdwPredict(z, P, Q, power = 2.5, nmax = 6)$prediction,
               gstat::idw(z ~ 1, d, nd, idp = 2.5, nmax = 6, debug.level = 0)$var1.pred, tolerance = 1e-12)
  expect_equal(IdwCv(z, P)$prediction, gstat::krige.cv(z ~ 1, d, verbose = FALSE)$var1.pred, tolerance = 1e-12)
})
