test_that("probability kriging equals gstat ordinary cokriging of indicator and uniform scores", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  u <- .morie_random_uniform(200, seed = 71)
  P <- cbind(10 * u[1:30], 10 * u[31:60])
  z <- sin(P[, 1] / 3) + P[, 2] / 5 + u[61:90]
  thr <- stats::median(z)
  tg <- rbind(c(2, 3), c(7.5, 6), c(4.4, 1.2))
  o <- ProbabilityKriging(P, z, tg, thr, c(0.02, 0.2, 3), c(0.01, 0.07, 3), c(0.01, 0.1, 3))
  d <- data.frame(x = P[, 1], y = P[, 2], I = as.numeric(z <= thr), U = o$uniform)
  sp::coordinates(d) <- ~ x + y
  g <- gstat::gstat(NULL, "I", I ~ 1, d, model = gstat::vgm(0.2, "Exp", 3, 0.02))
  g <- gstat::gstat(g, "U", U ~ 1, d, model = gstat::vgm(0.07, "Exp", 3, 0.01))
  g <- gstat::gstat(g, c("I", "U"), model = gstat::vgm(0.1, "Exp", 3, 0.01))
  nd <- data.frame(x = tg[, 1], y = tg[, 2])
  sp::coordinates(nd) <- ~ x + y
  pr <- suppressMessages(stats::predict(g, nd))
  expect_equal(o$estimate, pr$I.pred, tolerance = 1e-9)
})
