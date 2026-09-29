test_that("space-time cokriging at a single time equals gstat ordinary cokriging", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  u <- .morie_random_uniform(300, seed = 81)
  P <- cbind(10 * u[1:24], 10 * u[31:54])
  v <- (0:23) %% 2
  z <- sin(P[, 1] / 3) + u[91:114]
  B <- rbind(c(1, 0.6), c(0.6, 0.8))
  tg <- rbind(c(2, 3), c(7, 7), c(5.5, 1.5))
  o <- StCokriging(P, rep(0, 24), v, z, tg, rep(0, 3), B, 3, 2, c(0.05, 0.02))
  d0 <- data.frame(x = P[v == 0, 1], y = P[v == 0, 2], a = z[v == 0])
  sp::coordinates(d0) <- ~ x + y
  d1 <- data.frame(x = P[v == 1, 1], y = P[v == 1, 2], b = z[v == 1])
  sp::coordinates(d1) <- ~ x + y
  g <- gstat::gstat(NULL, "a", a ~ 1, d0, model = gstat::vgm(1, "Exp", 3, 0.05))
  g <- gstat::gstat(g, "b", b ~ 1, d1, model = gstat::vgm(0.8, "Exp", 3, 0.02))
  g <- gstat::gstat(g, c("a", "b"), model = gstat::vgm(0.6, "Exp", 3, 0))
  nd <- data.frame(x = tg[, 1], y = tg[, 2])
  sp::coordinates(nd) <- ~ x + y
  pr <- suppressMessages(stats::predict(g, nd))
  expect_equal(o$estimate, pr$a.pred, tolerance = 1e-9)
  expect_equal(o$variance, pr$a.var, tolerance = 1e-9)
})
