test_that("STKriging and STVariogram equal gstat::krigeST and variogramST", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("spacetime")
  skip_if_not_installed("xts")
  u <- .morie_random_uniform(200, seed = 41, stream = 0)
  P <- cbind(3 * u[1:6], 3 * u[7:12])
  Tt <- 0:4
  Z <- t(sapply(0:4, function(t) 1 + u[21 + 6 * t + 0:5] + 0.3 * t))
  tt <- as.POSIXct("2020-01-01", tz = "UTC") + Tt * 86400
  st <- spacetime::STFDF(sp::SpatialPoints(P), tt, data.frame(z = as.vector(t(Z))))
  NP <- rbind(c(1, 1), c(2.5, 0.4), c(0.3, 2.2))
  nst <- spacetime::STF(sp::SpatialPoints(NP), as.POSIXct("2020-01-01", tz = "UTC") + c(1, 2, 3) * 86400)
  ours <- list(type = "productSum", k = 0.5, space = list(model = "Sph", psill = 0.8, range = 2, nugget = 0.05),
               time = list(model = "Exp", psill = 0.6, range = 1.5))
  mod <- gstat::vgmST("productSum", space = gstat::vgm(0.8, "Sph", 2, 0.05), time = gstat::vgm(0.6, "Exp", 1.5), k = 0.5)
  attr(mod, "temporal unit") <- "days"
  kr <- gstat::krigeST(z ~ 1, st, nst, mod, computeVar = TRUE, progress = FALSE)
  r <- STKriging(as.vector(t(Z)), P[rep(1:6, 5), ], rep(Tt, each = 6), NP[rep(1:3, 3), ], rep(1:3, each = 3), ours)
  expect_equal(r$prediction, kr$var1.pred, tolerance = 1e-10)
  expect_equal(r$variance, kr$var1.var, tolerance = 1e-10)
  vg <- as.data.frame(gstat::variogramST(z ~ 1, st, tlags = 0:2, boundaries = c(0, 1, 2, 3.5), progress = FALSE))
  mine <- STVariogram(Z, P, Tt, 0:2, c(0, 1, 2, 3.5))
  expect_equal(mine$np, vg$np)
  expect_equal(mine$gamma[vg$np > 0], vg$gamma[vg$np > 0], tolerance = 1e-12)
})
