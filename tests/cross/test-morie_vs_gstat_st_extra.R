test_that("StFit, StUniversalKriging and StLocalKriging agree with gstat", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("spacetime")
  u <- .morie_random_uniform(400, seed = 43, stream = 0)
  n <- 12
  P <- cbind(4 * u[1:n], 4 * u[n + 1:n])
  Tt <- 0:5
  x <- u[25 + seq_len(n * 6)]
  Z <- matrix(1 + 2 * x + sin(P[rep(1:n, 6), 1]) + 0.2 * rep(Tt, each = n) + 0.3 * u[100 + seq_len(n * 6)], n)
  tt <- as.POSIXct("2020-01-01", tz = "UTC") + Tt * 86400
  st <- spacetime::STFDF(sp::SpatialPoints(P), tt, data.frame(z = as.vector(Z), x = x))
  vg <- gstat::variogramST(z ~ 1, st, tlags = 0:3, boundaries = c(0, 1, 2, 3, 4), progress = FALSE)
  vgd <- as.data.frame(vg)
  vgd <- vgd[!is.na(vgd$gamma) & vgd$np > 0, ]
  m0 <- gstat::vgmST("productSum", space = gstat::vgm(0.3, "Exp", 1.5, 0.05), time = gstat::vgm(0.3, "Exp", 2, 0.05),
                     k = 1)
  ref <- gstat::fit.StVariogram(vg, m0)
  r <- StFit(vgd$dist, as.numeric(vgd$timelag), vgd$gamma, vgd$np, "productSum",
             c(0.3, 1.5, 0.05, 0.3, 2, 0.05, 1))
  expect_lte(r$mse, attr(ref, "MSE") * (1 + 1e-8))
  mod <- gstat::vgmST("metric", joint = gstat::vgm(1, "Exp", 2, 0.05), stAni = 1.3)
  attr(mod, "temporal unit") <- "days"
  ours <- list(type = "metric", stAni = 1.3, joint = list(model = "Exp", psill = 1, range = 2, nugget = 0.05))
  NP <- rbind(c(1, 1.2), c(2.6, 0.7), c(0.4, 3.1))
  nx <- c(0.2, 0.5, 0.9)
  nst <- spacetime::STFDF(sp::SpatialPoints(NP), as.POSIXct("2020-01-01", tz = "UTC") + c(1, 3) * 86400,
                          data.frame(x = rep(nx, 2)))
  kr <- gstat::krigeST(z ~ x, st, nst, mod, computeVar = TRUE, progress = FALSE)
  uk <- StUniversalKriging(as.vector(Z), cbind(1, x), P[rep(1:n, 6), ], rep(Tt, each = n), cbind(1, rep(nx, 2)),
                           NP[rep(1:3, 2), ], rep(c(1, 3), each = 3), ours)
  expect_equal(uk$prediction, kr$var1.pred, tolerance = 1e-10)
  expect_equal(uk$variance, kr$var1.var, tolerance = 1e-10)
  kl <- gstat::krigeST(z ~ 1, st, nst, mod, nmax = 10, stAni = 1.3 / 86400, computeVar = TRUE, progress = FALSE)
  lk <- StLocalKriging(as.vector(Z), P[rep(1:n, 6), ], rep(Tt, each = n), NP[rep(1:3, 2), ], rep(c(1, 3), each = 3),
                       ours, nmax = 10, stani = 1.3)
  expect_equal(lk$prediction, kl$var1.pred, tolerance = 1e-10)
  expect_equal(lk$variance, kl$var1.var, tolerance = 1e-10)
})
