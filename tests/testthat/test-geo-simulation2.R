# Tests for GeoSimulation2: geostatistical simulation on the Philox stream.

m <- list(model = "Exp", sill = 1, range = 2)
dc <- rbind(c(0.5, 0.5), c(4, 1), c(2, 3.5))
dv <- c(0.8, -0.4, 1.1)
g <- expand.grid(i = 0:3, j = 0:2)
tg <- rbind(cbind(g$i, g$j), c(0.5, 0.5))

test_that("SGS first node is the simple kriging draw", {
  r <- SgsBlockSimulate(dc, dv, tg, m, mean = 0.1, k = 10, seed = 3)
  expect_equal(r$simulated[13], 0.8)
  t <- r$path[1] + 1
  x <- tg[t, ]
  C <- exp(-as.matrix(dist(dc)) / 2)
  c0 <- exp(-sqrt(colSums((t(dc) - x)^2)) / 2)
  lam <- solve(C, c0)
  z0 <- .morie_random_normal(13, seed = 3, stream = 1)[1]
  expect_equal(r$simulated[t], 0.1 + sum(lam * (dv - 0.1)) + sqrt(1 - sum(lam * c0)) * z0, tolerance = 1e-10)
})

