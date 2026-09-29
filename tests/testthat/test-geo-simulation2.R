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

test_that("ensembles, p-fields, co-simulation, SIS, SNESIM and annealing", {
  e <- ConditionalEnsemble(dc, dv, rbind(c(0.5, 0.5), c(1, 1)), m, n_real = 5)
  expect_equal(c(e$etype[1], e$variance[1]), c(0.8, 0))
  z <- .morie_random_normal(2, seed = 5, stream = 0)
  rho <- exp(-0.5)
  expect_equal(PfieldSimulate(c(1, 2), c(0.5, 0.3), rbind(c(0, 0), c(1, 0)), m, seed = 5),
               c(1 + 0.5 * z[1], 2 + 0.3 * (rho * z[1] + sqrt(1 - rho^2) * z[2])), tolerance = 1e-12)
  expect_equal(CollocatedCosimulate(dc, dv, tg, rep(0.1, 13), m, 0.5)$simulated[13], 0.8)
  s <- SisMarkovBayes(dc, c(0, 1, 1), tg, c(0.5, 0.5), list(m, m), soft = matrix(c(0.6, 0.4), 13, 2, byrow = TRUE), B = c(0.5, 0.5))
  expect_equal(s$simulated[13], 0L)
  ti <- outer(0:7, 0:7, function(j, i) (i %/% 2 + j %/% 2) %% 2)
  expect_equal(SnesimSimulate(ti, 5, 4, list(c(1, 0), c(0, 1)), conditioning = rbind(c(2, 1, 1)))$grid[2, 3], 1)
  a <- AnnealingSimulate(sin(0:35), 6, 6, c(1, 2), c(0.3, 0.6), n_iter = 600, every = 50)
  expect_equal(sort(as.numeric(a$grid)), sort(sin(0:35)))
})
