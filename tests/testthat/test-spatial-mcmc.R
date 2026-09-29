# Tests for SpatialMcmc: spatial MCMC samplers on the Philox stream.

A <- outer(0:4, 0:4, function(i, j) as.numeric(abs(i - j) == 1))
tg <- CarPoissonTarget(c(3, 5, 2, 8, 4), c(3.2, 4.1, 3, 5.5, 3.9), A, tau = 2, rho = 0.8)

test_that("CarPoissonTarget gradient matches finite differences", {
  x <- c(0.1, 0.2, -0.1, 0, 0.3, -0.2)
  fd <- vapply(1:6, function(k) {
    e <- replace(numeric(6), k, 1e-6)
    (tg$logp(x + e) - tg$logp(x - e)) / 2e-6
  }, 0)
  expect_equal(tg$grad(x), fd, tolerance = 1e-6)
})

test_that("MhSpatial reproduces its streams and HMC/NUTS sample a standard normal", {
  r <- MhSpatial(tg$logp, rep(0, 6), 1, step = 0.2, seed = 9)
  prop <- 0.2 * .morie_random_normal(6, seed = 9, stream = 0)
  acc <- log(.morie_random_uniform(1, seed = 9, stream = 1)) < tg$logp(prop) - tg$logp(rep(0, 6))
  expect_equal(r$samples[1, ], if (acc) prop else rep(0, 6))
  h <- HmcSpatial(function(x) -0.5 * x^2, function(x) -x, 0, 800, eps = 0.3, n_leapfrog = 8, seed = 1)
  expect_lt(abs(mean(h$samples)), 0.15)
  nu <- NutsSpatial(function(x) -0.5 * x^2, function(x) -x, 0, 800, eps = 0.3, max_depth = 6, seed = 2)
  expect_lt(abs(mean(nu$samples^2) - 1), 0.25)
})
