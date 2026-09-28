test_that("SpectralGRF embedding reproduces the covariance and is deterministic", {
  g <- cbind(rep(0:4 * 0.5, each = 4), rep(0:3 * 0.5, 5))
  r <- SpectralGRF(g, "matern", list(range = 0.4, nu = 1.5, sill = 2), seed = 1)
  N <- prod(r$torus)
  cc <- Re(stats::fft(r$eigenvalues, inverse = TRUE)) / N
  h <- sqrt((0.5 * 2)^2 + (0.5 * 3)^2) / 0.4
  expect_equal(cc[3, 4], 2 * 2^(1 - 1.5) / gamma(1.5) * h^1.5 * besselK(h, 1.5), tolerance = 1e-12)
  expect_true(r$embedding_exact)
  expect_identical(SpectralGRF(g, "gaussian", list(range = 2), n_sims = 2, seed = 3)$simulations,
                   SpectralGRF(g, "gaussian", list(range = 2), n_sims = 2, seed = 3)$simulations)
})

test_that("PowerLawField, HistogramTransform and CoherentFields", {
  f <- PowerLawField(8, 6, beta = 1.7, dx = 0.5, seed = 3)
  expect_lt(abs(sum(f$field)), 1e-12)
  expect_equal(HistogramTransform(c(.3, -1.2, .8, .1), c(10, 40, 20, 30))$transformed, c(30, 10, 40, 20))
  g <- as.matrix(expand.grid(0:2, 0:2))
  one <- CoherentFields(g, coherence = 1, seed = 2)
  expect_equal(one$first, one$second)
})

test_that("ThinPlateSpline and RandomPhaseField", {
  P <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(.5, .5), c(.2, .9))
  y <- 1 + 2 * P[, 1] - 3 * P[, 2]
  expect_equal(ThinPlateSpline(P, y, lam = 0.5)$fitted, y, tolerance = 1e-12)
  expect_equal(round(ThinPlateSpline(P[1:5, ], c(0, 1, 1, 2, 1.5), newdata = rbind(c(.5, .5), c(.25, .75)))$predicted, 6),
               c(1.5, 1.294285))
  expect_equal(round(RandomPhaseField(rbind(c(0, 0), c(1, .5)), "gaussian", n_waves = 4, seed = 2)$field, 6),
               c(0.481186, -0.249627))
})
