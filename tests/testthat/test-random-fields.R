P <- rbind(c(0, 0), c(1, 0), c(0, 2), c(1.5, 1.5))
M <- list(model = "Exp", psill = 4, range = 1.2, nugget = 1)
U <- list(model = "Exp", psill = 0.8, range = 1.2, nugget = 0.2)

test_that("transforms reproduce the Gaussian draws", {
  z <- CholeskySim(P, U, nsim = 2, seed = 5)$simulations
  expect_equal(TransformedField(P, M, "lognormal", seed = 5, nsim = 2, mean = 1, sd = 0.5)$field, exp(1 + 0.5 * z))
  cuts <- qnorm(c(0.2, 0.7))
  cat_f <- TransformedField(P, M, "categorical", seed = 5, nsim = 2, proportions = c(0.2, 0.5, 0.3))$field
  expect_equal(cat_f, (z > cuts[1]) + (z > cuts[2]))
  z1 <- CholeskySim(P, U, nsim = 2, seed = 5 + 7919)$simulations
  z2 <- CholeskySim(P, U, nsim = 2, seed = 5 + 2 * 7919)$simulations
  expect_equal(TransformedField(P, M, "chi2", seed = 5, nsim = 2, df = 3)$field, z^2 + z1^2 + z2^2)
  expect_equal(TransformedField(rbind(c(0, 0), c(1, 0)), list(model = "Exp", psill = 1, range = 1), "binary",
                                seed = 3)$field[1, ], c(1, 0))
  expect_error(TransformedField(P, M, "bogus"), "unknown")
})

test_that("max-stable field and anisotropy", {
  r <- MaxStableField(P, M, n_fields = 50, seed = 4)
  W <- CholeskySim(P, U, nsim = 50, seed = 4)$simulations
  zeta <- 1 / cumsum(-log(.morie_random_uniform(50, seed = 4, stream = 2000)))
  expect_equal(r$field, sqrt(2 * pi) * apply(zeta * pmax(W, 0), 2, max))
  expect_equal(unname(AnisotropicCoords(rbind(c(1, 0), c(0, 1)), 90, 0.5)), rbind(c(0, 1), c(-2, 0)))
})
