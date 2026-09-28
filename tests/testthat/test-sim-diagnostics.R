test_that("simulation diagnostics recompute", {
  r <- ConditionalTurningBands(c(1.5, 0.2, -0.4), rbind(c(0, 0), c(2, 1), c(1, 3)), rbind(c(0, 0), c(1, 1)),
                               range_ = 2, seed = 4)
  expect_equal(r$field[1], 1.5, tolerance = 1e-9)
  s <- StandardiseRealisations(list(c(1, 2, 6)), mean = 5, variance = 4)$realisations[[1]]
  expect_equal(c(mean(s), var(s)), c(5, 4), tolerance = 1e-12)
  P <- expand.grid(i = 0:3, j = 0:3)
  d <- DirectionalVariogram(as.numeric(P$i), as.matrix(P), c(0, 90), c(0, 1.5), tol = 10)
  expect_equal(unlist(d$gamma), c(0, 0.5))
  full <- Connectivity(list(rep(1, 25)), 5, 5, 1, pairs = list(c(1, 25)))
  expect_equal(c(full$n_components, full$pair_probability), c(1, 1))
  expect_equal(IndicatorVariogram(list(c(1, 0, 1, 0)), 1, 4, 1, c(1, 2))$mean_x, c(0.5, 0))
})
