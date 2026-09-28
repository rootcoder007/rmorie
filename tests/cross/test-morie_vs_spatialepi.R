test_that("disease mapping equals spdep and SpatialEpi", {
  skip_if_not_installed("spdep")
  skip_if_not_installed("SpatialEpi")
  skip_if_not_installed("MASS")
  u <- .morie_random_uniform(80, seed = 43, stream = 0)
  pop <- round(400 + 2500 * u[1:20])
  y <- floor(pop * 0.006 * exp(2 * (u[21:40] - 0.5)))
  nb <- lapply(1:20, function(i) as.integer(setdiff(c(i - 1, i + 1), c(0, 21))))
  nbo <- nb
  class(nbo) <- "nb"
  expect_equal(EbGlobal(y, pop)$estimate, spdep::EBest(y, pop)$estmm, tolerance = 1e-14)
  expect_equal(EbLocal(y, pop, nb)$estimate, spdep::EBlocal(y, pop, nbo)$est, tolerance = 1e-14)
  expect_equal(ProbabilityMap(y, pop)$pmap, spdep::probmap(y, pop)$pmap, tolerance = 1e-13)
  E <- pop * sum(y) / sum(pop)
  b <- SpatialEpi::eBayes(y, E)
  expect_equal(PoissonGammaEb(y, E)$RR, b$RR, tolerance = 1e-7)
})
