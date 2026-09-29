test_that("the default statistic is spdep's Moran's I with inverse-distance weights", {
  skip_if_not_installed("spdep")
  set.seed(23)
  n <- 30
  C <- cbind(runif(n, 0, 10), runif(n, 0, 10))
  z <- C[, 1] + rnorm(n)
  nb <- lapply(seq_len(n), function(i) setdiff(seq_len(n), i))
  class(nb) <- "nb"
  D <- as.matrix(dist(C))
  glist <- lapply(seq_len(n), function(i) 1 / D[i, nb[[i]]])
  lw <- spdep::nb2listw(nb, glist = glist, style = "B")
  ref <- spdep::moran(z, lw, n = n, S0 = spdep::Szero(lw))$I
  r <- monte_carlo_spatial_test(z, C, n_sim = 199, seed = 3)
  expect_equal(r$observed, ref, tolerance = 1e-12)
  mc <- spdep::moran.mc(z, lw, nsim = 199)
  expect_equal(unname(mc$statistic), r$observed, tolerance = 1e-12)
  expect_true(r$p_value < 0.05 && mc$p.value < 0.05)
})
