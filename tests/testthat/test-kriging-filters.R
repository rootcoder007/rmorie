u <- .morie_random_uniform(90, seed = 31, stream = 0)
P <- cbind(4 * u[1:30], 4 * u[31:60])
z <- 1 + sin(P[, 1]) + 0.5 * P[, 2] + 0.3 * u[61:90]
M <- list(model = "Exp", psill = 1, range = 1.5)
Q <- rbind(c(1.1, 2.2), c(3.3, 0.7))

test_that("filters, indicator ccdf, efficiency and collocated co-kriging", {
  f <- FilteredKrige(z, P, Q, M, 0.2)
  k <- Krige(z, P, Q, list(M, list(model = "Nug", psill = 0.2)))
  expect_equal(f$prediction, k$prediction, tolerance = 1e-12)
  expect_equal(f$variance, k$variance - 0.2, tolerance = 1e-12)
  r <- IndicatorCcdf(z, P, Q, c(1.5, 2.5, 3.5), M)
  expect_true(all(apply(r$ccdf, 1, function(G) all(diff(G) >= 0))))
  e <- KrigingEfficiency(z, P, Q, M)
  kk <- Krige(z, P, Q, M)
  expect_equal(e$efficiency, 1 - kk$variance, tolerance = 1e-12)
  c0 <- CollocatedCokriging(z, P, c(0.4, -0.3), Q, M, 0, mean_z = 2)
  expect_equal(c0$prediction, Krige(z, P, Q, M, beta = 2)$prediction, tolerance = 1e-12)
})
