# Coverage tests for three simulators of R/CholeskyExtra.R: the
# Wendland-tapered Gaussian field, the linear model of coregionalisation
# and the nested-structure decomposition, each replayed from its
# covariance and the package normal stream.

ce_P <- cbind(c(0, 1, 2, 0.5, 1.5, 2.5), c(0, 0.3, 0, 1, 1.2, 0.8))
ce_exp <- list(model = "Exp", psill = 1.5, range = 1)

test_that("tapered simulation multiplies the covariance by a Wendland taper", {
  r <- TaperedSimulate(ce_P, ce_exp, theta = 1.6, nsim = 2, seed = 3, mean = 10)
  D <- as.matrix(dist(ce_P))
  u <- pmin(D / 1.6, 1)
  C <- 1.5 * exp(-D) * (1 - u)^4 * (4 * u + 1)
  expect_equal(r$cov, C, tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(r$sparsity, mean(D >= 1.6))
  L <- t(chol(C))
  expect_equal(r$simulations[[2]], 10 + as.vector(L %*% .morie_random_normal(6, seed = 3, stream = 1)), tolerance = 1e-12)
  expect_equal(WendlandTaper(c(0, 0.5, 2), 1, k = 0), c(1, 0.5^2, 0))
  expect_error(WendlandTaper(0.5, 1, k = 4), "k must be 0, 1, 2 or 3")
})

test_that("LMC co-simulation uses the Kronecker covariance", {
  comps <- list(list(rbind(c(1, 0.5), c(0.5, 2)), ce_exp), list(diag(c(0.2, 0.1)), list(model = "Gau", psill = 1, range = 2)))
  r <- LmcCosimulate(ce_P, comps, nsim = 1, seed = 2, means = c(1, -1))
  D <- as.matrix(dist(ce_P))
  C <- kronecker(comps[[1]][[1]], 1.5 * exp(-D)) + kronecker(comps[[2]][[1]], exp(-(D / 2)^2))
  expect_equal(r$cov, C, tolerance = 1e-12, ignore_attr = TRUE)
  x <- as.vector(t(chol(C)) %*% .morie_random_normal(12, seed = 2, stream = 0))
  expect_equal(r$simulations[[1]][[1]], 1 + x[1:6], tolerance = 1e-12)
  expect_equal(r$simulations[[1]][[2]], -1 + x[7:12], tolerance = 1e-12)
})

test_that("nested decomposition sums independent component fields", {
  comps <- list(ce_exp, list(model = "Sph", psill = 0.5, range = 3))
  r <- NestedDecomposition(ce_P, comps, seed = 4)
  D <- as.matrix(dist(ce_P))
  f1 <- as.vector(t(chol(1.5 * exp(-D))) %*% .morie_random_normal(6, seed = 4, stream = 0))
  h <- D / 3
  f2 <- as.vector(t(chol(0.5 * ifelse(h < 1, 1 - 1.5 * h + 0.5 * h^3, 0))) %*% .morie_random_normal(6, seed = 4, stream = 1))
  expect_equal(r$components, list(f1, f2), tolerance = 1e-12)
  expect_equal(r$total, f1 + f2, tolerance = 1e-12)
  expect_equal(r$nominal_share, c(0.75, 0.25))
  expect_equal(r$share, c(var(f1), var(f2)) / var(f1 + f2), tolerance = 1e-12)
})
