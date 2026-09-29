skip_if_not_installed("spatialprobit")
skip_if_not_installed("Matrix")

# spatialprobit estimates the direct effects from Monte Carlo traces of W^i (tracesWi), so only the
# exact total effects are compared; the direct effects are checked exactly in the unit tests.
test_that("spprmf total impacts match spatialprobit::marginal.effects", {
  set.seed(4)
  n <- 40
  xy <- cbind(stats::runif(n), stats::runif(n))
  D <- as.matrix(stats::dist(xy))
  W <- t(apply(D, 1, function(d) as.numeric(rank(d, ties.method = "first") %in% 2:5)))
  W <- W / rowSums(W)
  X <- cbind(1, stats::rnorm(n), stats::runif(n))
  b <- c(0.2, 0.8, -0.5)
  rho <- 0.45
  obj <- structure(list(nobs = n, nvar = 3, cflag = 1, ndraw = 1, nomit = 0, bdraw = matrix(b, 1), pdraw = rho,
                        X = X, W = Matrix::Matrix(W, sparse = TRUE)), class = "sarprobit")
  me <- utils::getFromNamespace("marginal.effects.sarprobit", "spatialprobit")
  ref <- me(obj, o = 200)
  got <- spprmf(b, rho, X, W)
  expect_equal(got$total, as.vector(ref$total), tolerance = 1e-10)
})
