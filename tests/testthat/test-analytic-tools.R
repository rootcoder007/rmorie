# Tests for AnalyticTools: Laplace transforms, Laurent coefficients, Walsh-Hadamard, polynomials.

test_that("Laplace transform and inversions reproduce closed forms", {
  s <- c(0.3, 1, 2.5)
  expect_equal(LaplaceTransformNum(function(t) t * exp(-0.5 * t) + sin(t), s), 1 / (s + 0.5)^2 + 1 / (s^2 + 1),
               tolerance = 1e-10)
  t <- c(0.2, 1, 3.7)
  expect_equal(TalbotInverse(function(s) 1 / (s * s + 1) + 1 / (s + 2)^2, t), sin(t) + t * exp(-2 * t), tolerance = 1e-10)
  expect_equal(StehfestInverse(function(s) 1 / (s * (s + 1)), t), 1 - exp(-t), tolerance = 1e-5)
})

test_that("LaurentCoefficients and HadamardTransform", {
  r <- LaurentCoefficients(function(z) exp(z) / (z * z), 0, c(-2, -1, 0, 1, 3), n = 64)
  expect_equal(r$real, 1 / factorial(c(-2, -1, 0, 1, 3) + 2), tolerance = 1e-14)
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  H <- matrix(1, 1, 1)
  for (k in 1:3) H <- rbind(cbind(H, H), cbind(H, -H))
  expect_equal(HadamardTransform(x), as.numeric(H %*% x) / sqrt(8), tolerance = 1e-12)
  expect_equal(HadamardInverse(HadamardTransform(x)), x, tolerance = 1e-12)
})

test_that("Polynomial expansion, factorisation and cancellation", {
  expect_equal(PolyExpand(list(c(1, 1)), 3), c(1, 3, 3, 1))
  f <- PolyExpand(list(c(1, 1), c(2, 0, 1), c(3, 1, 1), c(1, 2, 0, 1)), c(3, 2, 1, 2)) %% 5
  r <- PolyFactorModP(f, 5, seed = 3)
  prod <- r$unit
  for (i in seq_along(r$factors)) for (k in seq_len(r$multiplicities[i])) prod <- PolyExpand(list(prod, r$factors[[i]])) %% 5
  expect_equal(prod, f)
  expect_equal(PolyFactorModP(c(3, 0, 0, 0, 0, 0, 0, 1), 7)$multiplicities, 7)
  rc <- RationalCancel(PolyExpand(list(c(1, 1), c(2, 3)), c(2, 1)), PolyExpand(list(c(1, 1), c(4, 0, 2))))
  expect_equal(rc$numerator, c(2, 5, 3))
  expect_equal(rc$denominator, c(4, 0, 2))
})

test_that("HeatEquationSeries, QuadraticRoots and FunctionalNorm", {
  r <- HeatEquationSeries(function(z) sin(2 * pi * z / 3), 3, 0.4, 0.7, 0.5, n_terms = 4, n_quad = 2000)
  expect_equal(r$u[1, 1], sin(2 * pi * 0.7 / 3) * exp(-0.4 * (2 * pi / 3)^2 * 0.5), tolerance = 1e-12)
  q <- QuadraticRoots(1e-3, 1e4, 3)$real
  expect_true(all(abs(1e-3 * q^2 + 1e4 * q + 3) <= 1e-9 * pmax(1, abs(1e4 * q))))
  expect_equal(FunctionalNorm(c(0, 0.5, 1), c(0, 1, 2))$norm, sqrt(0.25 + 1.25), tolerance = 1e-15)
})
