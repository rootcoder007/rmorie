test_that("inequality and accessibility equal ineq and SpatialAcc", {
  skip_if_not_installed("ineq")
  skip_if_not_installed("SpatialAcc")
  u <- .morie_random_uniform(200, seed = 67, stream = 0)
  x <- 1 + 30 * u[1:40]
  expect_equal(SpatialGini(x)$gini, ineq::Gini(x), tolerance = 1e-14)
  expect_equal(TheilDecomposition(x)$T, ineq::Theil(x), tolerance = 1e-14)
  expect_equal(TheilDecomposition(x)$L, ineq::Theil(x, parameter = 1), tolerance = 1e-14)
  D <- matrix(0.2 + 9 * u[41:160], 30, 4)
  P <- round(50 + 500 * u[161:190])
  S <- c(3, 8, 5, 2)
  expect_equal(FcaAccessibility(S, P, D, 5)$access, SpatialAcc::ac(P, S, D, 5, family = "2SFCA"), tolerance = 1e-14)
  expect_equal(FcaAccessibility(S, P, D, 5, "KD2SFCA", power = 1)$access,
               SpatialAcc::ac(P, S, D, 5, power = 1, family = "KD2SFCA"), tolerance = 1e-14)
})
