test_that("one-factor loadings agree with stats::factanal", {
  set.seed(41)
  n <- 300
  f <- rnorm(n)
  X <- sapply(c(0.9, 0.7, 0.6, 0.8, 0.5), function(l) l * f + sqrt(1 - l^2) * rnorm(n))
  ref <- stats::factanal(X, factors = 1, control = list(opt = list(factr = 1)))
  r <- subscale_ea_ave(X)
  expect_equal(r$loadings, abs(as.numeric(ref$loadings)), tolerance = 1e-5)
  expect_equal(r$uniquenesses, as.numeric(ref$uniquenesses), tolerance = 1e-5)
})
