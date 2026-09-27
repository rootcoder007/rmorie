mesf_a <- 1 * (abs(outer(1:12, 1:12, "-")) == 1)
mesf_y <- c(1, 1.4, 2.2, 2.9, 3.1, 2.6, 2, 1.1, .8, 1.5, 2.4, 2.7)
mesf_x <- cbind(1, c(.2, .5, .1, .9, .4, .3, .8, .6, .7, .05, .35, .55))

test_that("MoranEigenvectorFilter documented selection and fit", {
  r <- MoranEigenvectorFilter(mesf_y, mesf_x, mesf_a)
  expect_equal(r$selection$evec, c(0, 1, 3))
  expect_equal(r$stop_reason, "inversion")
  Z <- cbind(mesf_x, r$vectors)
  expect_equal(r$fitted, unname(stats::lm.fit(Z, mesf_y)$fitted.values), tolerance = 1e-10)
  expect_lt(max(abs(crossprod(mesf_x, r$vectors))), 1e-10)
})

test_that("step 0 is the Moran's I of the OLS residuals", {
  r <- MoranEigenvectorFilter(mesf_y, mesf_x, mesf_a)
  e <- stats::lm.fit(mesf_x, mesf_y)$residuals
  S <- 12 / sum(mesf_a) * mesf_a
  expect_equal(r$selection$moran[1], sum(e * (S %*% e)) / sum(e^2), tolerance = 1e-12)
  r10 <- MoranEigenvectorFilter(mesf_y[1:10], mesf_x[1:10, ], mesf_a[1:10, 1:10])
  expect_equal(r10$stop_reason, "exhausted")
  expect_equal(nrow(r10$selection), 5L)
})
