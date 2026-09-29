test_that("bayesian_fmi is the ratio of summed squared transitions to the energy sum of squares", {
  e <- c(3.2, 1.1, 4.7, 2.2, 2.9, 5.1, 0.4, 3.3)
  r <- bayesian_fmi(e)
  expect_equal(r$bfmi, sum(diff(e)^2) / sum((e - mean(e))^2), tolerance = 1e-13)
  expect_equal(r$energy_var, var(e), tolerance = 1e-13)
  expect_false(bayesian_fmi(cumsum(rep(1, 100)))$adequate)
  expect_equal(bayesian_fmi(c(1, 3, 2, 5, 4))$bfmi, 1.5)
})
