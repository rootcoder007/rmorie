test_that("median radius, test R2, hyperplane and correlation distance follow their formulas", {
  expect_equal(morie_esl_median_nn_radius(500, 10)$median_radius, (1 - 0.5^(1 / 500))^(1 / 10), tolerance = 1e-14)
  expect_equal(round(morie_esl_median_nn_radius(500, 10)$median_radius, 2), 0.52)
  r <- morie_esl_test_r2(c(1, 2, 3), c(1.1, 1.8, 3.3), 2.1)
  expect_equal(c(r$mse, r$mse0), c(mean(c(0.1, -0.2, 0.3)^2), mean(c(1.1, 0.1, -0.9)^2)), tolerance = 1e-14)
  h <- hyperplane_side(rbind(c(1, 2), c(0, -1)), c(3, 4), -2)
  expect_equal(c(h$value, h$distance), c(9, -6, 1.8, 1.2))
  expect_equal(correlation_dist(c(1, 2, 4, 3), c(2, 1, 5, 5))$estimate, 1 - cor(c(1, 2, 4, 3), c(2, 1, 5, 5)))
})
