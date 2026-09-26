test_that("rubins_rules matches mice::pool.scalar", {
  Q <- c(1.2, 1.5, 1.1, 1.4, 1.3)
  U <- c(0.04, 0.05, 0.045, 0.05, 0.042)
  r <- rubins_rules(Q, U)
  expect_equal(r$total_variance, 0.075399999999999981, tolerance = 1e-14)
  expect_equal(r$df, 25.267377777777789, tolerance = 1e-12)
  expect_equal(r$fmi, 0.44047988516312442, tolerance = 1e-12)
  expect_equal(r$relative_efficiency, 1 / (1 + r$fmi / 5), tolerance = 1e-15)
  r2 <- rubins_rules(Q, U, dfcom = 28)
  expect_equal(r2$df, 9.7104933353729557, tolerance = 1e-12)
  expect_equal(r2$fmi, 0.49262206679320941, tolerance = 1e-12)
  expect_error(rubins_rules(1, 1), "at least 2")
})

test_that("littles_mcar_test matches Little (1988) on the norm EM estimates", {
  # norm::em.norm(criterion = 1e-14) mean and covariance plugged into eq. 2
  X <- data.frame(
    a = c(5.1, 6.3, NA, 7.2, 5.9, 6.6, 5.4, NA, 5.5, 6.8, 4.9, 6.1, 5.7, NA, 6.4, 5.2, 6.9, 5.8, 6.2, 5.6),
    b = c(4.2, NA, 3.9, 5.8, 4.4, NA, 5.3, 4.1, 4.6, 5.2, 3.8, 4.9, NA, 4.5, 5.1, 4.0, 5.6, 4.7, NA, 4.3),
    c = c(6.1, 5.2, 6.9, NA, 6.0, 5.8, 7.1, 6.6, NA, 6.4, 5.5, 6.3, 6.0, 6.7, NA, 5.4, 7.0, 6.2, 6.5, 5.9))
  r <- littles_mcar_test(X)
  expect_equal(r$test_statistic, 17.308859080868178, tolerance = 1e-9)
  expect_equal(r$df, 6)
  expect_equal(r$p_value, 0.0082127166597023536, tolerance = 1e-8)
  em <- rmorie:::.em_mvn(as.matrix(X))
  expect_equal(unname(em$mu), c(5.7165933181158941, 4.6487487910217835, 6.2617858306938929), tolerance = 1e-10)
  expect_equal(em$sigma[1, 2], 0.37893374323314255, tolerance = 1e-9)
})
