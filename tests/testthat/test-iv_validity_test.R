test_that("Kitagawa statistic recomputes and rejects an invalid instrument", {
  z <- rep(0:1, 30)
  d <- as.numeric(sin(2.7 * (0:59)) + 0.8 * z > 0.3)
  y <- round(cos(1.3 * (0:59)) + d, 1)
  r <- bound_monotone_test(y, d, z, n_boot = 49)
  n <- 60
  Tn <- 30 * 30 / 60
  # the take-up constraint alone: P(D = 0 | Z = 1) - P(D = 0 | Z = 0)
  q1 <- mean(d[z == 1] == 0) / 2
  q0 <- mean(d[z == 0] == 0) / 2
  v <- Tn / n * (q1 / 0.25 - q1^2 / 0.125 + q0 / 0.25 - q0^2 / 0.125)
  expect_gte(r$statistic, sqrt(Tn) * (2 * q1 - 2 * q0) / max(0.07, sqrt(v)) - 1e-12)
  expect_true(r$p_value >= 0 && r$p_value <= 1)
  z2 <- rep(0:1, 100)
  d2 <- as.numeric(sin(2.7 * (0:199)) + 0.8 * z2 > 0.3)
  y2 <- round(2 * (cos(1.3 * (0:199)) + d2)) / 2
  expect_lt(bound_monotone_test(y2, 1 - d2, z2, n_boot = 49)$p_value, 0.05)
  expect_gt(bound_monotone_test(y2, d2, z2, n_boot = 49)$p_value, 0.5)
})
