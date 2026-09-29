test_that("E-values follow VanderWeele and Ding (2017)", {
  expect_equal(Evalu(2)$evalue, 2 + sqrt(2))
  expect_equal(Evalu(1)$evalue, 1)
  expect_equal(Evalu(0.4)$evalue, Evalu(2.5)$evalue)
  expect_equal(Evalu(3, ci_lower = 1.5, ci_upper = 6)$evalue_ci, 1.5 + sqrt(0.75))
  expect_equal(Evalu(1.8, ci_lower = 0.9, ci_upper = 3.6)$evalue_ci, 1)
  expect_equal(Evaltw(0.5, 0.3, 0.8)$evalue_ci, 1.25 + sqrt(1.25 * 0.25))
  expect_error(Evalu(-1), "positive")
})

test_that("Jntmed reproduces the two lm() slope t-tests", {
  x <- 0:7
  m <- c(0.3, 1.1, 1.6, 3.4, 3.9, 5.2, 6.4, 6.8)
  y <- c(1, 1.9, 3.1, 4.4, 4.8, 6.9, 7.2, 8.1)
  r <- Jntmed(x, m, y)
  ca <- summary(lm(m ~ x))$coefficients
  cb <- summary(lm(y ~ x + m))$coefficients
  expect_equal(r$a, ca["x", 1], tolerance = 1e-12)
  expect_equal(r$b, cb["m", 1], tolerance = 1e-12)
  expect_equal(r$p_a, ca["x", 4], tolerance = 1e-10)
  expect_equal(r$p_b, cb["m", 4], tolerance = 1e-10)
  expect_equal(r$p_value, max(ca["x", 4], cb["m", 4]), tolerance = 1e-10)
})
