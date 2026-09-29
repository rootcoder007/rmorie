test_that("PowerTTest reproduces stats::power.t.test", {
  for (st in c(TRUE, FALSE)) {
    expect_equal(PowerTTest(n = 12, delta = 1.1, strict = st), power.t.test(n = 12, delta = 1.1, strict = st)$power,
                 tolerance = 1e-12)
  }
  n <- PowerTTest(delta = 0.8, power = 0.9, type = "one-sample")
  expect_equal(PowerTTest(n = n, delta = 0.8, type = "one-sample"), 0.9, tolerance = 1e-10)
  expect_equal(n, power.t.test(delta = 0.8, power = 0.9, type = "one.sample", strict = TRUE, tol = 1e-12)$n,
               tolerance = 1e-8)
})

test_that("PowerPropTest reproduces power.prop.test and Cohen's h", {
  expect_equal(PowerPropTest(n = 80, p1 = 0.4, p2 = 0.6), power.prop.test(80, 0.4, 0.6, strict = TRUE)$power,
               tolerance = 1e-12)
  h <- abs(2 * asin(sqrt(0.4)) - 2 * asin(sqrt(0.6)))
  z <- qnorm(0.975)
  expect_equal(PowerPropTest(n = 80, p1 = 0.4, p2 = 0.6, method = "cohen_h"),
               pnorm(z - h * sqrt(40), lower.tail = FALSE) + pnorm(-z - h * sqrt(40)), tolerance = 1e-12)
})

test_that("PowerAnova and CalculateInteractionPower use ncp = f^2 N", {
  n <- 10
  k <- 3
  f <- 0.4
  expect_equal(PowerAnova(n = n, k = k, f = f),
               power.anova.test(groups = k, n = n, between.var = k * f^2 / (k - 1), within.var = 1)$power,
               tolerance = 1e-12)
  nn <- PowerAnova(k = 4, f = 0.25, power = 0.8)
  expect_equal(PowerAnova(n = nn, k = 4, f = 0.25), 0.8, tolerance = 1e-10)
  expect_equal(CalculateInteractionPower(200), PowerAnova(n = 100, k = 2, f = 0.2), tolerance = 1e-12)
})
