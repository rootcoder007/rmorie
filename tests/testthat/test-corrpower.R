test_that("correlation power recomputes", {
  pw <- function(n, r, a) {
    t <- qt(1 - a / 2, n - 2)
    rc <- t / sqrt(t^2 + n - 2)
    zr <- atanh(r) + r / (2 * (n - 1))
    pnorm((zr - atanh(rc)) * sqrt(n - 3)) + pnorm((-zr - atanh(rc)) * sqrt(n - 3))
  }
  expect_equal(PwrRTest(n = 100, r = 0.3)$power, pw(100, 0.3, 0.05), tolerance = 1e-12)
  n <- PwrRTest(r = 0.3, power = 0.8)$n
  expect_equal(pw(n, 0.3, 0.05), 0.8, tolerance = 1e-9)
  expect_identical(CohenRMagnitude(-0.3), "medium")
})
