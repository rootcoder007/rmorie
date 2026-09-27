test_that("nlsgn solves the book's quadratic exactly", {
  r <- nlsgn(function(x, t) t[1] + t[2] * x + t[3] * x^2, 1:5, c(4, 1, 3, 5, 6), c(1, 1, 1))
  expect_true(r$converged)
  expect_equal(r$coefficients, c(189 / 35, -92 / 35, 4 / 7), tolerance = 1e-9)
  expect_equal(r$rss, 134 / 35, tolerance = 1e-12)
  x <- 1:5
  y <- c(4, 1, 3, 5, 6)
  expect_equal(r$se, unname(summary(lm(y ~ x + I(x^2)))$coefficients[, 2]), tolerance = 1e-7)
})

test_that("nlsgn reaches the nls optimum of a * b^x", {
  x <- 1:5
  y <- c(3, 7, 12, 26, 51)
  r <- nlsgn(function(x, t) t[1] * t[2]^x, x, y, c(1, 1))
  m <- nls(y ~ a * b^x, start = list(a = 1.5, b = 2), control = nls.control(tol = 1e-9))
  expect_equal(r$coefficients, unname(coef(m)), tolerance = 1e-6)
  expect_lte(r$rss, deviance(m) * (1 + 1e-13))
  expect_equal(r$se, unname(summary(m)$coefficients[, 2]), tolerance = 1e-5)
})

test_that("nlsgn halves steps from a poor start", {
  r <- nlsgn(function(x, t) t[1] * t[2]^x, 1:5, c(3, 7, 12, 26, 51), c(1, 0.5))
  expect_true(r$converged)
  expect_lte(r$rss, 1.225082272330410893 * (1 + 1e-13))
})
