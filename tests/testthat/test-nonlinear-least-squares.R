test_that("nlsgn solves the book's quadratic exactly", {
  r <- nlsgn(function(x, t) t[1] + t[2] * x + t[3] * x^2, 1:5, c(4, 1, 3, 5, 6), c(1, 1, 1))
  expect_true(r$converged)
  expect_equal(r$coefficients, c(189 / 35, -92 / 35, 4 / 7), tolerance = 1e-9)
  expect_equal(r$rss, 134 / 35, tolerance = 1e-12)
  x <- 1:5
  y <- c(4, 1, 3, 5, 6)
  expect_equal(r$se, unname(summary(lm(y ~ x + I(x^2)))$coefficients[, 2]), tolerance = 1e-7)
})

test_that("nlsgn reaches the least-squares optimum of a * b^x", {
  x <- 1:5
  y <- c(3, 7, 12, 26, 51)
  r <- nlsgn(function(x, t) t[1] * t[2]^x, x, y, c(1, 1))
  a <- r$coefficients[1]
  b <- r$coefficients[2]
  e <- y - a * b^x
  J <- cbind(b^x, a * x * b^(x - 1))
  # first-order conditions: the RSS gradient -2 J'e vanishes at the optimum
  expect_lt(max(abs(crossprod(J, e))), 1e-8)
  rss <- sum(e^2)
  for (d in list(c(1e-4, 0), c(-1e-4, 0), c(0, 1e-5), c(0, -1e-5))) {
    expect_gt(sum((y - (a + d[1]) * (b + d[2])^x)^2), rss)
  }
  expect_equal(r$rss, rss, tolerance = 1e-12)
  expect_equal(r$se, sqrt(diag(rss / 3 * solve(crossprod(J)))), tolerance = 1e-6)
})

test_that("nlsgn halves steps from a poor start", {
  r <- nlsgn(function(x, t) t[1] * t[2]^x, 1:5, c(3, 7, 12, 26, 51), c(1, 0.5))
  expect_true(r$converged)
  expect_lte(r$rss, 1.225082272330410893 * (1 + 1e-13))
})
