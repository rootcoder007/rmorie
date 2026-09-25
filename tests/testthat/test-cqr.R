# Conformalized quantile regression (Romano, Patterson & Candes 2019).

test_that("qhat is the ceiling((n+1)(1-alpha))-th conformity score", {
  lo <- 0:9
  hi <- lo + 2
  y <- c(0.5, 3.4, 1.2, 4.8, 4.1, 8.0, 5.5, 8.2, 11.3, 10.0)
  s <- sort(pmax(lo - y, y - hi))
  k <- ceiling(11 * 0.8)
  r <- morie_cqr(lo, hi, y, 1, 3, alpha = 0.2)
  expect_identical(r$k, as.integer(k))
  expect_identical(r$qhat, s[k])
  expect_identical(c(r$lower, r$upper), c(1 - s[k], 3 + s[k]))
})

test_that("too few calibration points give an unbounded interval", {
  r <- morie_cqr(c(0, 1), c(1, 2), c(0.5, 1.5), 0, 1, alpha = 0.1)
  expect_identical(r$qhat, Inf)
  expect_identical(c(r$lower, r$upper), c(-Inf, Inf))
})
