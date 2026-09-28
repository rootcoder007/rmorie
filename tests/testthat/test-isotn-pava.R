test_that("isotn is weighted PAVA and equals stats::isoreg for unit weights", {
  y <- c(3, 1, 2, 5, 4, 4.5, 2, 6)
  expect_equal(isotn(1:8, y)$fitted, c(2, 2, 2, 3.875, 3.875, 3.875, 3.875, 6))
  expect_equal(isotn(1:8, y)$fitted, stats::isoreg(y)$yf)
  expect_equal(isotn(1:8, y, increasing = FALSE)$fitted, -stats::isoreg(-y)$yf)
  expect_equal(isotn(1:3, c(3, 1, 5), weights = c(1, 3, 2))$fitted, c(1.5, 1.5, 5))
})
