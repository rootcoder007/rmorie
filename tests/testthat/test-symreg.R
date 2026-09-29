X <- cbind((-10:10) / 4, cos((-10:10) / 3))
Y <- 2.5 * X[, 1]^2 - X[, 2] + 0.3

test_that("PysrRegression front is Pareto with PySR scores", {
  r <- PysrRegression(X, Y, niterations = 15, population_size = 40, seed = 3)
  e <- r$equations
  expect_true(all(diff(e$complexity) > 0) && all(diff(e$loss) < 0))
  k <- 2:nrow(e)
  expect_equal(e$score[k], -(log(pmax(e$loss[k], 1e-300)) - log(pmax(e$loss[k - 1], 1e-300))) / diff(e$complexity), tolerance = 1e-12)
  expect_equal(r$best$loss, mean((r$prediction - Y)^2), tolerance = 1e-12)
})

test_that("PysrRegression recovers a quadratic exactly", {
  x <- matrix((-8:8) / 4)
  r <- PysrRegression(x, 2 * x[, 1]^2 + x[, 1], niterations = 40, seed = 3)
  expect_lt(r$best$loss, 1e-20)
  expect_error(PysrRegression(X, Y, unary_operators = "tanh"))
})
