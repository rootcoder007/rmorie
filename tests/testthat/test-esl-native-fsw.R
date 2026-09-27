esl16_data <- function() {
  i <- 1:30
  X <- cbind(sin(i / 7), cos(i / 5), sin(i / 3 + 1), cos(i / 11) + 0.8 * sin(i / 7))
  list(X = X, y = X[, 1] * 1.5 - X[, 2] + 0.8 * X[, 4] + 0.3 * cos(i / 2))
}

test_that("the LAR lasso modification reproduces lars(type = 'lasso') including a drop", {
  d <- esl16_data()
  a <- morie_esl_least_angle_reg(d$X, d$y, method = "lasso")
  skip_if_not_installed("lars")
  l <- lars::lars(d$X, d$y, type = "lasso", normalize = TRUE)
  expect_equal(a$coef_path, unname(coef(l)), tolerance = 1e-10)
  expect_equal(a$dropped, 1L)
})

test_that("forward stagewise takes eps steps on the most correlated variable and tends to lars", {
  d <- esl16_data()
  f <- morie_esl_forward_stagewise(d$X, d$y, eps = 0.001, max_steps = 2000)
  xs <- scale(d$X, scale = FALSE)
  xs <- sweep(xs, 2, sqrt(colSums(xs^2)), "/")
  expect_equal(unname(rowSums(abs(diff(f$path)))), rep(0.001, 2000), tolerance = 1e-12)
  r <- d$y - mean(d$y)
  for (k in 1:2000) {
    j <- which.max(abs(crossprod(xs, r)))
    expect_equal(f$chosen[k], j)
    expect_equal(f$path[k + 1, j] - f$path[k, j], 0.001 * sign(crossprod(xs[, j], r)[1]), tolerance = 1e-12)
    r <- r - (f$path[k + 1, j] - f$path[k, j]) * xs[, j]
  }
  expect_equal(morie_esl_forward_stagewise(d$X, -d$y, eps = 0.001, max_steps = 2000)$path, -f$path, tolerance = 1e-12)
  skip_if_not_installed("lars")
  l <- lars::lars(d$X, d$y, type = "forward.stagewise", normalize = TRUE)
  ref <- coef(l, s = f$l1_norm, mode = "norm")
  expect_lt(max(abs(f$path[2001, ] - ref * sqrt(colSums(scale(d$X, scale = FALSE)^2)))), 0.01)
  expect_equal(morie_esl_l1_margin(c(1, -1, 1), c(0.8, -0.5, 0.2), c(0.5, -0.3))$margin, 0.25)
})
