test_that("supervised principal components follow Algorithm 18.1", {
  i <- 1:40
  X <- cbind(sin(i), cos(i), sin(2 * i), cos(3 * i), 0.5 * sin(i) + 0.5 * cos(i), sin(5 * i))
  y <- X[, 1] + X[, 2] + 0.3 * cos(7 * i)
  r <- morie_esl_supervised_pc(X, y, 1.5, newdata = rbind(c(0.2, 0.5, 0.1, -0.3, 0.35, 0.2)))
  expect_equal(r$selected, c(1L, 2L, 5L))
  pc <- prcomp(X[, r$selected])
  f <- lm(y ~ pc$x[, 1])
  expect_equal(r$fitted, unname(fitted(f)), tolerance = 1e-12)
  expect_equal(r$prediction, 0.66901672530837319, tolerance = 1e-12)
  Xf <- X[, -5]
  full <- morie_esl_supervised_pc(Xf, y, 0, m = 5)
  expect_equal(full$fitted, unname(fitted(lm(y ~ Xf))), tolerance = 1e-10)
})
