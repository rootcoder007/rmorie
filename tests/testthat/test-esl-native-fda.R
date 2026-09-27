test_that("FDA with the linear basis equals mda::fda(method = polyreg)", {
  i <- 1:60
  X <- cbind(sin(i) + 0.2 * (i %% 3), cos(3 * i) + 0.4 * (i %% 3), sin(2 * i))
  g <- i %% 3
  r <- morie_esl_fda(X, g, rbind(c(0.2, 0.5, 0.1), c(1.2, 1.0, -0.3)))
  expect_equal(r$eigenvalues, c(0.264095178837929, 0.00602149780425885), tolerance = 1e-12)
  expect_equal(r$prediction, c(1, 2))
  skip_if_not_installed("mda")
  d <- data.frame(x1 = X[, 1], x2 = X[, 2], x3 = X[, 3], g = factor(g))
  f <- mda::fda(g ~ ., data = d, method = mda::polyreg)
  expect_equal(morie_esl_fda(X, g)$prediction, as.numeric(as.character(predict(f))))
  v <- predict(f, d[1:3, 1:3], type = "variates")
  expect_equal(abs(morie_esl_fda(X, g, X[1:3, ])$variates / sqrt(r$eigenvalues * (1 - r$eigenvalues))[col(v)]),
               abs(unclass(v)), tolerance = 1e-10, ignore_attr = TRUE)
})
