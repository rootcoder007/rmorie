d <- .spdiag_data()

test_that("Slxwx, Slxflt and Slxres", {
  w <- Slxwx(d$X, d$W)
  expect_equal(w$WX[, 1], as.vector(d$W %*% d$X[, 2]), tolerance = 1e-14)
  expect_equal(w$lagged_columns, 1L)
  expect_equal(Slxflt(d$X, d$W)$filtered, unname(d$X - d$W %*% d$X), tolerance = 1e-14)
  expect_equal(Slxres(d$e, d$W)$statistic, .spdiag_moran(d$e, d$W), tolerance = 1e-13)
})

test_that("Slxboot resamples OLS residuals of the SLX design", {
  Z <- cbind(d$X, d$W %*% d$X[, 2])
  g <- lm.fit(Z, d$y)
  fit <- d$y - g$residuals
  e <- g$residuals - mean(g$residuals)
  B <- 30
  u <- .morie_random_uniform(B * 8, seed = 4)
  th <- vapply(1:B, function(b) lm.fit(Z, fit + e[floor(u[(b - 1) * 8 + 1:8] * 8) + 1])$coefficients[3], numeric(1))
  r <- Slxboot(d$y, d$X, d$W, B = B, seed = 4)
  expect_equal(r$statistic, unname(g$coefficients[3]), tolerance = 1e-11)
  expect_equal(c(r$ci_lower, r$ci_upper), unname(quantile(th, c(0.025, 0.975))), tolerance = 1e-10)
})
