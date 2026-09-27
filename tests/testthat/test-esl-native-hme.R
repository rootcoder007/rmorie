test_that("HME EM is monotone and matches the Python arm", {
  i <- 1:60
  x <- ((7 * i) %% 59) / 59 * 4 - 2
  y <- ifelse(x < 0, 2 * x, -x + 0.5) + 0.1 * cos(9 * i)
  r <- morie_esl_hme(cbind(x), y, K = 2)
  expect_true(r$converged)
  expect_true(all(diff(r$loglik_path) >= -1e-8))
  expect_equal(r$loglik, 77.1569768184434, tolerance = 1e-8)
  yb <- as.integer(sin(3 * x) + 0.3 * cos(9 * i) > 0)
  cc <- morie_esl_hme(cbind(x), yb, K = 2, task = "classification", max_iter = 60)
  expect_true(all(diff(cc$loglik_path) >= -1e-8))
})
