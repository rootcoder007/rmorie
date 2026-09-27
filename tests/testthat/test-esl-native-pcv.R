test_that("principal curve decreases the distance and reproduces a line", {
  th <- (0:39) / 39 * pi
  k <- 0:39
  X <- cbind(cos(th) + 0.05 * sin(7 * k), sin(th) + 0.05 * cos(5 * k))
  r <- morie_esl_principal_curve(X, penalty = 0.05)
  expect_true(all(diff(r$distance_path[1:4]) < 0))
  expect_equal(r$distance, 0.05491164089336352, tolerance = 1e-9)
  expect_lt(r$distance, 0.06)
  L <- cbind((0:19) / 10, 2 * (0:19) / 10 + 1)
  expect_lt(morie_esl_principal_curve(L)$distance, 1e-20)
})
