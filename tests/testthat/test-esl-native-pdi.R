test_that("ProDenICA recovers the sources and matches the Python arm", {
  xs <- 12345
  S <- matrix(0, 400, 2)
  for (i in 1:400) {
    for (j in 1:2) {
      xs <- (xs * 16807) %% 2147483647
      S[i, j] <- xs / 2147483647
    }
  }
  S <- cbind((S[, 1] - 0.5) * sqrt(12), -log(S[, 2]) - 1)
  M <- rbind(c(1, 0.4), c(0.6, 1))
  r <- morie_esl_prodenica(S %*% M)
  R <- M %*% r$unmixing
  amari <- (sum(rowSums(abs(R)) / apply(abs(R), 1, max) - 1) + sum(colSums(abs(R)) / apply(abs(R), 2, max) - 1)) / 4
  expect_true(r$converged)
  expect_lt(amari, 0.02)
  expect_equal(crossprod(r$A), diag(2), tolerance = 1e-12)
  expect_equal(crossprod(r$sources) / 400, diag(2), tolerance = 1e-10)
  expect_equal(r$negentropy, 0.5194526450310761, tolerance = 1e-9)
})
