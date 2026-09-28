test_that("AceIndex and BudykoOlr", {
  expect_equal(AceIndex(c(30, 40, 65, 90, 70, 34, 120.5)), 1e-4 * sum(c(40, 65, 90, 70, 120.5)^2), tolerance = 1e-12)
  r <- BudykoOlr(12.5, albedo = 0.31)
  expect_equal(r$equilibrium_temperature, (0.69 * 1361 / 4 - 203.3) / 2.09, tolerance = 1e-12)
})

test_that("PrewhitenedMannKendall pw recomputes", {
  x <- c(1.0, 2.1, 2.9, 4.2, 5.1, 5.8, 7.2, 8.1, 7.9, 9.4)
  r <- PrewhitenedMannKendall(x, "pw")
  d <- x - mean(x)
  r1 <- sum(d[-10] * d[-1]) / sum(d^2)
  y <- x[-1] - r1 * x[-10]
  S <- 0
  for (i in 1:8) for (j in (i + 1):9) S <- S + sign(y[j] - y[i])
  expect_equal(r$r1, r1, tolerance = 1e-12)
  expect_equal(r$S, S)
  expect_error(PrewhitenedMannKendall(x, "bad"))
})

test_that("FleissKappa and breakdown", {
  C <- rbind(c(3, 1, 0), c(0, 2, 2), c(4, 0, 0), c(1, 1, 2), c(0, 0, 4), c(2, 2, 0))
  P <- (rowSums(C^2) - 4) / 12
  pj <- colSums(C) / 24
  expect_equal(FleissKappa(C)$kappa, (mean(P) - sum(pj^2)) / (1 - sum(pj^2)), tolerance = 1e-12)
  x <- as.numeric(1:11)
  expect_equal(EmpiricalBreakdownPoint(mean, x)$m, 1)
  expect_equal(EmpiricalBreakdownPoint(median, x)$m, 6)
})
