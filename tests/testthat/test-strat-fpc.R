test_that("strat applies the finite population correction and matches Python", {
  df <- data.frame(y = c(1, 2, 3, 10, 11, 12, 14), stratum = c("a", "a", "a", "b", "b", "b", "b"))
  r <- strat(df, "y", "stratum", pop_sizes = c(a = 30, b = 10))
  v <- 0.75^2 * (1 - 3 / 30) * var(c(1, 2, 3)) / 3 + 0.25^2 * (1 - 4 / 10) * var(c(10, 11, 12, 14)) / 4
  expect_equal(r$se, sqrt(v), tolerance = 1e-12)
  expect_equal(r$estimate, 0.75 * 2 + 0.25 * 47 / 4, tolerance = 1e-12)
  # Python doctest of morie.fn.strat.stratified_mean
  expect_equal(round(r$se, 12), 0.442824739598)
})
