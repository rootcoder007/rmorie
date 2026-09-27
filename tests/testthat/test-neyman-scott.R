test_that("NeymanScottProcess matches spatstat formulas and the Python arm", {
  r <- NeymanScottProcess(10, 5, 0.05, r = c(0.02, 0.05, 0.1, 0.2))
  expect_lt(max(abs(r$K - c(0.005177693146204, 0.029973903326834, 0.094627982418754, 0.223832142254718))), 1e-12)
  m <- NeymanScottProcess(10, 5, 0.05, kernel = "matern", r = c(0.02, 0.05, 0.1, 0.2))
  expect_lt(max(abs(m$pcf - c(10.511864337358, 5.978394872537, 1, 1))), 1e-11)
  s <- NeymanScottProcess(20, 4, 0.04, kernel = "cauchy", window = c(0, 1, 0, 1), simulate = 3, seed = 1)
  expect_identical(vapply(s$simulated, nrow, 0L), c(94L, 107L, 109L))
  expect_lt(max(abs(s$simulated[[1]][1, ] - c(0.773795385777, 0.519382165482))), 1e-11)
})
