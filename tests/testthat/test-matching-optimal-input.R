test_that("optimal 1-D matching refuses non-finite scores instead of aborting", {
  f <- rmorie:::.morie_match_optimal_1d_cpp
  expect_error(f(NA_real_, NA_real_), "finite")
  expect_error(f(c(1.5, NA, 3), c(1, 2, 3)), "finite")
  expect_error(f(c(1, 2), c(1, Inf, 3)), "finite")
  expect_error(f(numeric(0), 1), "n_treated")
  m <- f(c(0.2, 0.8), c(0.1, 0.5, 0.9))
  expect_length(m, 2)
  expect_true(all(m %in% 1:3))
})
