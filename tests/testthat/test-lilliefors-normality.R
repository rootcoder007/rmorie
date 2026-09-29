test_that("lilef is the Lilliefors D with the Dallal-Wilkinson p-value", {
  x <- c(2.1, 3.4, 1.9, 5.6, 2.8, 3.1, 9.9, 2.5, 3.3, 2.7)
  n <- length(x)
  z <- sort((x - mean(x)) / sd(x))
  f <- pnorm(z)
  d <- max(c(seq_len(n) / n - f, f - (seq_len(n) - 1) / n))
  r <- lilef(x)
  expect_equal(r$statistic, d, tolerance = 1e-13)
  p <- exp(-7.01256 * d^2 * (n + 2.78019) + 2.99587 * d * sqrt(n + 2.78019) -
    0.122119 + 0.974598 / sqrt(n) + 1.67997 / n)
  expect_equal(r$p_value, p, tolerance = 1e-13)
  expect_identical(r$interpretation, "reject")
  y <- c(-0.61, 0.23, 1.02, -1.4, 0.35, -0.12, 0.77, -0.95, 0.51, 1.6, -0.3, 0.05)
  ry <- lilef(y)
  kk <- (sqrt(12) - 0.01 + 0.85 / sqrt(12)) * ry$statistic
  stephens <- if (kk <= 0.302) 1 else if (kk <= 0.5) {
    2.76773 - 19.828315 * kk + 80.709644 * kk^2 - 138.55152 * kk^3 + 81.218052 * kk^4
  } else if (kk <= 0.9) {
    -4.901232 + 40.662806 * kk - 97.490286 * kk^2 + 94.029866 * kk^3 - 32.355711 * kk^4
  } else {
    6.198765 - 19.558097 * kk + 23.186922 * kk^2 - 12.234627 * kk^3 + 2.423045 * kk^4
  }
  expect_equal(ry$p_value, stephens, tolerance = 1e-13)
  expect_equal(lilef(rbind(x, x + 1), axis = 0)$statistic, d, tolerance = 1e-13)
  expect_error(lilef(c(1, 2, 3)), "at least 4")
})
