test_that("lilef reproduces nortest::lillie.test", {
  skip_if_not_installed("nortest")
  set.seed(5)
  for (n in c(8, 30, 150)) {
    x <- rexp(n) + rnorm(n, sd = 0.4)
    r <- lilef(x)
    ref <- nortest::lillie.test(x)
    expect_equal(r$statistic, unname(ref$statistic), tolerance = 1e-12)
    expect_equal(r$p_value, ref$p.value, tolerance = 1e-12)
  }
})
