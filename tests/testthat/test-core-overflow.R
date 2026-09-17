test_that("the mean does not overflow where base R does not, on either kernel", {
  m <- rmorie:::morie_mean
  v <- rmorie:::morie_var
  expect_identical(m(rep(1e308, 3)), 1e308)
  expect_identical(m(rep(1e120, 3)), 1e120)
  expect_identical(v(rep(1e308, 3), 1L), 0)
  expect_identical(v(rep(1e120, 3), 1L), 0)
  expect_identical(v(rep(1e120, 3), 0L), 0)
  expect_identical(m(c(1e308, -1e308, 0)), 0)
  # non-finite inputs still propagate as in base R
  expect_identical(m(c(1, Inf)), Inf)
  expect_true(is.nan(m(c(1, NaN))))
  expect_true(is.na(m(c(1, NA))))
  set.seed(3)
  x <- stats::rnorm(1000, 1e6, 1)
  expect_identical(m(x), mean(x))
  expect_equal(v(x, 1L), stats::var(x), tolerance = 1e-14)
  # the vendored kernel, whatever bricklayer is installed
  expect_identical(rmorie:::morie_mean_cpp(rep(1e308, 3), shared = FALSE), 1e308)
  expect_identical(rmorie:::morie_var_cpp(rep(1e120, 3), 1L, shared = FALSE), 0)
  expect_identical(rmorie:::morie_mean_cpp(x, shared = FALSE), mean(x))
  # the shared kernel once bricklayer carries the fix
  if (utils::packageVersion("rmoriebricklayer") >= "0.5.1") {
    expect_identical(rmorie:::morie_mean_cpp(rep(1e308, 3), shared = TRUE), 1e308)
    expect_identical(rmorie:::morie_var_cpp(rep(1e120, 3), 1L, shared = TRUE), 0)
  }
  expect_type(rmorie:::.rmorie_shared_core(), "logical")
})
