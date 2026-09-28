test_that("WendlandTaper equals fields::Wendland", {
  skip_if_not_installed("fields")
  d <- c(0, 0.1, 0.3, 0.5, 0.77, 0.9, 1, 1.3)
  expect_equal(WendlandTaper(d, 1.2, 2, 0), ifelse(d < 1.2, (1 - d / 1.2)^2, 0), tolerance = 1e-15)
  for (dim in 1:3) {
    for (k in 1:3) {
      expect_equal(WendlandTaper(d, 1.2, dim, k), fields::Wendland(d, theta = 1.2, dimension = dim, k = k),
                   tolerance = 1e-13)
    }
  }
})
