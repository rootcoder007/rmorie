test_that("Yule's Q and Y equal DescTools", {
  skip_if_not_installed("DescTools")
  for (tb in list(matrix(c(23, 11, 7, 19), 2), matrix(c(5, 40, 17, 3), 2), matrix(c(102, 87, 95, 110), 2))) {
    r <- YuleAssociation(tb)
    expect_equal(r$Q, DescTools::YuleQ(tb), tolerance = 1e-14)
    expect_equal(r$Y, DescTools::YuleY(tb), tolerance = 1e-14)
  }
})
