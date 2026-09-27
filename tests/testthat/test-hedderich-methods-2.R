test_that("gtest reproduces the book's likelihood-ratio examples and DescTools::GTest", {
  expect_equal(lrgtst(c(4, 56), c(1 / 6, 5 / 6))$statistic, 5.3624868993911265, tolerance = 1e-13)
  r <- lrgtst(c(18, 55, 27), c(0.207025, 0.49595, 0.297025), n_estimated = 1)
  expect_equal(c(r$statistic, r$df), c(1.1916754720499076, 1), tolerance = 1e-13)
  r <- lrgtst(matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE))
  expect_equal(c(r$statistic, r$p_value), c(23.595462988817047, 9.6259434753331874e-05), tolerance = 1e-12)
})

test_that("catrnd splits chi-square as prop.trend.test and prop.test", {
  r <- catrnd(c(22, 16, 2), c(36, 34, 10), c(1, 0, -1))
  expect_equal(r$chi2_trend, 5.2197070572569864, tolerance = 1e-13)
  expect_equal(r$chi2_total, 5.4954248366013072, tolerance = 1e-13)
  expect_equal(r$z_unpooled, 0.411111111111111 / sqrt(0.611111111111111 * 0.388888888888889 / 36 +
                                                         0.2 * 0.8 / 10), tolerance = 1e-12)
})

test_that("ntrtau follows eq (7.415)", {
  expect_equal(ntrtau(0.3)$n_exact, 38.759899922711519, tolerance = 1e-13)
  expect_equal(ntrtau(0.5, 0.01, 0.9)$n_exact, 26.452243856441832, tolerance = 1e-13)
})

test_that("gamfit matches the book's moments and MASS::fitdistr", {
  tm <- c(274.0, 1.7, 871.0, 1311.0, 236.0, 458.0, 54.9, 1787.0, 0.75, 776.0, 28.5, 20.8, 363.0,
          1661.0, 828.0, 290.0, 175.0, 970.0, 1278.0, 126.0)
  r <- gamfit(tm, "moments")
  expect_equal(c(r$shape, r$rate), c(1.0559039471895129, 0.0018346556401063587), tolerance = 1e-13)
  r <- gamfit(tm)
  expect_equal(c(r$shape, r$rate), c(0.57918183671663281, 0.0010063408004181045), tolerance = 1e-12)
})

test_that("pchnrm equals nortest::pearson.test", {
  x <- stats::qnorm(stats::ppoints(40)) * 2 + 5 + 0.3 * sin(1:40)
  r <- pchnrm(x)
  expect_equal(c(r$statistic, r$p_value, r$n_classes), c(2.3, 0.89014511791686335, 9), tolerance = 1e-12)
  r <- pchnrm(x, n_classes = 8)
  expect_equal(c(r$statistic, r$p_value), c(1.2, 0.94487736500212194), tolerance = 1e-12)
})
