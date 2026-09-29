test_that("apre_statistic pools errors over roll calls", {
  m <- c(40, 10, 25, 3)
  e <- c(8, 5, 10, 3)
  pre <- (m - e) / m
  r <- apre_statistic(pre, minority = m)
  expect_equal(r$value, sum(m - e) / sum(m), tolerance = 1e-14)
  expect_equal(r$mean_pre, mean(pre), tolerance = 1e-14)
  expect_equal(r$median_pre, median(pre), tolerance = 1e-14)
  expect_false(apre_statistic(pre)$weighted)
  expect_error(apre_statistic(pre, minority = c(1, 2)), "one entry")
})
