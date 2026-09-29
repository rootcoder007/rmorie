skip_if_not_installed("signal")

test_that("group_delay matches signal::grpdelay", {
  b <- c(0.2, 0.5, 0.3)
  a <- c(1, -0.4, 0.1)
  ref <- suppressWarnings(signal::grpdelay(b, a, n = 64))
  expect_equal(group_delay(b, a, worN = 64)$value, as.numeric(ref$gd), tolerance = 1e-10)
})
