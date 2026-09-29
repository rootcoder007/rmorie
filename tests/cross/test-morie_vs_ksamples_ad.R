.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("kSamples")

test_that("Ksamp equals kSamples::ad.test (version 2, midranks)", {
  a <- c(0.1, 1.2, 0.5, 2.2, 1.9, 0.7, 1.2)
  b <- c(3.1, 2.4, 4.2, 3.3, 2.9, 1.2)
  cc <- c(1.5, 0.9, 2.6, 2, 1.1)
  t <- kSamples::ad.test(a, b, cc, method = "asymptotic")
  r <- Ksamp(a, b, cc)
  # kSamples reports its table rounded to four decimals
  expect_lt(abs(r$statistic - unname(t$ad[2, 2])), 5e-5)
})
