test_that("kbwrt is Silverman's rule 3.31 with canonical kernel rescaling", {
  x <- c(3.1, -0.4, 2.2, 5.9, 1.7, 0.3, 4.4, 2.8, -1.2, 3.6, 7.5, 1.1)
  n <- length(x)
  s <- sd(x)
  q <- quantile(x, c(0.25, 0.75), names = FALSE, type = 7)
  h <- 0.9 * min(s, (q[2] - q[1]) / 1.34) * n^(-0.2)
  expect_equal(kbwrt(x)$bw, h, tolerance = 1e-13)
  r_ep <- 15^0.2 / (1 / (2 * sqrt(pi)))^0.2
  expect_equal(kbwrt(x, kernel = "epanechnikov")$bw, h * r_ep, tolerance = 1e-13)
  expect_equal(kbwrt(x, kernel = "biweight")$ratio, (35 / (1 / (2 * sqrt(pi))))^0.2, tolerance = 1e-13)
  expect_error(kbwrt(x, kernel = "cosine"), "Unknown kernel")
})

test_that("bwrot is the 1.06 robust rule or Scott's 1.059 rule", {
  x <- c(3.1, -0.4, 2.2, 5.9, 1.7, 0.3, 4.4, 2.8, -1.2, 3.6, 7.5, 1.1)
  n <- length(x)
  q <- quantile(x, c(0.25, 0.75), names = FALSE)
  expect_equal(bwrot(x)$bandwidth, 1.06 * min(sd(x), diff(q) / 1.34) * n^(-0.2), tolerance = 1e-13)
  expect_equal(bwrot(x, method = "scott")$bandwidth, 1.059 * sd(x) * n^(-0.2), tolerance = 1e-13)
  expect_equal(bwrot(x, kernel = "uniform")$kernel_ratio, (4.5 / (1 / (2 * sqrt(pi))))^0.2, tolerance = 1e-13)
  expect_error(bwrot(x, method = "plug-in"), "Unknown method")
})
