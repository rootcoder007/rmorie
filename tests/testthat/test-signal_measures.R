test_that("group delay, noise and LMS recompute", {
  expect_equal(group_delay(c(1, 3, 3, 1), 1, worN = 8)$value, rep(1.5, 8), tolerance = 1e-12)
  b <- c(0.2, 0.5, 0.3)
  a <- c(1, -0.4, 0.1)
  ph <- function(w) Arg(sum(b * exp(-1i * w * 0:2)) / sum(a * exp(-1i * w * 0:2)))
  g <- group_delay(b, a, worN = 32)
  w <- g$frequencies[11]
  expect_equal(g$value[11], -(ph(w + 1e-6) - ph(w - 1e-6)) / 2e-6, tolerance = 1e-6)
  x <- sin(0.1 * (0:199)) + ifelse((0:199) %% 2 == 1, 0.3, -0.3)
  expect_equal(noise_power(x)$value, sum(diff(x)^2) / 398)
  expect_equal(noise_psd(x, fs = 100)$value, mean((x - mean(x))^2) / 100)
  expect_equal(max_step_size(c(1, -2, 2, -1), order = 4)$value, 0.2)
  expect_equal(qrs_duration(c(100, 460, 830), c(125, 482, 858), fs = 250)$value, 0.1)
  expect_equal(zero_crossing_rate(c(1, -1, -2, 3, 0))$value, 0.5)
})

test_that("spectrogram recomputes", {
  n <- 0:95
  x <- sin(2 * pi * 0.125 * n) + 0.5 * cos(2 * pi * 0.3125 * n)
  r <- spcgm(x, fs = 2, nperseg = 32, noverlap = 16, window = "boxcar")
  s <- x[1:32] - mean(x[1:32])
  expect_equal(sum(r$value[, 1]) * 2 / 32, mean(s^2), tolerance = 1e-12)
  expect_equal(r$times[1:2], c(8, 16))
  z <- spcgm(x, nperseg = 32, noverlap = 0, nfft = 64)
  expect_equal(which.max(z$value[, 2]) - 1, 8)
})
