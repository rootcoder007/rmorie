# Rangayyan batch 1 in R, against the same book equations as the Python
# arm, and against the Python arm's values where both compute the same
# quantity.  Expected values are computed here from the printed equation.

test_that("AICorder implements eq (7.60)", {
  eps <- c(10, 4, 3.9, 3.85)
  n <- 100; n_eff <- 40
  want <- log(eps) + 2 * seq_along(eps) / n_eff
  r <- AICorder(eps, n)
  expect_equal(unname(r$criterion), want, tolerance = 1e-15)
  expect_equal(unname(r$order), which.min(want))
  expect_equal(r$n_effective, n_eff)
})

test_that("AICorder is not the textbook N log sigma^2 form", {
  eps <- c(10, 4)
  r <- AICorder(eps, 100)
  textbook <- 100 * log(eps) + 2 * seq_along(eps)
  expect_gt(abs(r$criterion[1] - textbook[1]), 1)
})

test_that("AICorder validates its inputs", {
  expect_error(AICorder(c(1, -2), 100), "positive")
  expect_error(AICorder(c(1, 2), 100, window = "nope"), "unknown window")
  expect_equal(AICorder(c(1, 2), 100, window = "rectangular")$n_effective, 100)
})

test_that("BartlettPSD locates a pure tone and averages, not sums", {
  fs <- 64; n <- 256; f0 <- 8
  x <- sin(2 * pi * f0 * (0:(n - 1)) / fs)
  b <- BartlettPSD(x, fs = fs, n_segments = 4)
  expect_equal(b$freqs[which.max(b$psd)], f0)
  expect_equal(b$n_segments, 4)
  expect_equal(b$segment_length, 64)

  # eq (6.16) is a mean: repeating a segment cannot inflate power
  seg <- c(1, -2, 3, -1, 0.5, 2, -0.5, 1.5)
  expect_equal(BartlettPSD(seg, n_segments = 1)$psd,
               BartlettPSD(rep(seg, 4), n_segments = 4)$psd,
               tolerance = 1e-12)
})

test_that("BartlettPSD validates the segmentation", {
  x <- as.numeric(0:15)
  expect_error(BartlettPSD(x), "exactly one")
  expect_error(BartlettPSD(x, n_segments = 2, segment_length = 8),
               "exactly one")
  expect_error(BartlettPSD(1), "at least two")
})

test_that("ARtoCepstrum implements eq (7.65)", {
  a <- c(0.5, -0.3, 0.2)
  h1 <- -a[1]
  h2 <- -a[2] - (1 - 1 / 2) * a[1] * h1
  h3 <- -a[3] - ((1 - 1 / 3) * a[1] * h2 + (1 - 2 / 3) * a[2] * h1)
  expect_equal(ARtoCepstrum(a)$cepstrum, c(h1, h2, h3), tolerance = 1e-15)
})

test_that("the reindexed form of eq (7.65) is the same recursion", {
  # sum (1 - k/n) a_k h(n-k) == sum (j/n) h(j) a_{n-j} under j = n - k
  a <- c(0.5, -0.3, 0.2, 0.15)
  h <- numeric(length(a) + 1)
  for (n in seq_along(a)) {
    acc <- -a[n]
    if (n > 1) {
      j <- seq_len(n - 1)
      acc <- acc - sum((j / n) * h[j + 1] * a[n - j])
    }
    h[n + 1] <- acc
  }
  expect_equal(ARtoCepstrum(a)$cepstrum, h[-1], tolerance = 1e-15)
})

test_that("ARtoCepstrum handles the gain and validates input", {
  expect_equal(ARtoCepstrum(c(0.7, 0.1))$cepstrum[1], -0.7)
  expect_equal(ARtoCepstrum(c(0.7, 0.1), gain = exp(1))$c0, 1)
  expect_null(ARtoCepstrum(c(0.7, 0.1))$c0)
  expect_error(ARtoCepstrum(c(0.7), gain = 0), "positive")
  expect_error(ARtoCepstrum(numeric(0)), "at least one")
})

test_that("pre-policy spellings still work", {
  expect_equal(morie_ar_to_cepstrum(c(0.5))$cepstrum, -0.5)
  expect_equal(morie_ar_order_aic(c(10, 4), 100)$order,
               AICorder(c(10, 4), 100)$order)
})
