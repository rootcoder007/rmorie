test_that("reassigned_spectrogram applies the Auger-Flandrin operators", {
  x <- sin(0.7 * (0:39)) + 0.3 * cos(2.1 * (0:39) + 0.4)
  fs <- 8
  L <- 16
  hop <- 6
  r <- reassigned_spectrogram(x, fs = fs, window = L, hop = hop)
  j <- 3
  m <- 3
  k <- 0:(L - 1)
  seg <- x[(j - 1) * hop + seq_len(L)]
  h <- 0.5 - 0.5 * cos(2 * pi * k / L)
  E <- exp(-2i * pi * m * k / L)
  Xh <- sum(seg * h * E)
  Xt <- sum(seg * (k - L / 2) / fs * h * E)
  Xd <- sum(seg * pi * fs / L * sin(2 * pi * k / L) * E)
  tc <- ((j - 1) * hop + L / 2) / fs
  expect_equal(r$magnitude[m + 1, j], Mod(Xh), tolerance = 1e-12)
  expect_equal(r$t_reassigned[m + 1, j], tc + Re(Xt / Xh), tolerance = 1e-12)
  expect_equal(r$f_reassigned[m + 1, j], m * fs / L - Im(Xd / Xh) / (2 * pi), tolerance = 1e-12)
  tone <- exp(2i * pi * 0.19 * (0:95))
  rt <- reassigned_spectrogram(tone, fs = 1, window = 32, hop = 16)
  expect_lt(max(abs(rt$f_reassigned[rt$magnitude > 1] - 0.19)), 1e-4)
})
