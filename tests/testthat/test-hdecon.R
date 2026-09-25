# Homomorphic deconvolution: the smooth part and the excitation split the
# complex cepstrum exactly, so they reconvolve (circularly) to the signal.

test_that("hdecon components reconvolve to the input", {
  n <- 64
  t <- 0:(n - 1)
  x <- exp(-0.2 * t) * cos(0.9 * t) + ifelse(t %% 16 == 3, 0.6, 0)
  r <- hdecon(x, cutoff = 10)
  h <- r$filtered
  e <- r$extra$excitation
  rebuilt <- vapply(0:(n - 1), function(i) sum(h * e[((i - 0:(n - 1)) %% n) + 1]), numeric(1))
  expect_lt(max(abs(rebuilt - x)), 1e-9)
})
