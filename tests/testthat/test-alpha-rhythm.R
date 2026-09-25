# Alpha rhythm by the ACF: the fundamental period decides, so the
# multiples of a faster rhythm's period are not read as alpha.

test_that("a 10 Hz rhythm at 100 Hz is alpha with its peak at lag 10", {
  x <- sin(2 * pi * 10 * (0:399) / 100)
  r <- AlphaRhy(x, 100)
  expect_true(r$present)
  expect_identical(r$peak_lag, 10L)
  expect_identical(r$lag_range, c(7L, 13L))
})

test_that("a 25 Hz rhythm is not alpha, but is found with the beta band", {
  x <- sin(2 * pi * 25 * (0:399) / 100)
  expect_false(AlphaRhy(x, 100)$present)
  expect_equal(AlphaRhy(x, 100)$frequency_hz, 25)
  expect_true(AlphaRhy(x, 100, band = c(20, 30))$present)
  mixed <- sin(2 * pi * 10 * (0:399) / 100) + 0.2 * sin(2 * pi * 25 * (0:399) / 100)
  expect_identical(AlphaRhy(mixed, 100)$peak_lag, 10L)
  expect_error(AlphaRhy(x, 20), "Nyquist")
})
