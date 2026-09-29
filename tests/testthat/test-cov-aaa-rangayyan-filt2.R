# Coverage tests for three filters of R/aaa_rangayyan_filt2.R (Rangayyan
# 2024): the discrete-frequency Butterworth response, the squared
# Laplace-domain Butterworth and the first difference over a record.

test_that("discrete-frequency Butterworth: half power at the cutoff", {
  w <- c(0, 0.3, 0.6, 1.2, -0.6)
  r <- BwDirect(w, 0.6, 3)
  expect_equal(r$squared_magnitude, 1 / (1 + (abs(w) / 0.6)^6), tolerance = 1e-15)
  expect_equal(r$magnitude, sqrt(r$squared_magnitude))
  expect_equal(BwDirect(0.6, 0.6, 5)$squared_magnitude, 0.5)
  expect_error(BwDirect(w, 0, 2), "cutoff omega_c must be positive")
  expect_error(BwDirect(w, 1, 0), "order N must be at least 1")
})

test_that("squared Butterworth in s has 2N poles on the cutoff circle", {
  s <- complex(real = c(0, -0.5, 0.2), imaginary = c(1.5, 0.3, -2))
  r <- BwSqLap(s, 1.5, 2)
  expect_equal(r$H, 1 / (1 + (s / (1.5i))^4), tolerance = 1e-14)
  # on the imaginary axis H_a(s) H_a(-s) is the squared magnitude response
  expect_equal(Re(BwSqLap(1i * 0.9, 1.5, 2)$H), 1 / (1 + (0.9 / 1.5)^4), tolerance = 1e-14)
  poles <- 1.5 * exp(1i * pi * (2 * (0:3) + 1 + 2) / 4)
  expect_equal(Mod(1 + (poles / 1.5i)^4), rep(0, 4), tolerance = 1e-12)
  expect_equal(r$n_poles, 4L)
  expect_error(BwSqLap(-1.5 + 0i, 1.5, 1), "is a pole")
  expect_error(BwSqLap(s, 0, 2), "Omega_c must be positive")
  expect_error(BwSqLap(s, 1, 0), "order N must be at least 1")
})

test_that("first difference over a record", {
  x <- c(2, 5, 4, 7, 7)
  d <- Diff1(x, T = 0.5)
  expect_equal(d$y, c(2, 3, -1, 3, 0) / 0.5)
  expect_equal(d$b, c(2, -2))
  expect_equal(c(d$a, d$zeros), c(1, 1))
  expect_true(d$highpass)
  expect_error(Diff1(numeric(0)), "at least one sample")
  expect_error(Diff1(x, T = 0), "T must be positive")
})
