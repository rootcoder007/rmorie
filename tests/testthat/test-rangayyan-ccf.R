# The 14 Rangayyan methods that were still stubs inside four modules
# reported complete.  Same assertions as the Python arm; every expected
# value is an arithmetic identity or a book equation, never a value read
# back off the implementation.

sine <- function(n, cycles_hz, fs = 100, amp = 1, phase = 0) {
  amp * sin(2 * pi * cycles_hz * (seq_len(n) - 1L) / fs + phase)
}

# ------------------------------------------------------------ GLR, 8.30/8.31
test_that("Glr is the book difference of log-likelihoods, eq (8.31)", {
  r <- Glr(sine(400, 5), 201, order = 4)
  expect_close(r$d, r$h_pooled - (r$h_reference + r$h_test), tol = 1e-12)
})

test_that("Glr rises when the process changes", {
  stationary <- Glr(sine(400, 5), 201, order = 4)$d
  changed <- Glr(c(sine(200, 5), sine(200, 40)), 201, order = 4)$d
  expect_true(changed > stationary)
  expect_true(changed > 0)
})

test_that("Glr windows partition the record", {
  r <- Glr(sine(300, 5), 151, 300, order = 4)
  expect_equal(r$n_reference + r$n_test, 300)
})

test_that("Glr rejects windows shorter than the AR order", {
  expect_error(Glr(sine(100, 5), 4, order = 4))
})

test_that("EegAdapt finds the one boundary and restarts the reference", {
  r <- EegAdapt(c(sine(200, 5), sine(200, 40)), 100, window = 60, step = 20)
  expect_equal(r$n_boundaries, 1L)
  expect_true(r$reference_restarts_at_boundaries)
  # A sliding window resolves a change to within one window, not one
  # sample: the detector first sees it at pos = 200 - w and must have
  # fired by pos = 200.
  expect_true(r$boundaries[1L] >= 200 - r$window)
  expect_true(r$boundaries[1L] <= 200)
})

test_that("EegAdapt uses a robust threshold by default", {
  r <- EegAdapt(sine(400, 5), 100, window = 60, step = 20)
  expect_true(r$robust_threshold)
  expect_close(r$threshold, r$median + 3 * 1.4826 * r$mad, tol = 1e-12)
})

test_that("EegAdapt refuses a window longer than the record", {
  expect_error(EegAdapt(sine(50, 5), 100, window = 200))
})

# ---------------------------------------------------------------------- CCF
test_that("XCorr peak lag is the delay, with the book's sign", {
  x <- sine(200, 3)
  y <- c(rep(0, 7), x[seq_len(193)])
  expect_equal(XCorr(x, y, maxlag = 20)$peak_lag, 7L)
})

test_that("XCorr at lag zero of x with itself is the mean square", {
  x <- c(1, 2, 3, 4)
  r <- XCorr(x, x, maxlag = 0, biased = TRUE)
  expect_close(r$ccf[[1L]], sum(x * x) / 4, tol = 1e-12)
})

test_that("XCorr normalized is bounded and hits one on a copy", {
  x <- sine(200, 3)
  r <- XCorr(x, x, maxlag = 10, normalize = TRUE)
  expect_true(max(abs(r$ccf)) <= 1 + 1e-12)
  expect_close(r$ccf[which(r$lags == 0L)], 1, tol = 1e-12)
})

test_that("XCorrDisc agrees with XCorr on the peak", {
  x <- sine(200, 3)
  y <- c(rep(0, 5), x[seq_len(195)])
  expect_equal(XCorrDisc(x, y)$peak_lag, 5L)
  expect_equal(XCorr(x, y, maxlag = 30)$peak_lag, 5L)
})

test_that("CcfCont overlap shrinks as the delay grows", {
  tt <- 0:19
  x <- sine(20, 2)
  a <- XCorrCont(x, x, tt, 0)$overlap_fraction
  b <- XCorrCont(x, x, tt, 10)$overlap_fraction
  expect_equal(a, 1)
  expect_true(b < a)
})

test_that("CcfProc with the mean removed is the cross-covariance", {
  x <- c(1, 2, 3, 4); y <- c(2, 4, 6, 8)
  r <- XCorrProc(x, y, lags = 0, remove_mean = TRUE)
  expect_true(r$is_cross_covariance_when_mean_removed)
  want <- sum((x - 2.5) * (y - 5)) / 4
  expect_close(r$ccf[which(r$lags == 0L)], want, tol = 1e-12)
})

test_that("CorrConv: correlation is convolution with one reversed", {
  r <- CorrConv(c(1, 2, 3), c(1, 0, -1))
  expect_true(r$identity_holds)
  expect_close(r$max_difference, 0, tol = 1e-12)
  expect_close(r$ccf, r$via_convolution, tol = 1e-12)
})

test_that("NccfTpl is bounded and locates the template", {
  x <- sine(200, 3)
  r <- NccfTpl(x, x[41:70])
  expect_true(r$bounded_in_unit_interval)
  expect_true(max(r$gamma) <= 1 + 1e-12)
  expect_equal(r$peak_shift, 40L)
  expect_close(r$peak, 1, tol = 1e-9)
})

test_that("CorrDot is the cosine and is scale-free", {
  expect_close(CorrDot(c(1, 2, 3), c(2, 4, 6))$gamma, 1, tol = 1e-12)
  expect_close(CorrDot(c(1, 0), c(0, 1))$gamma, 0, tol = 1e-12)
  expect_close(CorrDot(c(1, 2), c(1, 2))$gamma,
               CorrDot(c(1, 2), c(100, 200))$gamma, tol = 1e-12)
})

test_that("EegAcf recovers the rhythm frequency", {
  r <- EegAcf(sine(400, 10, 100), 100)
  expect_close(r$implied_frequency_hz, 10, tol = 0.5)
})

test_that("AlphaRhy fires in band and reports the frequency", {
  r <- AlphaRhy(sine(400, 10, 100), 100)
  expect_true(r$present)
  expect_true(r$frequency_hz >= 8 && r$frequency_hz <= 13)
})

test_that("AlphaRhy band is the conventional 8-13 Hz", {
  expect_close(AlphaRhy(sine(400, 10, 100), 100)$band, c(8, 13), tol = 0)
})

# -------------------------------------------------- synthetic test signals
test_that("SinCosTest length is duration times rate", {
  expect_equal(SinCosTest(fs = 100, duration = 2.5)$n, 250L)
})

test_that("SinCosTest is the sum of its two named components", {
  r <- SinCosTest(fs = 100, duration = 0.5, f1 = 5, f2 = 20, a1 = 2,
                  a2 = 0.5)
  want <- r$a1 * sin(2 * pi * r$f1 * r$t) + r$a2 * cos(2 * pi * r$f2 * r$t)
  expect_close(r$x, want, tol = 1e-12)
})

test_that("CompSig peaks are where a matched filter actually peaks", {
  g <- c(1, 2, 1)
  r <- CompSig(g, c(0, 10, 20))
  expect_equal(r$overlapping_pairs, 0L)
  # Do not take the reported positions on trust -- run the matched filter
  # h(n) = g(M-1-n) over the composite and find its peaks.
  h <- rev(g)
  y <- vapply(seq_along(r$x), function(i)
    sum(vapply(seq_along(h), function(k) {
      j <- i - k + 1L
      if (j >= 1L && j <= length(r$x)) r$x[j] * h[k] else 0
    }, numeric(1))), numeric(1))
  found <- which(vapply(seq_along(y), function(i)
    y[i] == max(y[max(1L, i - 2L):min(length(y), i + 2L)]) &&
      y[i] > 0, logical(1))) - 1L
  expect_equal(as.integer(r$peaks_expected_at), c(2L, 12L, 22L))
  expect_true(all(c(2L, 12L, 22L) %in% found))
})

test_that("CompSig reports overlap when shifts are closer than the pulse", {
  expect_equal(CompSig(c(1, 2, 1), c(0, 1))$overlapping_pairs, 1L)
})

# ------------------------------------------------------------------ CorrCoef
test_that("CorrCoef is 1 on a positive scaling and -1 on negation", {
  expect_close(CorrCoef(c(1, 2, 3, 4), c(2, 4, 6, 8))$r, 1, tol = 1e-12)
  expect_close(CorrCoef(c(1, 2, 3, 4), c(-2, -4, -6, -8))$r, -1, tol = 1e-12)
})

test_that("CorrCoef is the zero-lag normalized XCorr of the centred signals", {
  x <- sine(120, 3)
  y <- sine(120, 3, phase = 0.7)
  cx <- x - mean(x); cy <- y - mean(y)
  lag0 <- XCorr(cx, cy, maxlag = 0, normalize = TRUE)$ccf[[1L]]
  expect_close(CorrCoef(x, y)$r, lag0, tol = 1e-9)
})

test_that("CorrCoef refuses a constant signal", {
  expect_error(CorrCoef(c(1, 1, 1), c(1, 2, 3)))
})
