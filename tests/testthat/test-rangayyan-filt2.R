# Rangayyan filter design part 2, in R: the 8-point moving average, the
# integrator, the difference operators, the baseline-wander filter, the
# Butterworth family, the bilinear transformation, the DFT-indexed
# responses, notch and comb, the windowed sinc and the windows.
#
# The book's worked fourth-order example, eqs (3.147)-(3.148), is the
# oracle: the pole coordinates and gain below are numbers the book
# prints, not this implementation's output.

WC_F2 <- sqrt(2.111456)
XS_F2 <- c(1, 4, 2, 8, 5, 7, 3, 9, 6, 2, 4, 1, 8, 3, 5)
SP_F2 <- c(1, 1, 9, 1, 1, 1, -9, 1, 1)

test_that("Ma8Imp eq (3.109) is eight equal taps", {
  r <- Ma8Imp()
  expect_equal(r$h, rep(0.125, 8))
  expect_equal(r$sum, 1)
  expect_true(r$equal_weights)
  expect_equal(Ma8Imp(n = 9)$value, 0)
})

test_that("Ma8Tf eq (3.110) vanishes at every multiple of fs/8", {
  expect_equal(Re(Ma8Tf(1)$H), 1)
  expect_equal(Ma8Tf(1)$n_zeros, 7L)
  for (k in 1:7) {
    w <- 2 * pi * k / 8
    z <- complex(real = cos(w), imaginary = sin(w))
    expect_equal(Mod(Ma8Tf(z)$H), 0, tolerance = 1e-12)
  }
})

test_that("Ma8Fr eq (3.111) factored form is exact", {
  r <- Ma8Fr(c(0, 0.3, 1, 2, pi))
  expect_true(r$factored_form_agrees)
  expect_lt(r$max_difference, 1e-12)
  expect_true(r$bracket_is_inside_the_product)
})

test_that("the docstring's product form would have been wrong", {
  # {1 + exp(-j4w)}{1 + 2cos w + ...} is NOT the book's form
  w <- 1
  brack <- 1 + 2 * cos(w) + 2 * cos(2 * w) + 2 * cos(3 * w)
  wrong <- 0.125 * (1 + complex(real = cos(-4 * w),
                                imaginary = sin(-4 * w))) * brack
  expect_gt(Mod(wrong - Ma8Fr(w)$H), 1e-3)
})

test_that("Ma8Rec eq (3.120) matches the direct form", {
  r <- Ma8Rec(XS_F2)
  expect_true(r$agrees_with_direct_form)
  expect_equal(r$additions_per_sample, 2L)
  expect_true(r$error_accumulates)
})

test_that("Ma8RecTf eq (3.121) is one at the removable singularity", {
  expect_equal(Re(Ma8RecTf(1)$H), 1)
  expect_true(Ma8RecTf(1)$still_fir)
  expect_error(Ma8RecTf(0), "pole")
})

test_that("Ma8Sinc eq (3.122) agrees with eq (3.111)", {
  r <- Ma8Sinc(c(0, 0.3, 1, 2))
  expect_true(r$agrees_with_eq_3_111)
  expect_equal(r$group_delay, 3.5)
  expect_true(r$delay_is_not_an_integer)
  # and the measured group delay agrees
  expect_equal(GrpDelay(rep(0.125, 8), fs = 1000, n_freqs = 257)$mean,
               3.5, tolerance = 1e-9)
})

test_that("the integrator responses, eqs (3.116)-(3.118)", {
  expect_equal(IntFr(2)$H, complex(real = 0, imaginary = -0.5))
  expect_equal(IntMag(2)$magnitude, 0.5)
  expect_equal(IntPh(2)$phase, -pi / 2)
  # positive magnitude and flipped phase for negative omega
  expect_equal(IntMag(-2)$magnitude, 0.5)
  expect_equal(IntPh(-2)$phase, pi / 2)
  for (f in list(IntFr, IntMag, IntPh)) expect_error(f(0))
})

test_that("RunInt eq (3.112) and RunIntAll eq (3.113)", {
  t <- (0:200) / 100
  x <- rep(1, 201)
  r <- RunInt(x, t, 0.5)
  expect_equal(r$y[201], 0.5, tolerance = 1e-9)
  expect_gt(r$clipped_windows, 0)
  expect_error(RunInt(x, t, 0), "positive")
  a <- RunIntAll(rep(1, 101), (0:100) / 100)
  expect_equal(a$y[1], 0)
  expect_equal(a$total, 1, tolerance = 1e-12)
  expect_true(a$discrete_pole_on_the_unit_circle)
})

test_that("IntFt eq (3.115) returns the delta weight separately", {
  r <- IntFt(2, 1, X0 = 3)
  expect_equal(r$Y, complex(real = 0, imaginary = -2))
  expect_equal(r$delta_weight, complex(real = pi * 3, imaginary = 0))
  expect_false(r$at_dc)
  z <- IntFt(2, 0, X0 = 3)
  expect_null(z$Y)
  expect_true(z$at_dc)
})

test_that("FDiff eq (3.123) scales by the sampling interval", {
  expect_equal(FDiff(c(0, 1, 3), T = 1)$y, c(0, 1, 2))
  expect_equal(FDiff(c(0, 1, 3), T = 0.5)$y, c(0, 2, 4))
  expect_error(FDiff(c(1, 2), T = 0), "positive")
})

test_that("the first-difference responses, eqs (3.124)-(3.127)", {
  expect_equal(Mod(FDiffTf(1)$H), 0, tolerance = 1e-15)
  expect_true(FDiffFr(c(0, 0.5, 1.5, pi))$forms_agree)
  expect_equal(FDiffMag(pi)$magnitude, 2)
  expect_equal(FDiffMag(0)$magnitude, 0)
  expect_equal(FDiffPh(1)$phase, pi / 2 - 0.5)
  expect_equal(FDiffPh(1)$group_delay, 0.5)
  for (w in c(0.4, 1.2, 2.5)) {
    H <- FDiffFr(w)$H
    expect_equal(Mod(H), FDiffMag(w)$magnitude, tolerance = 1e-12)
    expect_equal(Arg(H), FDiffPh(w)$phase, tolerance = 1e-12)
  }
})

test_that("CDiff3 eq (3.128) is the mean of two first differences", {
  r <- CDiff3(XS_F2)
  expect_true(r$derivation_agrees)
  expect_true(r$controls_noise_amplification)
  expect_true(r$poor_above_fs_over_10)
})

test_that("CDiff3Tf eq (3.129) factors into a difference and a mean", {
  r <- CDiff3Tf(c(0.8, 1.3))
  expect_true(r$cascade_agrees)
  expect_equal(r$zeros, c(1, -1))
  expect_true(r$bandpass)
})

test_that("the central-difference responses, eqs (3.130)-(3.131)", {
  expect_equal(CDiff3Mag(0)$magnitude, 0)
  expect_equal(CDiff3Mag(pi)$magnitude, 0, tolerance = 1e-15)
  expect_equal(CDiff3Mag(pi / 2)$magnitude, 1)
  expect_equal(CDiff3Ph(1)$phase, pi / 2 - 1)
  # a whole sample of delay, against the half sample of eq (3.127)
  expect_gt(CDiff3Ph(1)$group_delay, FDiffPh(1)$group_delay)
})

test_that("Diff2 is two first differences in cascade", {
  r <- Diff2(XS_F2)
  expect_true(r$cascade_agrees)
  expect_equal(r$zeros, c(1, 1))
  expect_true(r$gain_rises_quadratically)
})

test_that("BWander eq (3.132) kills DC and passes the rest", {
  expect_equal(Mod(BWander(1)$H), 0, tolerance = 1e-15)
  expect_equal(BWander(1)$poles, 0.995)
  w <- pi / 2
  expect_equal(Mod(BWander(complex(real = cos(w), imaginary = sin(w)))$H),
               1, tolerance = 0.01)
  expect_error(BWander(0.5, pole = 1), "cancels the zero")
})

test_that("BWanderZ eq (3.133) is the same filter", {
  r <- BWanderZ(c(0.8, 1.3, complex(real = 0.2, imaginary = 0.9)))
  expect_true(r$forms_agree)
  expect_true(r$numerator_is_the_distance_to_the_zero)
})

test_that("BWanderEq eq (3.134) carries a plus on the feedback", {
  r <- BWanderEq(c(1, 0, 0, 0))
  expect_equal(r$feedback_sign, "+")
  # a decaying tail, not an alternating one: the pole is at +0.995
  expect_lt(r$y[2], 0)
  expect_lt(r$y[3], 0)
  expect_lt(abs(r$y[3]), abs(r$y[2]))
  # the step transient decays as pole^n, so 50 samples is nowhere settled
  expect_gt(abs(BWanderEq(rep(5, 50))$y[50]), 3)
  expect_lt(abs(BWanderEq(rep(5, 4000))$y[4000]), 1e-6)
})

test_that("BwSqMag eq (3.135) is half power at cutoff for every order", {
  for (n in c(1, 2, 4, 8))
    expect_equal(BwSqMag(2, 2, n)$squared_magnitude, 0.5)
  expect_equal(BwSqMag(0, 2, 4)$squared_magnitude, 1)
  v <- BwSqMag(c(0, 1, 2, 4, 8), 2, 4)$squared_magnitude
  expect_true(all(diff(v) <= 1e-15))
  expect_error(BwSqMag(1, 0, 4), "positive")
})

test_that("BwPoles eq (3.137) reproduces the book's worked example", {
  p <- BwPoles(WC_F2, 4)$left_half_plane
  got <- sort(paste(round(Re(p), 6), round(abs(Im(p)), 6)))
  expect_equal(got, sort(c("-0.556072 1.342475", "-0.556072 1.342475",
                           "-1.342475 0.556072", "-1.342475 0.556072")))
  expect_equal(length(p), 4L)
  expect_true(BwPoles(WC_F2, 4)$none_on_the_imaginary_axis)
})

test_that("BwPoles lie on a circle spaced evenly", {
  r <- BwPoles(3, 5)
  expect_true(all(abs(Mod(r$poles) - 3) < 1e-12))
  expect_equal(r$angular_spacing, pi / 5)
  expect_true(r$real_pole_for_odd_order)
  expect_error(BwPoles(3, 5, k = 0), "1..2N")
})

test_that("BwAnalog eq (3.138) reproduces the book's gain", {
  r <- BwAnalog(WC_F2, 4)
  expect_equal(r$gain, 4.458247, tolerance = 1e-5)
  expect_true(r$coefficients_are_real)
  expect_equal(Mod(BwAnalog(WC_F2, 4, s = 0)$H), 1)
  # half power at the cutoff
  expect_equal(Mod(BwAnalog(2, 4, s = complex(real = 0, imaginary = 2))$H),
               1 / sqrt(2), tolerance = 1e-9)
})

test_that("the bilinear transformation and its warping", {
  expect_equal(Bilinear(1)$s, complex(real = 0, imaginary = 0))
  expect_error(Bilinear(-1), "infinity")
  expect_true(BilinUnit(c(0.2, 1, 2.5))$sigma_vanishes)
  expect_true(BilinUnit(c(0.2, 1, 2.5))$forms_agree)
  expect_equal(BilinWarp(0)$Omega, 0)
  r <- BilinUnwarp(c(0.5, 2, 10, 1000))
  expect_true(r$inverts_eq_3_141)
  expect_true(r$always_inside_the_open_interval)
  expect_error(BilinWarp(pi), "needs")
  # the warping is nonlinear
  expect_false(isTRUE(all.equal(BilinWarp(1)$Omega,
                                2 * BilinWarp(0.5)$Omega,
                                tolerance = 1e-3)))
})

test_that("BwDigital eq (3.143) puts every zero at z = -1", {
  r <- BwDigital(N = 4, fc = 100, fs = 1000)
  expect_equal(r$zeros_at_minus_one, 4L)
  expect_true(r$zeros_are_forced_by_the_bilinear_transform)
  expect_true(r$leading_a_is_one)
  expect_equal(sum(r$b) / sum(r$a), 1, tolerance = 1e-12)
  expect_error(BwDigital(N = 4), "not both and not neither")
  expect_error(BwDigital(N = 4, fc = 600, fs = 1000), "Nyquist")
})

test_that("BwLp and BwHp are half power at the requested cutoff", {
  w <- 2 * pi * 100 / 1000
  z <- complex(real = cos(w), imaginary = sin(w))
  expect_equal(Mod(BwLp(100, 4, 1000, z = z)$H), 1 / sqrt(2),
               tolerance = 1e-9)
  expect_equal(Mod(BwHp(100, 4, 1000, z = z)$H), 1 / sqrt(2),
               tolerance = 1e-9)
  expect_equal(Mod(BwHp(100, 4, 1000, z = 1)$H), 0, tolerance = 1e-12)
  expect_true(BwLp(100, 4, 1000)$prewarped)
})

test_that("a higher order gives a sharper transition", {
  g <- function(order, f) {
    w <- 2 * pi * f / 1000
    Mod(BwLp(100, order, 1000,
             z = complex(real = cos(w), imaginary = sin(w)))$H)
  }
  expect_lt(g(8, 200), g(2, 200))
  expect_equal(g(8, 100), g(2, 100), tolerance = 1e-6)
})

test_that("the DFT-indexed designs, eqs (3.146) and (3.149)", {
  lo <- BwLpDft(16, kc = 4, N = 2)
  expect_equal(length(lo$magnitude), 16L)
  for (k in 1:7)
    expect_equal(lo$magnitude[k + 1], lo$magnitude[16 - k + 1])
  expect_equal(lo$magnitude[1], 1)
  expect_equal(lo$squared_magnitude[5], 0.5)
  # K wc/ws = 100*33/1000 = 3.3, so the ceiling makes kc 4 not 3
  expect_equal(BwLpDft(100, fc = 33, fs = 1000, N = 2)$kc, 4L)
  hi <- BwHpDft(16, kc = 4, N = 2)
  expect_equal(hi$magnitude[1], 0)
  expect_equal(hi$squared_magnitude[5], 0.5)
  # lowpass and highpass are complementary in power
  a <- BwLpDft(32, kc = 8, N = 3)$squared_magnitude
  b <- BwHpDft(32, kc = 8, N = 3)$squared_magnitude
  for (k in 2:17) expect_equal(a[k] + b[k], 1, tolerance = 1e-12)
})

test_that("Notch60 puts conjugate zeros on the interference", {
  r <- Notch60(1000, 60)
  expect_equal(r$gain_at_the_notch, 0, tolerance = 1e-12)
  expect_equal(r$dc_gain, 1)
  expect_true(r$fir)
  expect_error(Notch60(1000, 600), "Nyquist")
})

test_that("poles narrow the notch", {
  fs <- 1000
  f0 <- 60
  wide <- Notch60(fs, f0)
  gw <- function(f) {
    w <- 2 * pi * f / fs
    z <- complex(real = cos(w), imaginary = sin(w))
    Mod(sum(wide$b * z^-(seq_along(wide$b) - 1L)))
  }
  gn <- function(f) {
    w <- 2 * pi * f / fs
    Mod(Notch(f0, r = 0.98, fs = fs,
              z = complex(real = cos(w), imaginary = sin(w)))$H)
  }
  bw <- function(g) {
    lo <- f0
    hi <- f0
    while (lo > 0.05 && g(lo) < 1 / sqrt(2)) lo <- lo - 0.05
    while (hi < fs / 2 - 0.05 && g(hi) < 1 / sqrt(2)) hi <- hi + 0.05
    hi - lo
  }
  expect_lt(bw(gn), bw(gw) / 5)
  expect_equal(bw(gn), (1 - 0.98) * fs / pi, tolerance = 0.1)
  expect_error(Notch(60, fs = 1000), "not both and not neither")
  expect_error(Notch(60, r = 1, fs = 1000), "cancel the zeros")
  expect_gt(Notch(60, bandwidth = 2, fs = 1000)$r,
            Notch(60, bandwidth = 8, fs = 1000)$r)
})

test_that("Comb notches every harmonic at once", {
  r <- Comb(20, fs = 1000)
  expect_equal(r$notch_spacing_hz, 50)
  expect_equal(r$notch_frequencies_hz[1:3], c(0, 50, 100))
  expect_equal(r$dc_gain, 0)
  expect_true(r$removes_dc_as_well)
  for (f in c(50, 100, 150)) {
    w <- 2 * pi * f / 1000
    z <- complex(real = cos(w), imaginary = sin(w))
    expect_equal(Mod(Comb(20, fs = 1000, z = z)$H), 0, tolerance = 1e-12)
  }
})

test_that("the windows carry the book's coefficients", {
  expect_equal(HannW(9)$endpoints, c(0, 0))
  expect_true(HannW(9)$reaches_zero_at_the_ends)
  expect_equal(HammingW(9)$endpoints, c(0.08, 0.08))
  expect_false(HammingW(9)$reaches_zero_at_the_ends)
  expect_equal(BlackmanW(9)$endpoints, c(0, 0), tolerance = 1e-15)
  for (w in list(HannW(15), HammingW(15), BlackmanW(15))) {
    expect_true(w$symmetric)
    expect_equal(max(w$w), 1, tolerance = 1e-9)
  }
  # the Hann WINDOW is not the Hann FILTER of eq (3.100)
  expect_true(HannW(9)$not_the_hann_filter_of_eq_3_100)
  expect_false(isTRUE(all.equal(HannW(3)$w, c(0.25, 0.5, 0.25))))
  for (f in list(HannW, HammingW, BlackmanW)) expect_equal(f(1)$w, 1)
})

test_that("WindowFn dispatches and names the rectangular default", {
  expect_equal(WindowFn(5, "rectangular")$w, rep(1, 5))
  expect_equal(WindowFn(9, "hann")$w, HannW(9)$w)
  expect_equal(WindowFn(9, "blackman")$w, BlackmanW(9)$w)
  expect_true(WindowFn(5, "hann")$doing_nothing_is_the_rectangular_window)
  expect_error(WindowFn(5, "bartlett"), "window_type must be")
  expect_error(WindowFn(0), "at least 1")
})

test_that("SincKern is symmetric, normalized, and tapered by a window", {
  r <- SincKern(100, fs = 1000, M = 32)
  expect_equal(sum(r$h), 1, tolerance = 1e-12)
  expect_equal(r$delay_samples, 16)
  expect_equal(r$h[1:16], rev(r$h[18:33]))
  expect_true(r$truncation_causes_gibbs_ripple)
  tapered <- SincKern(100, fs = 1000, M = 64, window = "hamming")
  expect_false(tapered$truncation_causes_gibbs_ripple)
  a <- FreqResp(SincKern(100, fs = 1000, M = 64)$h, fs = 1000,
                n_freqs = 1024)$magnitude
  b <- FreqResp(tapered$h, fs = 1000, n_freqs = 1024)$magnitude
  expect_lt(max(b[401:1024]), max(a[401:1024]))
})

test_that("FreqResp runs DC to Nyquist inclusive", {
  r <- FreqResp(c(0.25, 0.5, 0.25), fs = 1000, n_freqs = 101)
  expect_equal(r$f[1], 0)
  expect_equal(r$f[101], 500)
  expect_equal(r$magnitude[1], 1)
  expect_equal(r$magnitude[101], 0, tolerance = 1e-15)
  expect_true(r$one_sided)
  expect_error(FreqResp(1, a = c(1, -1), fs = 1000, n_freqs = 8),
               "unit circle")
})

test_that("PhaseResp marks where the response vanishes", {
  r <- PhaseResp(c(0.25, 0.5, 0.25), fs = 1000, n_freqs = 101)
  expect_true(r$unwrap)
  expect_equal(r$n_undefined, 1L)      # Nyquist, where H is exactly zero
  good <- r$unwrapped[r$defined]
  expect_true(all(diff(good) <= 1e-12))
  w <- 2 * pi * r$f[r$defined] / 1000
  expect_equal((good[length(good)] - good[1]) / (w[length(w)] - w[1]),
               -1, tolerance = 1e-9)
})

test_that("group delay of a symmetric FIR is half its order", {
  expect_equal(GrpDelay(c(0.25, 0.5, 0.25), fs = 1000, n_freqs = 257)$mean,
               1, tolerance = 1e-9)
  expect_equal(GrpDelay(rep(1 / 3, 3), fs = 1000, n_freqs = 257)$mean,
               1, tolerance = 1e-9)
  expect_equal(GrpDelay(rep(0.125, 8), fs = 1000, n_freqs = 257)$mean,
               3.5, tolerance = 1e-9)
  # the three-point mean has a zero at w = 2pi/3; differentiating the
  # phase there gives a spike, the coefficient formula does not
  expect_true(GrpDelay(rep(1 / 3, 3), fs = 1000)$from_the_coefficients)
  lp <- BwLp(100, 4, 1000)
  expect_false(GrpDelay(lp$b, lp$a, fs = 1000)$approximately_constant)
})

test_that("IirDiffGen eq (3.144) runs a designed filter", {
  lp <- BwLp(100, 2, 1000)
  y <- IirDiffGen(rep(1, 80), lp$b, lp$a[-1])$y
  expect_equal(y[80], 1, tolerance = 1e-6)
})

test_that("MfiltH reverses the template and peaks where it sits", {
  r <- MfiltH(c(1, 2, 3))
  expect_equal(r$h, c(3, 2, 1))
  expect_equal(r$peak_index, 2L)
  expect_equal(r$energy, 14)
  n <- MfiltH(c(1, 2, 3), normalize = TRUE)
  expect_equal(sum(n$h^2), 1)
  expect_error(MfiltH(c(0, 0), normalize = TRUE), "no energy")
  g <- c(1, 2, 3, 2, 1)
  h <- MfiltH(g)$h
  x <- c(rep(0, 8), g, rep(0, 8))
  y <- vapply(seq_along(x), function(i) {
    k <- which(i - (seq_along(h) - 1L) >= 1L)
    sum(h[k] * x[i - (k - 1L)])
  }, numeric(1))
  expect_equal(which.max(y), 8L + length(g))
})
