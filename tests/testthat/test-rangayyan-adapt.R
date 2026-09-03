# Rangayyan optimal and adaptive filtering, in R.  Same book equations as
# the Python arm, and the same hand-computed expected values.

sine_a <- function(n, cycles, amp = 1, phase = 0)
  amp * sin(2 * pi * cycles * (0:(n - 1)) / n + phase)

lcg_a <- function(n, seed = 7, lo = -0.5, hi = 0.5) {
  out <- numeric(n)
  s <- seed
  for (i in seq_len(n)) {
    s <- (1103515245 * s + 12345) %% 2147483648
    out[i] <- lo + (hi - lo) * (s / 2147483648)
  }
  out
}

test_that("WienerOut is eq (3.154), a convolution", {
  r <- WienerOut(c(1, 0.5), c(1, 2, 3))
  expect_equal(r$d_hat, c(1, 2.5, 4))
  expect_equal(r$settled_from, 1L)
})

test_that("WienerDot eq (3.155) matches the convolution form", {
  w <- c(1, 0.5)
  x <- c(1, 2, 3)
  conv <- WienerOut(w, x)$d_hat
  # x(n) runs backwards in time
  expect_equal(WienerDot(w, c(x[3], x[2]))$d_hat, conv[3])
  expect_error(WienerDot(c(1, 2), 1), "same length")
})

test_that("MseGrad eq (3.167) vanishes at the optimum", {
  Phi <- matrix(c(2, 1, 1, 2), 2, 2)
  Theta <- c(3, 3)
  w <- WienerOpt(Phi, Theta)$w_o
  g <- MseGrad(Phi, Theta, w)
  expect_equal(g$gradient, c(0, 0), tolerance = 1e-9)
  expect_true(g$at_optimum)
  expect_equal(MseGrad(Phi, Theta, c(0, 0))$gradient, c(-6, -6))
})

test_that("WienerHopf eq (3.168) solves the normal equation", {
  r <- WienerHopf(matrix(c(2, 1, 1, 2), 2, 2), c(3, 3))
  expect_equal(r$w, c(1, 1))
  expect_equal(r$max_residual, 0, tolerance = 1e-12)
  expect_error(WienerHopf(matrix(1, 2, 2), c(1, 2)), "singular")
})

test_that("WienerOpt eq (3.169) solves rather than inverting", {
  r <- WienerOpt(matrix(c(2, 1, 1, 2), 2, 2), c(3, 3))
  expect_equal(r$w_o, c(1, 1))
  expect_true(r$solved_not_inverted)
})

test_that("WienerMin eq (3.172) is the variance less the explained part", {
  Phi <- matrix(c(2, 1, 1, 2), 2, 2)
  Theta <- c(3, 3)
  r <- WienerMin(Phi, Theta, 10)
  expect_equal(r$explained, 6)
  expect_equal(r$j_min, 4)
  expect_true(r$consistent)
  # a variance too small for the covariances cannot be right
  bad <- WienerMin(Phi, Theta, 1)
  expect_lt(bad$j_min, 0)
  expect_false(bad$consistent)
})

test_that("WienerConv eq (3.174) holds at the solution", {
  phi <- c(2, 1, 0.5)
  theta <- c(3, 3)
  Phi <- matrix(c(phi[1], phi[2], phi[2], phi[1]), 2, 2)
  r <- WienerConv(WienerOpt(Phi, theta)$w_o, phi, theta)
  expect_true(r$holds)
  expect_true(r$requires_stationarity)
})

test_that("WienerFreqR eq (3.175) flags undetermined bins", {
  r <- WienerFreqR(c(1, 0.5, 0), c(2, 4, 0), c(2, 2, 0))
  expect_true(r$holds)
  expect_equal(r$undetermined_bins, 2L)
})

test_that("WienerFreq eq (3.176) is the CSD over the PSD", {
  r <- WienerFreq(c(2, 4), c(1, 2))
  expect_equal(Re(r$W), c(0.5, 0.5))
  z <- WienerFreq(c(1, 0), c(1, 5))
  expect_equal(z$W[2], complex(real = 0, imaginary = 0))
  expect_equal(z$undetermined_bins, 1L)
  expect_true(z$zero_where_undetermined)
})

test_that("WienerSnr eq (3.186) has the three stated properties", {
  r <- WienerSnr(c(0, 1, 4, 1), c(1, 0, 1, 9))
  expect_equal(r$W[1], 0)                 # nothing to restore
  expect_equal(r$W[2], 1)                 # noiseless
  expect_equal(r$W[3], 0.8)
  expect_equal(r$W[4], 0.1)
  expect_gt(r$W[3], r$W[4])               # falls with the SNR
  expect_true(r$zero_where_signal_absent)
  expect_true(r$unity_where_noise_absent)
  expect_error(WienerSnr(c(1, -1), c(1, 1)), "cannot be negative")
})

test_that("Whopf builds a Toeplitz system from data", {
  n <- 400
  x <- sine_a(n, 7)
  d <- 0.5 * x
  r <- Whopf(x, d, order = 3)
  expect_true(r$toeplitz)
  expect_true(r$acf_biased)
  expect_equal(length(r$w), 3L)
  expect_lt(r$j_min, r$var_d)
  expect_error(Whopf(c(1, 2), c(1, 2), order = 5), "more samples")
})

test_that("WienerFilt needs exactly one route", {
  x <- sine_a(64, 3)
  expect_error(WienerFilt(x), "not both and not neither")
  expect_error(WienerFilt(x, desired = x, sd = 1, seta = 1),
               "not both and not neither")
  expect_error(WienerFilt(sine_a(32, 3), sd = rep(1, 17)), "BOTH")
})

test_that("WienerFilt time route recovers a scaled signal", {
  n <- 400
  x <- sine_a(n, 7)
  d <- 0.5 * x
  r <- WienerFilt(x, desired = d, order = 3)
  expect_equal(r$route, "time")
  expect_lt(max(abs(r$y[11:n] - d[11:n])), 0.05)
})

test_that("WienerFilt frequency route suppresses a noisy band", {
  n <- 64
  x <- sine_a(n, 4)
  half <- n %/% 2 + 1
  k <- 0:(half - 1)
  r <- WienerFilt(x, sd = as.numeric(k == 4), seta = as.numeric(k != 4))
  expect_equal(r$route, "frequency")
  expect_equal(r$y, x, tolerance = 1e-9)
})

test_that("AncInput checks the independence premise", {
  v <- sine_a(256, 3)
  m <- sine_a(256, 41)
  r <- AncInput(v, m)
  expect_equal(r$x, v + m)
  expect_true(r$independent)
  bad <- AncInput(v, 0.5 * v)
  expect_equal(bad$correlation, 1)
  expect_false(bad$independent)
})

test_that("AncOut eq (3.196) makes the error the output", {
  r <- AncOut(c(1, 2, 3), c(0.5, 0.5, 0.5))
  expect_equal(r$e, c(0.5, 1.5, 2.5))
  expect_equal(r$v_hat, r$e)
  expect_true(r$error_is_the_output)
})

test_that("LmsOut eq (3.195) filters the reference", {
  r <- LmsOut(c(1, 0.5), c(2, 4))
  expect_equal(r$y, c(2, 5))
  expect_true(r$filters_the_reference)
})

test_that("LmsSqErr eq (3.200) expands the square", {
  r <- LmsSqErr(3, c(1, 2), c(0.5, 0.25))
  expect_equal(r$e, 2)
  expect_true(r$agrees)
  expect_true(r$nonnegative)
  expect_true(r$instantaneous_not_expected)
})

test_that("LmsDescent eqs (3.201)-(3.202) equal Widrow-Hoff", {
  w <- c(0.1, -0.2)
  e <- 0.7
  r <- c(1, 2)
  mu <- 0.05
  expect_equal(LmsDescent(w, e, r, mu)$w_next, WidrowHoff(w, e, r, mu)$w_next)
  expect_equal(LmsDescent(w, e, r, mu)$gradient, -2 * e * r)
})

test_that("WidrowHoff eq (3.203) keeps the factor of two", {
  r <- WidrowHoff(c(0, 0), 1, c(1, 2), 0.1)
  expect_equal(r$w_next, c(0.2, 0.4))
  expect_true(r$factor_of_two_is_in_the_equation)
  expect_true(WidrowHoff(0, 1, 1, 0.5)$within_bound)
  expect_false(WidrowHoff(0, 1, 1, 5)$within_bound)
})

test_that("LmsVarStep eq (3.204) is eq (3.203) with a moving mu", {
  a <- LmsVarStep(c(0, 0), 1, c(1, 2), 0.1)
  expect_equal(a$w_next, WidrowHoff(c(0, 0), 1, c(1, 2), 0.1)$w_next)
  expect_true(a$time_varying)
})

test_that("LmsZhang eq (3.205) normalizes by the running power", {
  r <- LmsZhang(0.5, 4, 2, alpha = 0.02)
  expect_gt(r$power, 0)
  expect_equal(r$mu, 0.5 / (5 * r$power))
  expect_lt(LmsZhang(0.5, 4, 20, alpha = 0.02)$mu, r$mu)
  expect_error(LmsZhang(1.5, 4, 1), "0 < mu < 1")
  expect_error(LmsZhang(0.5, 4, 1, alpha = 0.9), "alpha")
})

test_that("LmsFilt cancels a correlated interference", {
  n <- 2000
  v <- sine_a(n, 5)
  ref <- sine_a(n, 61)
  x <- v + 0.8 * ref
  r <- LmsFilt(x, ref, order = 4, mu = 0.005)
  tail_i <- (n %/% 2 + 1):n
  expect_lt(max(abs(r$e[tail_i] - v[tail_i])),
            max(abs(x[tail_i] - v[tail_i])) / 4)
  expect_true(r$within_bound)
  expect_true(r$converges_in_the_mean_only)
  # the taps are not c(0.8, 0, 0, 0); the GAIN at the reference frequency is
  w <- 2 * pi * 61 / n
  k <- seq_along(r$final_weights) - 1L
  gain <- Mod(sum(r$final_weights * exp(-1i * k * w)))
  expect_equal(gain, 0.8, tolerance = 0.03)
  expect_error(LmsFilt(sine_a(64, 3), sine_a(64, 7), order = 2, mu = -1),
               "positive")
})

test_that("LMS misadjustment grows with the step size", {
  n <- 2000
  v <- sine_a(n, 5)
  ref <- sine_a(n, 61)
  x <- v + 0.8 * ref
  tail_i <- (n %/% 2 + 1):n
  resid <- function(mu)
    max(abs(LmsFilt(x, ref, order = 4, mu = mu)$e[tail_i] - v[tail_i]))
  r <- vapply(c(0.005, 0.02, 0.05), resid, numeric(1))
  expect_equal(r, sort(r))
})

test_that("LmsFilt variable step survives a reference starting at zero", {
  ref <- sine_a(256, 41)
  expect_equal(ref[1], 0)
  r <- LmsFilt(sine_a(256, 5), ref, order = 4, mu = 0.5, variable = TRUE)
  expect_true(r$variable_step)
  expect_equal(length(r$step_history), 256L)
  expect_true(all(is.finite(r$step_history) & r$step_history > 0))
})

test_that("RlsObj eq (3.206) weights recent errors more", {
  r <- RlsObj(c(1, 1, 1), 0.5)
  expect_equal(r$weights, c(0.25, 0.5, 1))
  expect_equal(r$xi, 1.75)
  expect_equal(r$memory, 2)
  expect_true(RlsObj(c(1, 1), 1)$growing_window)
  expect_equal(RlsObj(c(1, 1), 1)$memory, Inf)
  expect_error(RlsObj(1, 1.5), "lambda")
})

test_that("RlsNormal eq (3.207) has the Wiener-Hopf form", {
  r <- RlsNormal(matrix(c(2, 1, 1, 2), 2, 2), c(3, 3))
  expect_equal(r$w_tilde, c(1, 1))
  expect_true(r$same_form_as_wiener_hopf)
  expect_true(r$direct_inversion)
})

test_that("AbcdLemma eq (3.213) matches the direct inverse", {
  r <- AbcdLemma(matrix(c(4, 1, 1, 3), 2, 2), matrix(c(1, 2), 2, 1),
                 matrix(1, 1, 1), matrix(c(1, 2), 1, 2))
  expect_true(r$holds)
  expect_true(r$scalar_when_k_is_one)
  expect_equal(r$max_difference, 0, tolerance = 1e-9)
})

test_that("RlsUpdate eq (3.224) uses the plus form", {
  r <- RlsUpdate(c(1, 2), c(0.5, 0.25), 2)
  expect_equal(r$w_next, c(2, 2.5))
  expect_equal(r$sign, "+")
  expect_false(is.null(r$erratum))
})

test_that("RlsApriori eq (3.225) uses the previous weights", {
  r <- RlsApriori(5, c(1, 2), c(0.5, 1))
  expect_equal(r$prediction, 2.5)
  expect_equal(r$alpha, 2.5)
  expect_true(r$uses_previous_weights)
})

test_that("RlsFilt converges faster than LMS", {
  n <- 600
  ref <- sine_a(n, 31)
  x <- 0.8 * ref
  early <- 21:60
  expect_lt(max(abs(RlsFilt(x, ref, order = 3, lam = 0.98)$e[early])),
            max(abs(LmsFilt(x, ref, order = 3, mu = 0.01)$e[early])))
})

test_that("RlsFilt keeps P symmetric", {
  r <- RlsFilt(sine_a(400, 11), sine_a(400, 29), order = 4, lam = 0.99)
  expect_true(r$p_symmetrized)
  expect_lt(r$p_symmetry_error, 1e-6)
  expect_error(RlsFilt(sine_a(64, 3), sine_a(64, 7), order = 2, lam = 1.5),
               "lambda")
})

test_that("RlsLattice reports every order and its stability", {
  r <- RlsLattice(sine_a(500, 13), order = 4)
  expect_equal(length(r$reflection), 4L)
  expect_true(r$stable)
  expect_true(r$every_stage_is_a_predictor)
  expect_true(all(abs(r$reflection) < 1))
})

test_that("RlsMonitor excludes the convergence transient", {
  n <- 800
  x <- c(sine_a(n, 7)[1:(n / 2)], sine_a(n, 61)[(n / 2 + 1):n])
  r <- RlsMonitor(x, order = 4, settle = 100, window = 40)
  expect_true(r$transient_excluded)
  expect_equal(r$settle, 100L)
  expect_true(all(r$boundaries >= 100))
})

test_that("RlsMonitor finds a change of statistics", {
  n <- 800
  x <- c(sine_a(n, 5)[1:(n / 2)], sine_a(n, 71)[(n / 2 + 1):n])
  expect_gte(RlsMonitor(x, order = 4, settle = 80, window = 30,
                        threshold = 3)$n_boundaries, 1)
})

test_that("Kalman tracks a constant state", {
  z <- lapply(3 + lcg_a(200, seed = 11), function(v) v)
  r <- Kalman(z, matrix(1), matrix(1), matrix(1e-6), matrix(0.5),
              x0 = 0, P0 = matrix(10))
  expect_equal(r$states[[200]][1], 3, tolerance = 0.15)
  expect_lt(r$covariances[[200]][1, 1], 10)
  expect_true(r$p_symmetrized)
  expect_false(r$joseph_form)
})

test_that("Kalman covariance shrinks monotonically for a static state", {
  r <- Kalman(rep(list(1), 40), matrix(1), matrix(1), matrix(0), matrix(1),
              x0 = 0, P0 = matrix(5))
  p <- vapply(r$covariances, function(m) m[1, 1], numeric(1))
  expect_true(all(diff(p) <= 1e-12))
  expect_error(Kalman(list(c(1, 2)), matrix(1), matrix(1), matrix(1),
                      matrix(1)), "length 1")
})

test_that("Riccati is the fixed point of the Kalman recursion", {
  F <- matrix(0.9)
  H <- matrix(1)
  Q <- matrix(0.1)
  R <- matrix(1)
  r <- Riccati(F, H, Q, R)
  expect_true(r$converged)
  # the scalar DARE has a closed form here: p^2 + 0.09 p - 0.1 = 0
  expect_equal(r$P[1, 1], (-0.09 + sqrt(0.09^2 + 0.4)) / 2,
               tolerance = 1e-9)
  # the filter stores the UPDATED covariance, P (1 - K H)
  k <- Kalman(rep(list(0), 500), F, H, Q, R, x0 = 0, P0 = matrix(1))
  expect_equal(k$covariances[[500]][1, 1], r$P[1, 1] * (1 - r$K[1, 1]),
               tolerance = 1e-9)
  expect_true(r$steady_state_is_the_wiener_solution)
})

test_that("Sem is scale free", {
  a <- c(1, 2, 4, 8)
  expect_equal(Sem(a, a)$sem, 0)
  g <- Sem(3 * a, a)
  expect_equal(g$shape_only, 0, tolerance = 1e-12)
  expect_equal(g$mean_offset, log(3))
  expect_true(g$scale_free)
  expect_gt(Sem(rev(a), a)$shape_only, 0.5)
  expect_error(Sem(c(1, -1), c(1, 1)), "cannot be negative")
})

test_that("Acfseg eq (8.27): the power distance sees a gain change", {
  a <- sine_a(200, 7)
  r <- Acfseg(5 * a, a)
  # the square roots of the powers are in the ratio 5, so d_P = 4
  expect_equal(r$power_distance, 4)
  expect_false(r$amplitude_invariant)
  expect_true(r$boundary)
})

test_that("Acfseg eq (8.28) vanishes on an identical window", {
  a <- sine_a(200, 7)
  r <- Acfseg(a, a)
  expect_equal(r$power_distance, 0, tolerance = 1e-12)
  expect_equal(r$spectral_distance, 0, tolerance = 1e-12)
  expect_equal(r$distance, 0, tolerance = 1e-12)
  expect_false(r$boundary)
  expect_gt(Acfseg(sine_a(200, 47), sine_a(200, 5))$spectral_distance, 0.1)
})

test_that("Acfseg eq (8.29) weights the two distances by the thresholds", {
  r <- Acfseg(sine_a(200, 47), sine_a(200, 5), thp = 2, thf = 4)
  expect_equal(r$distance, r$power_distance / 2 + r$spectral_distance / 4)
  expect_equal(r$boundary, r$distance > 1)
})

test_that("Acfseg picks q where the ACFs first turn negative", {
  a <- sine_a(400, 8)
  r <- Acfseg(a, a)
  expect_true(r$lags_auto)
  expect_gte(r$lags, 1L)
  # every ACF value under a square root in eq (8.28) is nonnegative
  expect_true(all(r$acf_test >= 0))
  expect_true(all(r$acf_reference >= 0))
  expect_equal(Acfseg(a, a, lags = r$lags)$lags, r$lags)
  expect_error(Acfseg(a, a, lags = r$lags + 1L), "turn")
  expect_error(Acfseg(rep(0, 200), a), "zero energy")
  expect_error(Acfseg(a, a, thp = 0), "Th_P")
})

test_that("PcgSeg restarts the reference at each boundary", {
  n <- 1200
  x <- c(sine_a(n, 20)[1:(n / 2)], sine_a(n, 120)[(n / 2 + 1):n])
  r <- PcgSeg(x, fs = 1000, window = 100, step = 50, order = 4)
  expect_true(r$reference_restarted_at_boundaries)
  expect_true(r$robust_threshold)
  expect_equal(length(r$sem), length(r$times))
})

test_that("PsdAcf eq (4.30) agrees with the circular ACF", {
  r <- PsdAcf(sine_a(64, 5))
  expect_true(r$holds)
  expect_true(r$linear_acf_is_smoothed)
})

test_that("Anc and FetalEcg report reference leakage", {
  n <- 1000
  v <- sine_a(n, 3)
  ref <- sine_a(n, 57)
  r <- Anc(v + 0.8 * ref, ref, order = 4, mu = 0.005)
  expect_equal(r$adaptation, "lms")
  expect_true(r$well_separated)
  expect_error(Anc(v, ref, method = "nlms"), "lms")
  f <- FetalEcg(v + 0.8 * ref, ref, order = 4, mu = 0.005)
  expect_equal(length(f$fetal), n)
  expect_gt(f$suppression_db, 0)
  expect_true(f$single_reference)
  expect_true(f$widrow_used_multiple_references)
  expect_error(FetalEcg(v, ref[-1]), "same length")
})
