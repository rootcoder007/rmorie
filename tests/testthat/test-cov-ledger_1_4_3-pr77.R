# R/ledger_1_4_3.R, the lines PR #77 added that no other test reached: the null Gini of an all-zero
# Poisson draw, the hot-spot threshold without attractiveness diffusion, and every refusal of the
# 1.4.3 ledger functions -- against base R and the closed forms the help pages state.

test_that("an all-zero Poisson draw has Gini 0 in the null band", {
  x <- c(1, rep(0, 9))           # mean 0.1: about a third of the draws are all zero
  r <- morie_concentration_null_bound(x, n_sim = 200L, seed = 3L)
  gini <- function(s) sum(abs(outer(s, s, "-"))) / (2 * length(s)^2 * mean(s))  # mean-difference form
  set.seed(3L)
  sims <- vapply(seq_len(200L), function(i) stats::rpois(10L, 0.1), numeric(10))
  zero <- colSums(sims) == 0
  expect_gt(sum(zero), 0L)
  ref <- apply(sims, 2L, function(s) if (sum(s) == 0) 0 else gini(s))
  expect_equal(r$null_mean, mean(ref), tolerance = 1e-12)
  expect_equal(r$null_q95, unname(stats::quantile(ref, 0.95)), tolerance = 1e-12)
  expect_equal(r$gini, gini(x), tolerance = 1e-12)
})

test_that("without attractiveness diffusion the threshold is Q > 0 and the critical mode has det = -R", {
  # eta = 0: P = 0 and det(k) = R - Q k, with Q = D (3 eps D rho - omega) and R = omega A
  r <- morie_hotspot_threshold(A0 = 1, omega = 1, eps = 1, D = 1, gamma = 1, eta = 0, k = c(0, 4, 8))
  Abar <- 1 + 1 * 1 * 1 / 1
  rho <- 1 / Abar
  Q <- 1 * (3 * 1 * 1 * rho - 1)
  R <- 1 * Abar
  expect_identical(r$P, 0)
  expect_equal(c(r$Q, r$R), c(Q, R))
  expect_true(r$unstable)
  expect_equal(r$k_crit, 2 * R / Q)
  expect_equal(r$det_crit, -R)
  # the growth rate is the largest real part of the linearised matrix's eigenvalues (base eigen)
  a <- -1 + 1 * 1 * rho
  M <- function(k) matrix(c(a, -rho + 2 * 1 * rho / Abar * k, 1 * 1 * Abar, -Abar - 1 * k), 2L)
  expect_equal(r$growth_crit, max(Re(eigen(M(r$k_crit))$values)), tolerance = 1e-12)
  expect_equal(r$dispersion$det, vapply(c(0, 4, 8), function(k) det(M(k)), 0), tolerance = 1e-12)
  expect_equal(r$dispersion$det, R - Q * c(0, 4, 8), tolerance = 1e-12)
  # no reinforcement: Q = -omega D < 0, stable at every wavelength, no critical mode
  s <- morie_hotspot_threshold(A0 = 1, omega = 1, eps = 0, D = 1, gamma = 1, eta = 0)
  expect_equal(s$Q, -1)
  expect_false(s$unstable)
  expect_true(is.na(s$k_crit) && is.na(s$det_crit) && is.na(s$growth_crit))
  expect_error(morie_hotspot_threshold(A0 = 1, omega = 1, eps = 1, D = 1, gamma = 1, eta = 0, k = c(1, -1)),
               "`k` must be non-negative wavenumbers-squared", fixed = TRUE)
})

test_that("the 1.4.3 ledger functions refuse what their help pages rule out", {
  cells <- c(n100 = 1, n010 = 1, n001 = 1, n110 = 1, n101 = 1, n011 = 1, n111 = -1)
  expect_error(morie_dark_figure_no_interaction(cells), "counts must be non-negative", fixed = TRUE)
  expect_error(morie_deterrence_logcost(c(1, 2), c(1, 2), 1),
               "`p`, `s` and `c` must have one value per observation", fixed = TRUE)
  expect_error(morie_deterrence_logcost(c(1, 2), c(1, 2), c(0, 1), y = 1),
               "`y` must have one value per observation", fixed = TRUE)
  for (w in list(c(1, 1), c(1, -1, 1), c(0, 0, 0))) {
    expect_error(morie_rif_quantile(c(1, 2, 3), 0.5, weights = w),
                 "`weights` must be non-negative with a positive sum", fixed = TRUE)
  }
  expect_error(morie_age_crime_type_cov(c(0.5, 0.5), c(1, 2), 3), "one weight and two rates per type", fixed = TRUE)
  expect_error(morie_feedback_loop_envelope(1, 1, 1, n = -1L), "`n` must be a non-negative horizon", fixed = TRUE)
  for (g in list(-1, NA_real_, c(1, 2), "1")) {
    expect_error(morie_spillover_ring_bound(c(0.1, 0.2), gamma_max = g),
                 "`gamma_max` must be a single non-negative bound", fixed = TRUE)
  }
  expect_error(morie_spillover_ring_bound(c(0.1, 0.2), gamma_max = 1, gamma = 0.5),
               "`gamma` must have one finite value per unit", fixed = TRUE)
  expect_error(morie_spillover_ring_bound(c(0.1, 0.2), gamma_max = 1, gamma = c(0.5, NA)),
               "`gamma` must have one finite value per unit", fixed = TRUE)
  expect_error(morie_dark_figure_comparable(VA = "1000", RA = 1, RB = 1),
               "`VA` must be a single finite number at least 0", fixed = TRUE)
  expect_error(morie_dark_figure_comparable(VA = 10, RA = 1, RB = Inf),
               "`RB` must be a single finite number at least 0", fixed = TRUE)
  expect_error(morie_dark_figure_comparable(VA = 10, RA = 0, RB = 3),
               "`RA` must be positive for the bound: with nothing recorded on A, lambda r_A = 0", fixed = TRUE)
})
