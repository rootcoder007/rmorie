# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/snmcox_native.R (g-estimation of a rank-preserving
# structural nested failure time model, Robins 1992). U(psi) =
# int_0^T exp(psi A(u)) du is worked out interval by interval, the
# score sum (A - E[A|L])(U - Ubar) is recomputed, and the estimate is
# checked to be the root of that score.

.sc_T <- c(5, 8, 3, 10, 6, 4, 9, 7, 2, 11, 6.5, 8.5)
.sc_A <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
.sc_L <- cbind(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0))

test_that("blip_down integrates exp(psi A(u)) over the treated intervals", {
  # never treated: U = T
  expect_equal(morie_snmcox_blip_down(5, list(), 0.4), 5)
  # treated throughout: U = T exp(psi)
  expect_equal(morie_snmcox_blip_down(5, list(c(0, 5)), 0.4), 5 * exp(0.4), tolerance = 1e-12)
  # treated for 2 of 5 units
  expect_equal(morie_snmcox_blip_down(5, list(c(1, 3)), 0.4), 3 + 2 * exp(0.4), tolerance = 1e-12)
  # an interval running past T is truncated
  expect_equal(morie_snmcox_blip_down(5, list(c(4, 99)), 0.4), 4 + 1 * exp(0.4), tolerance = 1e-12)
  # overlapping intervals are not double counted, and order does not matter
  expect_equal(morie_snmcox_blip_down(10, list(c(4, 8), c(1, 5)), 0.3),
               morie_snmcox_blip_down(10, list(c(1, 8)), 0.3), tolerance = 1e-12)
  expect_equal(morie_snmcox_blip_down(10, list(c(1, 8)), 0.3), 3 + 7 * exp(0.3), tolerance = 1e-12)
  # psi = 0 leaves the time alone
  expect_equal(morie_snmcox_blip_down(7, list(c(0, 4)), 0), 7)
  expect_error(morie_snmcox_blip_down(-1, list(), 0), "cannot be negative")
})

test_that("gest_score is sum (A - Ehat)(U - Ubar) over the events", {
  ev <- rep(1, 12)
  tt <- lapply(seq_len(12), function(i) if (.sc_A[i] > 0) list(c(0, .sc_T[i])) else list())
  psi <- 0.25
  r <- morie_snmcox_gest_score(psi, .sc_T, ev, .sc_A, .sc_L, tt)
  U <- vapply(seq_len(12), function(i) morie_snmcox_blip_down(.sc_T[i], tt[[i]], psi), 0)
  expect_equal(r$U, U, tolerance = 1e-12)
  # the treatment model is a logistic fit of A on L
  e <- plogis(cbind(1, .sc_L) %*% coef(glm(.sc_A ~ .sc_L, family = binomial(),
                                           control = list(epsilon = 1e-14, maxit = 200))))
  expect_equal(r$e, as.numeric(e), tolerance = 1e-5)
  terms <- (.sc_A - r$e) * (U - mean(U))
  expect_equal(r$s, sum(terms), tolerance = 1e-12)
  expect_equal(r$z, sum(terms) / sqrt(sum(terms^2)), tolerance = 1e-12)
  expect_equal(r$m, 12L)
  # with fewer than two events the score is empty
  few <- morie_snmcox_gest_score(psi, .sc_T, c(1, rep(0, 11)), .sc_A, .sc_L, tt)
  expect_equal(c(few$s, few$z, few$m), c(0, 0, 0))
  # a constant treatment falls back to the linear model
  lin <- morie_snmcox_gest_score(psi, .sc_T, ev, rep(1, 12), .sc_L, tt)
  expect_true(all(is.finite(lin$e)))
})

test_that("morie_snmcox solves the score equation and inverts the score test", {
  ev <- rep(1, 12)
  for (fn in list(morie_snmcox, morie_snm_cox)) {
    f <- fn(.sc_T, ev, .sc_A, .sc_L, n_grid = 61L)
    expect_true(f$converged)
    expect_equal(f$score_at_estimate, 0, tolerance = 1e-6)
    expect_equal(morie_snmcox_gest_score(f$psi, .sc_T, ev, .sc_A, .sc_L,
                                         lapply(seq_len(12), function(i)
                                           if (.sc_A[i] > 0) list(c(0, .sc_T[i])) else list()))$s,
                 0, tolerance = 1e-6)
    expect_equal(f$time_ratio, exp(f$psi), tolerance = 1e-12)
    expect_lte(f$lower, f$psi)
    expect_gte(f$upper, f$psi)
    expect_equal(f$n, 12L)
    expect_equal(f$artificial_censored, 0L)
    expect_length(f$grid_psi, 61L)
    # the score changes sign across the root
    expect_lt(min(f$grid_score) * max(f$grid_score), 0)
  }
  # explicit treatment histories are used as given
  tt <- lapply(seq_len(12), function(i) if (.sc_A[i] > 0) list(c(1, 3)) else list())
  h <- morie_snmcox(.sc_T, ev, .sc_A, .sc_L, treat_times = tt, n_grid = 41L)
  expect_equal(h$blipped[1], morie_snmcox_blip_down(.sc_T[1], tt[[1]], h$psi), tolerance = 1e-12)
  expect_error(morie_snmcox(numeric(0), numeric(0), numeric(0)), "no subjects")
  expect_error(morie_snmcox(.sc_T, ev[-1], .sc_A), "12 times but 11 event")
  expect_error(morie_snmcox(.sc_T, rep(2, 12), .sc_A), "event must be 0/1")
  expect_error(morie_snmcox(.sc_T, ev, .sc_A[-1]), "12 times but 11 treatment values")
  expect_error(morie_snmcox(.sc_T, ev, .sc_A, treat_times = list()), "12 times but 0 treatment histories")
  expect_error(morie_snmcox(.sc_T, ev, .sc_A, censor_time = 9), "artificial-censoring construction")
  expect_error(morie_snmcox(.sc_T, ev, .sc_A, psi_range = c(1, -1)), "must be increasing")
})

test_that("morie_snmcox_cheatsheet states the blip-down", {
  expect_match(morie_snmcox_cheatsheet(), "U(psi) = int_0^T exp(psi A(u)) du", fixed = TRUE)
})
