# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tmlcmp_native.R (targeted cumulative incidence under
# competing risks). Cause-specific hazards are counted by hand, the
# Aalen-Johansen recursion F_j = sum S(u-) lambda_j(u) is recomputed,
# the F + S closure is checked to be one, and 1 - KM is shown to
# overstate F_j exactly as the note says.

.cr_t <- c(1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5)
.cr_e <- c(1, 2, 1, 1, 0, 2, 1, 0, 2, 1, 1, 0)
.cr_a <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
.cr_W <- cbind(c(0.5, -0.5, 1, 0, -1, 0.25, 0.75, -0.25, 0.1, -0.1, 0.9, -0.9))
.cr_grid <- 1:5

.cr_haz <- function(cause, w = rep(1, 12), keep = seq_len(12)) {
  vapply(.cr_grid, function(u) {
    risk <- sum(w[keep][.cr_t[keep] >= u])
    ev <- sum(w[keep][.cr_t[keep] == u & .cr_e[keep] == cause])
    if (risk > 1e-12) ev / risk else 0
  }, 0)
}

test_that("cause_specific_hazards counts events of each type over those at risk", {
  h <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid)
  expect_equal(h$types, c(1, 2))
  expect_equal(h$n, 12L)
  expect_equal(h$hazards[["1"]], .cr_haz(1), tolerance = 1e-12)
  expect_equal(h$hazards[["2"]], .cr_haz(2), tolerance = 1e-12)
  # at t = 1: 12 at risk, one cause-1 event and one cause-2 event
  expect_equal(h$hazards[["1"]][1], 1 / 12)
  expect_equal(h$hazards[["2"]][1], 1 / 12)
  # restricted to one arm
  a1 <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid, A = .cr_a, arm = 1)
  keep <- which(.cr_a == 1)
  expect_equal(a1$hazards[["1"]], .cr_haz(1, keep = keep), tolerance = 1e-12)
  expect_equal(a1$n, 6L)
  # weights scale both the numerator and the risk set
  w <- seq(0.5, 3, length.out = 12)
  hw <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid, weights = w)
  expect_equal(hw$hazards[["1"]], .cr_haz(1, w), tolerance = 1e-12)
  expect_equal(cause_specific_hazards(.cr_t, .cr_e, .cr_grid, weights = rep(2, 12))$hazards[["1"]],
               .cr_haz(1), tolerance = 1e-12)
  expect_error(cause_specific_hazards(.cr_t, .cr_e[-1], .cr_grid), "length mismatch")
  expect_error(cause_specific_hazards(.cr_t, .cr_e, .cr_grid, weights = 1), "weights length mismatch")
  expect_error(cause_specific_hazards(.cr_t, .cr_e, .cr_grid, A = .cr_a, arm = 7), "no subjects in arm")
  expect_error(cause_specific_hazards(.cr_t, rep(0, 12), .cr_grid), "no events of any type")
})

test_that("cumulative_incidence is the Aalen-Johansen recursion and closes with S", {
  h <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid)
  ci <- cumulative_incidence(h$hazards, .cr_grid)
  h1 <- .cr_haz(1)
  h2 <- .cr_haz(2)
  S <- 1
  F1 <- F2 <- numeric(5)
  Sv <- numeric(5)
  for (k in 1:5) {
    F1[k] <- (if (k > 1) F1[k - 1] else 0) + S * h1[k]
    F2[k] <- (if (k > 1) F2[k - 1] else 0) + S * h2[k]
    S <- S * (1 - h1[k] - h2[k])
    Sv[k] <- S
  }
  expect_equal(ci$F[["1"]], F1, tolerance = 1e-12)
  expect_equal(ci$F[["2"]], F2, tolerance = 1e-12)
  expect_equal(ci$survival, Sv, tolerance = 1e-12)
  expect_equal(ci$closure, rep(1, 5), tolerance = 1e-12)
  expect_equal(ci$types, c(1, 2))
  # raising the competing hazard lowers F_1 without touching lambda_1
  h_more <- h$hazards
  h_more[["2"]] <- pmin(h_more[["2"]] * 3, 1)
  ci2 <- cumulative_incidence(h_more, .cr_grid)
  expect_lt(ci2$F[["1"]][5], ci$F[["1"]][5])
  expect_equal(cumulative_incidence(list("1" = c(0.5, 0.5)), 1:2)$F[["1"]], c(0.5, 0.75),
               tolerance = 1e-12)
  expect_error(cumulative_incidence(list(), .cr_grid), "no hazards given")
})

test_that("one_minus_km treats competing events as censoring and overstates F_j", {
  h <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid)
  km <- one_minus_km(h$hazards, .cr_grid, 1)
  h1 <- .cr_haz(1)
  expect_equal(km$estimate, 1 - cumprod(1 - h1), tolerance = 1e-12)
  ci <- cumulative_incidence(h$hazards, .cr_grid)
  expect_gt(km$estimate[5], ci$F[["1"]][5])
  expect_match(km$caveat, "overstates F_j", fixed = TRUE)
  expect_error(one_minus_km(h$hazards, .cr_grid, 3), "cause 3 has no hazard")
})

test_that("morie_tmlcmp contrasts the weighted incidences at the horizon", {
  r <- morie_tmlcmp(.cr_t, .cr_e, .cr_a, .cr_W, times = .cr_grid, cause = 1, horizon = 4,
                    g = rep(0.5, 12))
  # arm 1 with weights 1{A = a} / g: the hazards double the treated counts
  h1 <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid, .cr_a, 1, ifelse(.cr_a == 1, 1, 0) / 0.5)
  c1 <- cumulative_incidence(h1$hazards, .cr_grid)
  h0 <- cause_specific_hazards(.cr_t, .cr_e, .cr_grid, .cr_a, 0, ifelse(.cr_a == 0, 1, 0) / 0.5)
  c0 <- cumulative_incidence(h0$hazards, .cr_grid)
  expect_equal(r$F_treated, c1$F[["1"]][4], tolerance = 1e-12)
  expect_equal(r$F_control, c0$F[["1"]][4], tolerance = 1e-12)
  expect_equal(r$psi, r$F_treated - r$F_control, tolerance = 1e-12)
  # the influence-curve standard error, recomputed
  hit <- as.numeric(.cr_t <= 4 & .cr_e == 1)
  d <- (.cr_a / 0.5) * (hit - r$F_treated) - ((1 - .cr_a) / 0.5) * (hit - r$F_control)
  expect_equal(r$se, sqrt(sum((d - mean(d))^2)) / 12, tolerance = 1e-12)
  expect_equal(r$ci, r$psi + c(-1.96, 1.96) * r$se, tolerance = 1e-12)
  expect_equal(r$horizon, 4)
  expect_equal(r$closure_treated, rep(1, 5), tolerance = 1e-12)
  # the propensity is fitted from X when none is supplied
  f <- morie_tmlcmp(.cr_t, .cr_e, .cr_a, .cr_W, times = .cr_grid)
  expect_true(is.finite(f$psi))
  expect_equal(f$horizon, 5)
  expect_equal(tmlecompetingrisks(.cr_t, .cr_e, .cr_a, .cr_W, times = .cr_grid)$psi, f$psi)
  expect_error(morie_tmlcmp(.cr_t, .cr_e[-1], .cr_a, .cr_W), "differ in length")
  expect_error(morie_tmlcmp(.cr_t, .cr_e, .cr_a, .cr_W, times = .cr_grid, horizon = 0.5),
               "horizon precedes all grid points")
})
