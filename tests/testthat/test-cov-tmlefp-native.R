# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tmlefp_native.R (Crump, Hotz, Imbens & Mitnik 2009
# optimal overlap). Expected values come from the defining equations:
# gamma = 2 E[k | k < gamma], 1 / (alpha (1 - alpha)) = gamma, the
# normalised IPW contrast and omega = e (1 - e).

.tf_e <- c(0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.97)
.tf_w <- c(0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L)
.tf_y <- c(1.2, 0.4, 2.5, 0.9, 3.1, 1.7, 2.2, 3.4, 1.1, 2.9, 3.8, 4.4)

.tf_ipw <- function(y, w, e, sel, om = rep(1, length(y))) {
  t1 <- sum((om * w * y / e)[sel]) / sum((om * w / e)[sel])
  t0 <- sum((om * (1 - w) * y / (1 - e))[sel]) / sum((om * (1 - w) / (1 - e))[sel])
  t1 - t0
}

test_that("alpha_from_gamma inverts 1/(alpha(1-alpha)) = gamma on (0, 1/2]", {
  for (fn in list(alpha_from_gamma, morie_alpha_from_gamma)) {
    for (g in c(4, 5, 10, 57.3)) {
      a <- fn(g)
      expect_equal(1 / (a * (1 - a)), g, tolerance = 1e-12)
      expect_lte(a, 0.5)
      expect_equal(a, (1 - sqrt(1 - 4 / g)) / 2, tolerance = 1e-12)
    }
    expect_error(fn(3.9), "at least 4")
  }
})

test_that("optimal_alpha finds the Theorem 5.2 fixed point", {
  for (fn in list(optimal_alpha, morie_optimal_alpha)) {
    r <- fn(.tf_e)
    k <- 1 / (.tf_e * (1 - .tf_e))
    expect_equal(r$k, k, tolerance = 1e-12)
    expect_false(r$no_trimming)
    expect_equal(r$gamma, 2 * mean(k[k < r$gamma]), tolerance = 1e-10)
    expect_equal(1 / (r$alpha * (1 - r$alpha)), r$gamma, tolerance = 1e-10)
    expect_identical(r$keep, k <= r$gamma)
    # homoskedastic rule is alpha <= e <= 1 - alpha (Corollary 5.1)
    expect_identical(r$keep, .tf_e >= r$alpha & .tf_e <= 1 - r$alpha)
    expect_equal(r$trim, sum(!r$keep))
    # no trimming when sup k <= 2 E[k]
    e2 <- c(0.3, 0.4, 0.5, 0.6, 0.7)
    r2 <- fn(e2)
    expect_true(r2$no_trimming)
    expect_equal(r2$alpha, 0)
    expect_true(all(r2$keep))
    # heteroskedastic k = s1/e + s0/(1-e); alpha undefined
    s1 <- seq(0.5, 2, length.out = 12)
    s0 <- rev(s1)
    rh <- fn(.tf_e, s1, s0)
    kh <- s1 / .tf_e + s0 / (1 - .tf_e)
    expect_equal(rh$k, kh, tolerance = 1e-12)
    expect_true(is.nan(rh$alpha))
    expect_equal(rh$gamma, 2 * mean(kh[kh < rh$gamma]), tolerance = 1e-10)
    # one variance supplied, the other defaults to 1
    r1 <- fn(.tf_e, sigma2_treated = s1)
    expect_equal(r1$k, s1 / .tf_e + 1 / (1 - .tf_e), tolerance = 1e-12)
    expect_error(fn(numeric(0)), "no propensity")
    expect_error(fn(c(0, 0.5)), "strictly")
    expect_error(fn(.tf_e, s1[1:3], s0), "one conditional")
    expect_error(fn(.tf_e, -s1, s0), "positive")
  }
})

test_that("optimal_alpha_att solves the one-sided Theorem 5.3 rule", {
  for (fn in list(optimal_alpha_att, morie_optimal_alpha_att)) {
    r <- fn(.tf_e, .tf_w)
    g <- 1 / (1 - .tf_e[.tf_w == 1L])
    thr <- 1 / (1 - r$alpha_t)
    expect_equal(thr, 2 * mean(g[g < thr]), tolerance = 1e-10)
    expect_identical(r$keep, .tf_e <= r$alpha_t)
    expect_equal(r$trim, sum(.tf_e > r$alpha_t))
    expect_false(r$no_trimming)
    rn <- fn(c(0.2, 0.3, 0.4), c(1L, 0L, 1L))
    expect_true(rn$no_trimming)
    expect_equal(rn$alpha_t, 1)
    expect_error(fn(.tf_e, .tf_w[-1]), "one treatment")
    expect_error(fn(.tf_e, rep(0L, 12)), "no treated")
    expect_error(fn(c(1, 0.5), c(1L, 0L)), "strictly")
  }
})

test_that("owate_weights are e(1-e) or the inverse heteroskedastic k", {
  s1 <- seq(0.5, 2, length.out = 12)
  for (fn in list(owate_weights, morie_owate_weights)) {
    expect_equal(fn(.tf_e), .tf_e * (1 - .tf_e), tolerance = 1e-12)
    expect_equal(fn(.tf_e, s1, 2), 1 / (s1 / .tf_e + 2 / (1 - .tf_e)), tolerance = 1e-12)
    expect_equal(fn(.tf_e, sigma2_control = s1), 1 / (1 / .tf_e + s1 / (1 - .tf_e)), tolerance = 1e-12)
    expect_error(fn(c(0.5, 1)), "strictly")
  }
})

test_that("morie_tmlefp and its aliases report the OSATE, ATE and OWATE", {
  for (fn in list(morie_tmlefp, morie_optimal_overlap, morie_tmle_effective_pi)) {
    r <- fn(.tf_y, .tf_w, .tf_e)
    rule <- optimal_alpha(.tf_e)
    sel <- rule$keep
    expect_equal(r$osate, .tf_ipw(.tf_y, .tf_w, .tf_e, sel), tolerance = 1e-12)
    expect_equal(r$estimate, r$osate)
    expect_equal(r$ate_full, .tf_ipw(.tf_y, .tf_w, .tf_e, rep(TRUE, 12)), tolerance = 1e-12)
    om <- .tf_e * (1 - .tf_e)
    expect_equal(r$owate, .tf_ipw(.tf_y, .tf_w, .tf_e, rep(TRUE, 12), om), tolerance = 1e-12)
    expect_equal(r$n_kept, sum(sel))
    expect_equal(r$n_trimmed, 12 - sum(sel))
    k <- 1 / (.tf_e * (1 - .tf_e))
    expect_equal(r$variance_bound, mean(k[sel]) / (sum(sel) / 12), tolerance = 1e-12)
    expect_equal(r$variance_bound_full, mean(k), tolerance = 1e-12)
    expect_lt(r$variance_bound, r$variance_bound_full)
    ra <- fn(.tf_y, .tf_w, .tf_e, estimand = "att")
    at <- optimal_alpha_att(.tf_e, .tf_w)
    expect_equal(ra$alpha, at$alpha_t)
    expect_true(is.nan(ra$gamma))
    expect_equal(ra$estimate, .tf_ipw(.tf_y, .tf_w, .tf_e, at$keep), tolerance = 1e-12)
    s1 <- seq(0.5, 2, length.out = 12)
    rh <- fn(.tf_y, .tf_w, .tf_e, sigma2_treated = s1, sigma2_control = rep(1, 12))
    expect_equal(rh$owate_weights, 1 / (s1 / .tf_e + 1 / (1 - .tf_e)), tolerance = 1e-12)
    expect_error(fn(.tf_y[-1], .tf_w, .tf_e), "same length")
    expect_error(fn(.tf_y, .tf_w + 1L, .tf_e), "0 or 1")
    expect_error(fn(.tf_y, .tf_w, .tf_e, estimand = "atc"), "estimand")
  }
})

test_that("morie_tmlefp_cheatsheet names the rule", {
  s <- morie_tmlefp_cheatsheet()
  expect_type(s, "character")
  expect_length(s, 1L)
  expect_match(s, "Crump, Hotz, Imbens & Mitnik 2009", fixed = TRUE)
  expect_match(s, "e(1-e)", fixed = TRUE)
})
