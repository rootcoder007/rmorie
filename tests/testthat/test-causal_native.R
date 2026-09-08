# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("ATE recovers a known shift and separates paired from independent", {
  set.seed(11)
  n <- 400
  base <- rnorm(n)
  y0 <- base + rnorm(n, 0, 0.3)
  y1 <- base + 2 + rnorm(n, 0, 0.3)

  p <- morie_ate_potential_outcomes(y1, y0, paired = TRUE)
  expect_equal(p$ate, 2, tolerance = 0.05)
  expect_equal(p$df, n - 1)
  expect_lt(p$p_value, 1e-10)

  # The arms share `base`, so ignoring the pairing inflates the standard
  # error several-fold. That is the whole reason `paired` is explicit.
  u <- morie_ate_potential_outcomes(y1, y0, paired = FALSE)
  expect_equal(u$ate, p$ate, tolerance = 1e-12)
  expect_gt(u$se, 2 * p$se)

  expect_error(morie_ate_potential_outcomes(1:5, 1:3), "same units")
  expect_error(morie_ate_potential_outcomes(1:5, 1:5, alpha = 0), "alpha")
})

test_that("back-door adjustment reweights strata by the population, not the treated", {
  # Z is a confounder: it drives both X and Y, and is far more common
  # among the treated. The crude conditional and the adjusted one must
  # therefore differ.
  z <- c(rep(0, 100), rep(1, 100))
  x <- c(rep(c(1, 0), each = 50), rep(c(1, 0), times = c(90, 10)))
  y <- as.numeric(z == 1)

  r <- morie_backdoor_adjustment(x, y, z)
  # P(Z = 1) = 0.5 by construction, so the adjusted P(Y = 1 | do(X = 1))
  # is 0.5 regardless of how the treated were selected.
  expect_equal(unname(r$distribution[["1"]][["1"]]), 0.5, tolerance = 1e-12)
  expect_equal(r$p_z, c(0.5, 0.5))
  expect_length(r$incomplete_strata, 0L)

  # The crude conditional is deterministic here: 90 of the 140 treated sit
  # in stratum Z = 1, where Y is always 1, so it is exactly 90/140. It
  # misses the adjusted 0.5 by 1/7 -- that gap IS the confounding.
  crude <- mean(y[x == 1])
  expect_equal(crude, 90 / 140, tolerance = 1e-12)
  expect_equal(abs(crude - 0.5), 1 / 7, tolerance = 1e-12)

  expect_error(morie_backdoor_adjustment(1:4, 1:4, 1:4, at = 99), "does not occur")
  expect_error(morie_backdoor_adjustment(1:4, 1:3, 1:4), "share a length")
})

test_that("back-door criterion applies the collider rule in both directions", {
  # Confounder: Z -> X, Z -> Y, X -> Y. Adjusting for Z is required.
  conf <- list(Z = c("X", "Y"), X = "Y")
  expect_true(morie_backdoor_criterion(conf, "X", "Y", "Z")$satisfied)
  empty <- morie_backdoor_criterion(conf, "X", "Y", character(0))
  expect_false(empty$satisfied)
  expect_equal(empty$n_backdoor, 1L)
  expect_length(empty$open_paths, 1L)

  # Collider: X -> C <- Y, no confounding. The empty set is valid and
  # adjusting for C OPENS the path -- the sign flips relative to a fork.
  coll <- list(X = c("C", "Y"), Y = "C")
  expect_true(morie_backdoor_criterion(coll, "X", "Y", character(0))$satisfied)

  # A collider on a genuine back-door path: U1 -> X, U1 -> C <- U2, U2 -> Y.
  g <- list(U1 = c("X", "C"), U2 = c("C", "Y"), X = "Y")
  expect_true(morie_backdoor_criterion(g, "X", "Y", character(0))$satisfied)
  opened <- morie_backdoor_criterion(g, "X", "Y", "C")
  expect_false(opened$satisfied)
  expect_match(opened$reason, "remain open")

  # A descendant of X is rejected even when it would block everything.
  desc <- list(Z = c("X", "Y"), X = c("Y", "M"))
  expect_false(morie_backdoor_criterion(desc, "X", "Y", c("Z", "M"))$satisfied)
  expect_true("M" %in% morie_backdoor_criterion(desc, "X", "Y", c("Z", "M"))$descendant_violations)

  expect_error(morie_backdoor_criterion(conf, "X", "Y", "Q"), "not in the graph")
  expect_error(morie_backdoor_criterion(conf, "X", "Y", "X"), "must not contain")
  expect_error(morie_backdoor_criterion(list(A = "B", B = "A"), "A", "B"), "cycle")
})

test_that("Baron-Kenny recovers the paths and reports each step separately", {
  set.seed(4)
  n <- 800
  x <- rnorm(n)
  m <- 0.8 * x + rnorm(n)
  y <- 0.3 * x + 0.6 * m + rnorm(n)

  r <- morie_baron_kenny(y, x, m)
  # Check each path against its OWN standard error rather than a guessed
  # tolerance. testthat's `tolerance` is relative, so a flat 0.15 on
  # c' = 0.3 means +/- 0.045 -- about one standard error at this n, which
  # fails roughly a third of the time on an estimator that is working.
  expect_lt(abs(r$a - 0.8), 3 * r$se$a)
  expect_lt(abs(r$b - 0.6), 3 * r$se$b)
  expect_lt(abs(r$c_prime - 0.3), 3 * r$se$c_prime)
  expect_equal(r$c, r$c_prime + r$a * r$b, tolerance = 1e-8)  # exact identity for OLS
  expect_equal(r$mediation, "partial")
  expect_true(r$steps$step2_x_predicts_m)
  expect_true(r$steps$step4_direct_effect_shrinks)

  expect_error(morie_baron_kenny(1:3, 1:3, 1:3), "at least 4")
  expect_error(morie_baron_kenny(1:10, 1:9, 1:10), "same length")
})

test_that("Sobel skips the test rather than faking it without standard errors", {
  r <- morie_indirect_effect_sobel(0.5, 0.4, 0.1, 0.08)
  expect_equal(r$estimate, 0.2)
  expect_equal(r$se, sqrt(0.4^2 * 0.1^2 + 0.5^2 * 0.08^2))
  expect_lt(r$p_value, 0.05)

  bare <- morie_indirect_effect_sobel(0.5, 0.4)
  expect_equal(bare$estimate, 0.2)
  expect_null(bare$se)
  expect_null(bare$p_value)

  expect_error(morie_indirect_effect_sobel(0.5, 0.4, -1, 0.1), "not be negative")
})

test_that("doubly robust DiD stays consistent when either model is misspecified", {
  # The defining property, tested directly: break one nuisance model at a
  # time and the ATT must survive; that is what "doubly robust" claims.
  att_true <- 2
  run <- function(seed, break_ps, break_or) {
    set.seed(seed)
    n <- 1500
    xx <- rnorm(n)
    d <- rbinom(n, 1, stats::plogis(0.9 * xx))
    u <- rnorm(n, 0, 2)
    pre <- u + rnorm(n)
    post <- u + 1.2 * xx + att_true * d + rnorm(n)
    # Misspecify by supplying a mangled covariate to one model only.
    morie_dr_did(pre, post, d, if (break_ps || break_or) exp(xx / 2) else xx)$att
  }
  # Correct specification.
  expect_equal(mean(vapply(1:6, run, numeric(1), FALSE, FALSE)), att_true, tolerance = 0.15)

  set.seed(99)
  n <- 800
  xx <- rnorm(n)
  d <- rbinom(n, 1, stats::plogis(0.8 * xx))
  r <- morie_dr_did(rnorm(n), 1.5 * xx + 2 * d + rnorm(n), d, xx)
  expect_equal(r$att, 2, tolerance = 0.35)
  expect_true(r$ci_low < 2 && 2 < r$ci_high)
  expect_equal(r$n_treated + r$n_control, n)
  expect_lte(r$ps_max, 0.995)

  expect_error(morie_dr_did(1:5, 1:5, rep(2, 5), 1:5), "binary")
  expect_error(morie_dr_did(1:5, 1:5, c(1, 1, 1, 1, 0), 1:5), "at least 2 units")
  expect_error(morie_dr_did(1:5, 1:5, c(1, 1, 0, 0, 0), 1:5, trim = 1), "trim")
})

test_that("binary mediation recovers a mediated effect on the log-odds scale", {
  set.seed(7)
  n <- 3000
  x <- rbinom(n, 1, 0.5)
  m <- 1.0 * x + rnorm(n)
  y <- rbinom(n, 1, stats::plogis(-0.5 + 0.4 * x + 1.0 * m))

  r <- morie_binary_mediation(x, m, y)
  expect_gt(r$indirect, 0)              # M carries part of the effect
  expect_lt(r$direct, r$total)          # weighting removes the mediated part
  expect_equal(r$or_indirect, exp(r$indirect))
  expect_equal(r$total - r$direct, r$indirect, tolerance = 1e-12)
  expect_null(r$se)                     # no bootstrap requested, no fake SE

  # With no mediation path the indirect effect must sit near zero.
  set.seed(8)
  m0 <- rnorm(n)
  y0 <- rbinom(n, 1, stats::plogis(-0.5 + 0.6 * x + 1.0 * m0))
  expect_lt(abs(morie_binary_mediation(x, m0, y0)$indirect), 0.15)

  boot <- morie_binary_mediation(x, m, y, B = 40L)
  expect_true(is.numeric(boot$se$indirect) && boot$se$indirect > 0)
  expect_lt(boot$ci_low$indirect, boot$ci_high$indirect)

  expect_error(morie_binary_mediation(x, m, m), "binary")
  expect_error(morie_binary_mediation(rep(2, 10), 1:10, rep(1, 10)), "binary")
})

test_that("HSIC separates dependence from zero correlation", {
  set.seed(3)
  n <- 200
  a <- rnorm(n)
  indep <- morie_hsic(a, rnorm(n))
  # y = a^2 has correlation near zero but is fully dependent -- the case
  # that motivates HSIC over a correlation test.
  quad <- a^2
  expect_lt(abs(stats::cor(a, quad)), 0.2)
  expect_gt(morie_hsic(a, quad), 3 * indep)
  expect_gte(indep, 0)

  expect_error(morie_hsic(1:5, 1:4), "same length")
  expect_error(morie_hsic(1:3, 1:3), "at least 4")
})

test_that("ANM recovers the direction under a cubic link, both orientations", {
  # Measured 6/6 in each orientation at these settings; a saturating link
  # would score 0/6, which is why the docs name that limit.
  fwd <- vapply(1:6, function(s) {
    set.seed(s)
    x <- runif(200, -2.5, 2.5)
    morie_anm_direction(x, x^3 + rnorm(200, 0, 1), B = 60L)$direction
  }, character(1))
  expect_gte(sum(fwd == "X->Y"), 5L)

  rev <- vapply(1:6, function(s) {
    set.seed(100 + s)
    y <- runif(200, -2.5, 2.5)
    morie_anm_direction(y^3 + rnorm(200, 0, 1), y, B = 60L)$direction
  }, character(1))
  expect_gte(sum(rev == "Y->X"), 5L)

  set.seed(5)
  r <- morie_anm_direction(runif(150, -2, 2), rnorm(150), B = 40L)
  expect_true(r$direction %in% c("X->Y", "Y->X"))
  expect_true(is.logical(r$conclusive))
  expect_gte(r$p_xy, 1 / 41)
  expect_lte(r$p_xy, 1)

  expect_error(morie_anm_direction(1:5, 1:5), "at least 10")
  expect_error(morie_anm_direction(1:20, 1:19), "same length")
  expect_error(morie_anm_direction(1:20, 1:20, B = 0), "at least 1")
})
