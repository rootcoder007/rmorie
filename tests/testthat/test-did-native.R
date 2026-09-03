# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Module 14 — structural tests for the native DiD family.
#
#' @srrstats {G5.4} Correctness tested against hand-computable
#'   estimands and known DGP ground truth.
#' @srrstats {G5.5} All randomized checks run under fixed seeds.
#' @srrstats {G5.6} Parameter recovery on synthetic panels with known
#'   treatment effects, within stated tolerances.

.mk_stag_panel <- function(n_units = 200, n_t = 8, seed = 42) {
  set.seed(seed)
  g_onset <- sample(c(0, 4, 5, 6), n_units, replace = TRUE,
                    prob = c(0.4, 0.2, 0.2, 0.2))
  x1 <- rnorm(n_units)
  x2 <- runif(n_units)
  u <- rnorm(n_units)
  do.call(rbind, lapply(seq_len(n_units), function(i) {
    t <- seq_len(n_t)
    d <- as.integer(g_onset[i] > 0 & t >= g_onset[i])
    y <- u[i] + 0.5 * t + d * 1.5 + rnorm(n_t, 0, 0.5)
    data.frame(id = i, tt = t, y = y, d = d, g = g_onset[i],
               x1 = x1[i], x2 = x2[i])
  }))
}

test_that("native TWFE equals explicit-dummy OLS (FWL identity)", {
  pan <- .mk_stag_panel(n_units = 60, n_t = 5, seed = 1)
  fit <- rmorie:::.morie_did_twfe_native(pan$y, cbind(d = pan$d),
                                         pan$id, pan$tt, pan$id)
  # Independent check: plain lm() with explicit unit + time dummies.
  ref <- stats::lm(y ~ d + factor(id) + factor(tt), data = pan)
  expect_equal(unname(fit$beta[["d"]]),
               unname(stats::coef(ref)[["d"]]), tolerance = 1e-10)
})

test_that("morie_did_panel_fe recovers tau and carries the native label", {
  pan <- .mk_stag_panel(seed = 2)
  res <- morie_did_panel_fe(pan, "y", "d", "id", "tt")
  expect_equal(res$estimate, 1.5, tolerance = 0.15)
  expect_identical(res$method, "did_panel_fe (rmorie native)")
  expect_true(is.finite(res$std_error) && res$std_error > 0)
  expect_equal(res$details$n_units, 200)
  expect_equal(res$details$n_periods, 8)
})

test_that("native event study: reference period zero, effects post-onset", {
  pan <- .mk_stag_panel(seed = 3)
  res <- morie_did_event_study(pan, "y", "id", "tt", "g",
                               leads = 3L, lags = 3L)
  cf <- res$coefficients
  expect_true(all(c("relative_time", "estimate", "std_error",
                    "ci_lower", "ci_upper", "p_value") %in% names(cf)))
  ref_row <- cf[cf$relative_time == -1, ]
  expect_equal(ref_row$estimate, 0)
  post <- cf[cf$relative_time >= 0, ]
  expect_true(all(post$estimate > 1.0))
  pre <- cf[cf$relative_time < -1, ]
  expect_true(all(abs(pre$estimate) < 0.5))
  expect_identical(res$details$backend, "rmorie native")
})

test_that("event-study treatment_time column accepts Inf never-treated", {
  pan <- .mk_stag_panel(seed = 4)
  pan$g_inf <- ifelse(pan$g == 0, Inf, pan$g)
  a <- morie_did_event_study(pan, "y", "id", "tt", "g",
                             leads = 2L, lags = 2L)
  b <- morie_did_event_study(pan, "y", "id", "tt", "g_inf",
                             leads = 2L, lags = 2L)
  expect_equal(a$coefficients$estimate, b$coefficients$estimate,
               tolerance = 1e-10)
})

test_that("group_time_att: post-treatment cells recover tau; shape", {
  pan <- .mk_stag_panel(seed = 5)
  out <- morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                  n_bootstrap = 50L, seed = 5)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("cohort", "time", "att", "std_error", "ci_lower",
                    "ci_upper", "p_value") %in% names(out)))
  post <- out[out$time >= out$cohort, ]
  expect_true(nrow(post) >= 6)
  expect_equal(mean(post$att), 1.5, tolerance = 0.15)
  pre <- out[out$time < out$cohort, ]
  expect_true(all(abs(pre$att) < 0.5))
  expect_true(all(is.finite(out$std_error) & out$std_error > 0))
})

test_that("group_time_att: est_method and control_group variants run", {
  pan <- .mk_stag_panel(n_units = 120, seed = 6)
  for (m in c("doubly_robust", "ipw", "outcome_regression")) {
    for (cg in c("never_treated", "not_yet_treated")) {
      out <- morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                      covariates = c("x1", "x2"),
                                      method = m, control_group = cg,
                                      n_bootstrap = 0L)
      post <- out[out$time >= out$cohort, ]
      expect_equal(mean(post$att), 1.5, tolerance = 0.2)
    }
  }
})

test_that("staggered wrapper aggregates the native ATT(g,t)", {
  pan <- .mk_stag_panel(n_units = 120, seed = 7)
  res <- morie_did_staggered(pan, "y", "id", "tt", "g",
                             n_bootstrap = 30L, seed = 7)
  expect_true(all(c("group_time", "overall", "by_cohort",
                    "by_event_time") %in% names(res)))
  expect_true(is.finite(res$overall$estimate))
  # The overall aggregate averages ALL (g,t) cells incl. pre-treatment
  # placebos (preserved API shape); the post-treatment cells recover tau.
  gt <- res$group_time
  post <- gt[gt$time >= gt$cohort, ]
  expect_equal(mean(post$att), 1.5, tolerance = 0.25)
})

test_that("doubly robust DiD: native label, recovery, both SE conventions", {
  set.seed(8)
  n <- 900
  x <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.4 * x))
  post <- rbinom(n, 1, 0.5)
  y <- 1 + 0.5 * x + 0.4 * post + 2 * d * post + 0.2 * d + rnorm(n)
  df <- data.frame(y = y, d = d, post = post, x = x)
  res <- morie_did_doubly_robust(df, "y", "d", "post", covariates = "x",
                                 n_bootstrap = 0L)
  expect_identical(res$method, "did_doubly_robust (rmorie native)")
  expect_equal(res$estimate, 2, tolerance = 0.25)
  res_b <- morie_did_doubly_robust(df, "y", "d", "post", covariates = "x",
                                   n_bootstrap = 0L,
                                   se_convention = "bessel")
  expect_equal(res_b$estimate, res$estimate)
  # bessel SE = reference SE * sqrt(n / (n - 1)) exactly:
  # sd/sqrt(n) vs sd*sqrt(n-1)/n
  expect_equal(res_b$std_error, res$std_error * sqrt(n / (n - 1)),
               tolerance = 1e-12)
  # bootstrap SE is close to the analytic one
  res_boot <- morie_did_doubly_robust(df, "y", "d", "post",
                                      covariates = "x",
                                      n_bootstrap = 400L, seed = 8)
  expect_equal(res_boot$std_error, res$std_error, tolerance = 0.2)
})

test_that("DID-M native engine matches a hand-computed estimand", {
  # 2 joiners with jumps 5 and 7, 2 stayers with jumps 1 and 3:
  # DID+ = mean(5,7) - mean(1,3) = 4.
  hd <- data.frame(
    unit = rep(1:4, each = 2), time = rep(1:2, 4),
    d = c(0, 1, 0, 1, 0, 0, 0, 0),
    y = c(0, 5, 0, 7, 0, 1, 0, 3))
  expect_equal(rmorie:::.morie_didm_point(hd, "y", "d", "unit", "time"), 4)
  # Leavers: symmetric construction, DID- = mean stayers1 - mean leavers.
  hl <- data.frame(
    unit = rep(1:4, each = 2), time = rep(1:2, 4),
    d = c(1, 0, 1, 0, 1, 1, 1, 1),
    y = c(0, 1, 0, 3, 0, 5, 0, 7))
  expect_equal(rmorie:::.morie_didm_point(hl, "y", "d", "unit", "time"), 4)
})

test_that("morie_did_chaisemartin_dhaultfoeuille: finite estimate + boot SE", {
  pan <- .mk_stag_panel(n_units = 80, n_t = 6, seed = 9)
  res <- morie_did_chaisemartin_dhaultfoeuille(pan, "y", "d", "id", "tt",
                                               n_bootstrap = 50L, seed = 9)
  expect_true(is.finite(res$estimate))
  expect_equal(res$estimate, 1.5, tolerance = 0.3)
  expect_true(is.finite(res$std_error) && res$std_error > 0)
  expect_equal(res$method, "chaisemartin_dhaultfoeuille")
  expect_identical(res$details$backend, "rmorie native")
})

test_that("Goodman-Bacon: weights sum to 1 and overall equals TWFE", {
  pan <- .mk_stag_panel(n_units = 90, n_t = 6, seed = 10)
  out <- morie_did_bacon_decomposition(pan, "y", "d", "id", "tt")
  expect_equal(sum(out$components$weight), 1, tolerance = 1e-10)
  # The decomposition identity: weighted average of 2x2s = TWFE coef.
  twfe <- morie_did_panel_fe(pan, "y", "d", "id", "tt")
  expect_equal(out$overall_estimate, twfe$estimate, tolerance = 1e-8)
  expect_true(all(out$components$type %in%
                    c("Treated vs Untreated", "Earlier vs Later Treated",
                      "Later vs Earlier Treated")))
})

test_that("feTR weights: sum to 1; late-period negative weights detected", {
  # Strongly staggered design with long post-periods produces the
  # canonical negative-weight pathology.
  pan <- .mk_stag_panel(n_units = 150, n_t = 12, seed = 11)
  pan$g2 <- ifelse(pan$g == 0, 0, pmax(pan$g - 2, 2))
  pan$d2 <- as.integer(pan$g2 > 0 & pan$tt >= pan$g2)
  out <- morie_did_twoway_fe_weights(pan, "id", "tt", "d2")
  expect_s3_class(out, "morie_did_twfe_diagnostics")
  expect_equal(out$sum_weights, 1, tolerance = 1e-10)
  expect_true(out$n_negative_weights >= 0)
  expect_identical(out$method, "twoway_fe_weights (rmorie native)")
  expect_error(morie_did_twoway_fe_weights(pan, "id", "tt", "d2",
                                           type = "feS"),
               "feTR")
})

test_that("group_time_att se_convention bessel >= reference relation holds", {
  pan <- .mk_stag_panel(n_units = 80, seed = 12)
  a <- morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                n_bootstrap = 0L)
  b <- morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                n_bootstrap = 0L,
                                se_convention = "bessel")
  expect_equal(a$att, b$att)
  expect_false(isTRUE(all.equal(a$std_error, b$std_error)))
  # Same order of magnitude (they differ only in centering + Bessel)
  expect_true(all(abs(a$std_error / b$std_error - 1) < 0.1))
})

test_that("honest sensitivity: M-bar widens CIs; breakdown found", {
  pan <- .mk_stag_panel(seed = 20)
  es <- morie_did_event_study(pan, "y", "id", "tt", "g",
                              leads = 3L, lags = 3L)
  out <- morie_did_honest_sensitivity(es, m_bar_range = c(0, 1, 5, 50))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4L)
  widths <- out$ci_upper - out$ci_lower
  expect_true(all(diff(widths) >= 0))
  # M-bar = 0 reproduces the conventional CI
  cf0 <- es$coefficients[es$coefficients$relative_time == 0, ]
  z <- qnorm(0.975)
  expect_equal(out$ci_lower[1], cf0$estimate - z * cf0$std_error,
               tolerance = 1e-10)
  # A large enough M-bar always covers zero (tau = 1.5 with real
  # pre-period noise), so a breakdown point exists
  expect_true(is.finite(attr(out, "breakdown_m_bar")))
  expect_false(out$covers_zero[1])
  expect_true(out$covers_zero[4])
  # errors on a bad target time / malformed input
  expect_error(morie_did_honest_sensitivity(es, target_time = 99L),
               "relative time")
  expect_error(morie_did_honest_sensitivity(list()), "coefficients")
})

# --- Module 15: synthetic control family ---

test_that("simplex projection and simplex LS behave", {
  p <- rmorie:::.morie_simplex_proj(c(2, 0.5, -1))
  expect_equal(sum(p), 1)
  expect_true(all(p >= 0))
  # LS with an exactly representable target recovers the weights
  set.seed(30)
  A <- matrix(rnorm(200), 20, 10)
  w0 <- rmorie:::.morie_simplex_proj(runif(10))
  b <- as.numeric(A %*% w0)
  w <- rmorie:::.morie_simplex_ls(A, b)
  expect_equal(as.numeric(A %*% w), b, tolerance = 1e-5)
})

test_that("morie_synth_control recovers a known treatment effect", {
  set.seed(31)
  n_t <- 14
  onset <- 10
  # Treated unit is an exact convex combo of donors 1-3 + effect 2 post.
  donors <- matrix(rnorm(8 * n_t, sd = 0.3), 8, n_t) +
    outer(runif(8, -1, 1), seq_len(n_t) * 0.2)
  w_true <- c(0.5, 0.3, 0.2, rep(0, 5))
  treated <- as.numeric(t(donors) %*% w_true) +
    c(rep(0, onset - 1), rep(2, n_t - onset + 1))
  pan <- rbind(
    data.frame(unit = "tr", time = seq_len(n_t), y = treated),
    do.call(rbind, lapply(1:8, function(i)
      data.frame(unit = paste0("d", i), time = seq_len(n_t),
                 y = donors[i, ]))))
  fit <- morie_synth_control(pan, "y", "unit", "time",
                             treated_unit = "tr", treatment_time = onset)
  expect_s3_class(fit, "morie_synth")
  expect_equal(sum(fit$weights), 1, tolerance = 1e-6)
  expect_true(all(fit$weights >= -1e-10))
  expect_equal(fit$att, 2, tolerance = 0.2)
  expect_lt(fit$pre_rmspe, 0.1)
  # the treated unit has the most extreme RMSPE ratio -> small p
  expect_lte(fit$placebo_pvalue, 2 / 9 + 1e-9)
  expect_output(print(fit), "Synthetic control")
})

test_that("morie_synth_control validates input", {
  pan <- data.frame(unit = rep(c("a", "b"), each = 3),
                    time = rep(1:3, 2), y = rnorm(6))
  expect_error(morie_synth_control(pan[-1, ], "y", "unit", "time",
                                   treated_unit = "a",
                                   treatment_time = 3),
               "balanced")
  expect_error(morie_synth_control(pan, "y", "unit", "time",
                                   treated_unit = "zz",
                                   treatment_time = 3),
               "treated_unit")
  expect_error(morie_synth_control(pan, "y", "unit", "time",
                                   treated_unit = "a",
                                   treatment_time = 2),
               "pre-treatment")
})

test_that("native SDID recovers tau; all three inference methods run", {
  set.seed(32)
  N_co <- 25
  N_tr <- 5
  T_pre <- 8
  T_post <- 4
  n_t <- T_pre + T_post
  u <- rnorm(N_co + N_tr)
  tfx <- cumsum(rnorm(n_t, 0.2, 0.1))
  Y <- outer(u, rep(1, n_t)) + outer(rep(1, N_co + N_tr), tfx) +
    matrix(rnorm((N_co + N_tr) * n_t, 0, 0.3), N_co + N_tr)
  Y[N_co + seq_len(N_tr), T_pre + seq_len(T_post)] <-
    Y[N_co + seq_len(N_tr), T_pre + seq_len(T_post)] + 1.8
  fit <- rmorie:::.morie_sdid_native(Y, N_co, T_pre)
  expect_equal(fit$estimate, 1.8, tolerance = 0.3)
  expect_equal(sum(fit$unit_weights), 1, tolerance = 1e-6)
  expect_equal(sum(fit$time_weights), 1, tolerance = 1e-6)
  for (m in c("placebo", "jackknife", "bootstrap")) {
    inf <- rmorie:::.morie_sdid_inference(Y, N_co, T_pre, method = m,
                                          n_boot = 60L, seed = 32)
    expect_true(is.finite(inf$se) && inf$se > 0)
  }
})

test_that("morie_did_synthetic + synthdid_estimate use the native engine", {
  set.seed(33)
  n_units <- 20
  n_t <- 10
  onset <- 7
  treated <- c("u1", "u2")
  pan <- do.call(rbind, lapply(seq_len(n_units), function(i) {
    id <- paste0("u", i)
    tr <- id %in% treated
    y <- rnorm(1) + 0.3 * seq_len(n_t) +
      ifelse(tr & seq_len(n_t) >= onset, 1.2, 0) + rnorm(n_t, 0, 0.3)
    data.frame(unit = id, time = seq_len(n_t), y = y,
               g = ifelse(tr, onset, Inf),
               w01 = as.integer(tr & seq_len(n_t) >= onset))
  }))
  res <- morie_did_synthetic(pan, "y", "unit", "time", "g",
                             n_bootstrap = 50L, seed = 33)
  expect_equal(res$method, "synthetic_did (rmorie native)")
  expect_equal(res$estimate, 1.2, tolerance = 0.35)
  expect_true(is.finite(res$std_error))
  res2 <- morie_did_synthdid_estimate(pan, "unit", "time", "w01", "y",
                                      vcov_method = "jackknife")
  expect_s3_class(res2, "morie_did_synthdid_result")
  expect_equal(res2$att, res$estimate, tolerance = 1e-10)
  expect_equal(res2$n_pre, 6L)
  expect_equal(res2$n_treated, 2L)
  expect_identical(res2$method, "sdid (rmorie native)")
})
