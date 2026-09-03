# SPDX-License-Identifier: AGPL-3.0-or-later
# Property-based invariants for the native DiD family across random DGPs.
#
#' @srrstats {G5.9b} Estimator invariants hold across multiple random
#'   seeds and design shapes.
library(testthat)
library(rmorie)

.rand_panel <- function(seed) {
  set.seed(seed)
  n_units <- sample(60:150, 1)
  n_t <- sample(5:9, 1)
  n_onsets <- sample(seq_len(min(3L, n_t - 3L)), 1)
  onsets <- sort(sample(3:(n_t - 1), n_onsets))
  g <- sample(c(0, onsets), n_units, replace = TRUE)
  tau <- runif(1, 0.5, 2.5)
  u <- rnorm(n_units)
  pan <- do.call(rbind, lapply(seq_len(n_units), function(i) {
    t <- seq_len(n_t)
    d <- as.integer(g[i] > 0 & t >= g[i])
    data.frame(id = i, tt = t, d = d, g = g[i],
               y = u[i] + 0.3 * t + d * tau + rnorm(n_t, 0, 0.4))
  }))
  attr(pan, "tau") <- tau
  pan
}

test_that("property: TWFE = explicit-dummy OLS across random panels", {
  for (seed in c(2L, 23L, 61L)) {
    pan <- .rand_panel(seed)
    fit <- rmorie:::.morie_did_twfe_native(pan$y, cbind(d = pan$d),
                                           pan$id, pan$tt, pan$id)
    ref <- stats::lm(y ~ d + factor(id) + factor(tt), data = pan)
    expect_equal(unname(fit$beta[["d"]]),
                 unname(stats::coef(ref)[["d"]]), tolerance = 1e-9)
  }
})

test_that("property: Bacon identity — decomposition averages to TWFE", {
  for (seed in c(5L, 31L, 77L)) {
    pan <- .rand_panel(seed)
    out <- morie_did_bacon_decomposition(pan, "y", "d", "id", "tt")
    expect_equal(sum(out$components$weight), 1, tolerance = 1e-9)
    twfe <- morie_did_panel_fe(pan, "y", "d", "id", "tt")
    expect_equal(out$overall_estimate, twfe$estimate, tolerance = 1e-7)
  }
})

test_that("property: ATT(g,t) recovers a constant effect across designs", {
  for (seed in c(7L, 43L)) {
    pan <- .rand_panel(seed)
    tau <- attr(pan, "tau")
    out <- morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                    n_bootstrap = 0L)
    post <- out[out$time >= out$cohort, ]
    if (!nrow(post)) next
    expect_equal(mean(post$att), tau, tolerance = 0.3)
    expect_true(all(is.finite(post$std_error) & post$std_error > 0))
    # Pre-treatment placebo cells centre on zero
    pre <- out[out$time < out$cohort, ]
    if (nrow(pre) >= 3) expect_lt(abs(mean(pre$att)), 0.3)
  }
})

test_that("property: DID-M recovers a constant instantaneous effect", {
  for (seed in c(11L, 53L)) {
    pan <- .rand_panel(seed)
    tau <- attr(pan, "tau")
    est <- rmorie:::.morie_didm_point(pan, "y", "d", "id", "tt")
    expect_true(is.finite(est))
    expect_equal(est, tau, tolerance = 0.45)
  }
})

test_that("property: feTR weights sum to one over treated cells", {
  for (seed in c(13L, 59L)) {
    pan <- .rand_panel(seed)
    out <- morie_did_twoway_fe_weights(pan, "id", "tt", "d")
    expect_equal(out$sum_weights, 1, tolerance = 1e-9)
    w <- out$raw$weight
    expect_identical(sum(!is.na(w)), sum(out$raw$treatment != 0))
  }
})

test_that("property: event-study reference period is exactly zero", {
  for (seed in c(17L, 71L)) {
    pan <- .rand_panel(seed)
    res <- morie_did_event_study(pan, "y", "id", "tt", "g",
                                 leads = 2L, lags = 2L)
    cf <- res$coefficients
    expect_equal(cf$estimate[cf$relative_time == -1], 0)
    expect_true(all(is.finite(cf$estimate)))
    expect_true(all(diff(cf$relative_time) > 0))
  }
})

test_that("property: doubly robust DiD is invariant to covariate scaling", {
  set.seed(19)
  n <- 700
  x <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.5 * x))
  post <- rbinom(n, 1, 0.5)
  y <- 1 + 0.4 * x + 0.3 * post + 1.2 * d * post + rnorm(n)
  df <- data.frame(y = y, d = d, post = post, x = x, x_scaled = 100 * x)
  a <- morie_did_doubly_robust(df, "y", "d", "post", covariates = "x",
                               n_bootstrap = 0L)
  b <- morie_did_doubly_robust(df, "y", "d", "post",
                               covariates = "x_scaled",
                               n_bootstrap = 0L)
  expect_equal(a$estimate, b$estimate, tolerance = 1e-8)
  expect_equal(a$std_error, b$std_error, tolerance = 1e-8)
})

test_that("property: SCM weights are a distribution; synth interpolates", {
  for (seed in c(29L, 83L)) {
    set.seed(seed)
    n_d <- sample(5:9, 1)
    n_t <- sample(10:14, 1)
    onset <- n_t - 3
    donors <- matrix(rnorm(n_d * n_t), n_d, n_t)
    w0 <- rmorie:::.morie_simplex_proj(runif(n_d))
    treated <- as.numeric(t(donors) %*% w0) + rnorm(n_t, 0, 0.05)
    pan <- rbind(
      data.frame(unit = "tr", time = seq_len(n_t), y = treated),
      do.call(rbind, lapply(seq_len(n_d), function(i)
        data.frame(unit = paste0("d", i), time = seq_len(n_t),
                   y = donors[i, ]))))
    fit <- morie_synth_control(pan, "y", "unit", "time",
                               treated_unit = "tr",
                               treatment_time = onset,
                               optimize_v = FALSE)
    expect_equal(sum(fit$weights), 1, tolerance = 1e-6)
    expect_true(all(fit$weights >= -1e-10))
    # synthetic path stays within the donor envelope
    syn <- fit$time_series$synthetic
    expect_true(all(syn <= apply(donors, 2, max) + 1e-8))
    expect_true(all(syn >= apply(donors, 2, min) - 1e-8))
  }
})
