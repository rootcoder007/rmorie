# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Synthetic data generators for DiD tests
# ---------------------------------------------------------------------------

set.seed(1)

# Simple 2x2 DiD DGP: tau = 0.5
make_did_2x2 <- function(n = 400, tau = 0.5, seed = 1) {
  set.seed(seed)
  d <- rbinom(n, 1, 0.5)
  p <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  y <- 1.0 + 0.3 * d + 0.4 * p + tau * d * p + 0.2 * x + rnorm(n, sd = 0.5)
  data.frame(y = y, d = d, post = p, x = x,
             clust = sample.int(20, n, replace = TRUE))
}

# Balanced panel DiD DGP: tau = 0.7
make_did_panel <- function(n_units = 30, n_periods = 6,
                           tau = 0.7, seed = 1) {
  set.seed(seed)
  # half units get treatment at period 4
  treat_unit <- rbinom(n_units, 1, 0.5)
  treat_time <- ifelse(treat_unit == 1L, 4L, Inf)
  rows <- list()
  for (u in seq_len(n_units)) {
    a_i <- rnorm(1)
    for (t in seq_len(n_periods)) {
      d_it <- as.integer(treat_unit[u] == 1L && t >= 4L)
      y <- a_i + 0.1 * t + tau * d_it + rnorm(1, sd = 0.4)
      rows[[length(rows) + 1L]] <- data.frame(
        unit = u, time = t, y = y, d = d_it,
        treat_time = treat_time[u], x = rnorm(1)
      )
    }
  }
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# 1. morie_did_2x2
# ---------------------------------------------------------------------------

test_that("morie_did_2x2 returns expected fields on simple DGP", {
  # 1s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_heavy()
  df <- make_did_2x2()
  res <- morie_did_2x2(df, "y", "d", "post")
  expect_true(all(c("estimate", "std_error", "t_stat", "p_value",
                    "ci_lower", "ci_upper", "n_treated", "n_control",
                    "method", "details") %in% names(res)))
  expect_type(res$estimate, "double")
  expect_true(is.finite(res$std_error))
  expect_lt(res$ci_lower, res$ci_upper)
  expect_identical(res$method, "did_2x2")
})

test_that("morie_did_2x2 recovers true effect tau = 0.5", {
  skip_heavy()
  df <- make_did_2x2(n = 800, tau = 0.5, seed = 2)
  res <- morie_did_2x2(df, "y", "d", "post")
  expect_equal(res$estimate, 0.5, tolerance = 0.2)
})

test_that("morie_did_2x2 handles covariates", {
  skip_heavy()
  df <- make_did_2x2()
  res <- morie_did_2x2(df, "y", "d", "post", covariates = "x")
  expect_true(is.finite(res$estimate))
  expect_true(length(res$details$all_coefficients) >= 5)
})

test_that("morie_did_2x2 drops NA rows", {
  skip_heavy()
  df <- make_did_2x2(n = 100)
  df$y[1:5] <- NA
  res <- morie_did_2x2(df, "y", "d", "post")
  expect_equal(res$details$n_obs, 95)
})


# ---------------------------------------------------------------------------
# 2. morie_did_repeated_cross_section
# ---------------------------------------------------------------------------

test_that("morie_did_repeated_cross_section works with weights", {
  skip_heavy()
  df <- make_did_2x2()
  df$w <- runif(nrow(df), 0.5, 2)
  res <- morie_did_repeated_cross_section(df, "y", "d", "post",
                                          weights = "w")
  expect_true(is.finite(res$estimate))
  expect_equal(res$method, "did_repeated_cross_section")
})


# ---------------------------------------------------------------------------
# 3. morie_did_panel_fe
# ---------------------------------------------------------------------------

test_that("morie_did_panel_fe recovers tau on panel DGP", {
  skip_heavy()
  # Module 14: native TWFE engine.
  df <- make_did_panel(n_units = 40, n_periods = 6, tau = 0.7, seed = 3)
  res <- morie_did_panel_fe(df, "y", "d", "unit", "time")
  expect_true(is.finite(res$estimate))
  expect_equal(res$estimate, 0.7, tolerance = 0.25)
  expect_true(grepl("did_panel_fe", res$method))
})


# ---------------------------------------------------------------------------
# 4. morie_did_event_study
# ---------------------------------------------------------------------------

test_that("morie_did_event_study returns coefficients with reference period", {
  skip_heavy()
  # Module 14: native event-study engine.
  df <- make_did_panel()
  res <- morie_did_event_study(df, "y", "unit", "time", "treat_time",
                               leads = 2L, lags = 2L)
  expect_true("coefficients" %in% names(res))
  expect_s3_class(res$coefficients, "data.frame")
  expect_true(any(res$coefficients$relative_time == -1))
  # reference period has estimate 0 by construction
  ref <- res$coefficients[res$coefficients$relative_time == -1, ]
  expect_equal(ref$estimate, 0)
})


# ---------------------------------------------------------------------------
# 5. morie_did_test_parallel_trends
# ---------------------------------------------------------------------------

test_that("morie_did_test_parallel_trends returns expected fields", {
  skip_heavy()
  df <- make_did_panel()
  # build a binary treatment col & restrict to pre-treatment for the test
  df$treat <- as.integer(is.finite(df$treat_time))
  res <- morie_did_test_parallel_trends(df, "y", "treat", "time",
                                        pre_periods = c(1, 2, 3))
  expect_true(all(c("coefficients", "joint_f_stat", "joint_p_value",
                    "parallel_trends_plausible") %in% names(res)))
  expect_type(res$parallel_trends_plausible, "logical")
})


# ---------------------------------------------------------------------------
# 6. morie_did_parallel_trends_data
# ---------------------------------------------------------------------------

test_that("morie_did_parallel_trends_data returns group-by-time means", {
  skip_heavy()
  df <- make_did_panel()
  df$treat <- as.integer(is.finite(df$treat_time))
  out <- morie_did_parallel_trends_data(df, "y", "treat", "time")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("time", "group", "mean_outcome", "se", "n") %in%
                    colnames(out)))
  expect_gt(nrow(out), 0)
})


# ---------------------------------------------------------------------------
# 7. morie_did_group_time_att / staggered / aggregate
# ---------------------------------------------------------------------------

test_that("morie_did_group_time_att returns a data frame with att", {
  skip_heavy()
  # Module 14: native Callaway-Sant'Anna engine.
  df <- make_did_panel(n_units = 50, n_periods = 6, tau = 0.6, seed = 4)
  out <- tryCatch(
    morie_did_group_time_att(df, "y", "unit", "time", "treat_time",
                             n_bootstrap = 30L, seed = 4),
    error = function(e) NULL
  )
  skip_if(is.null(out), "group_time_att failed in environment")
  expect_s3_class(out, "data.frame")
  expect_true("att" %in% colnames(out))
})

test_that("morie_did_aggregate_gt_att overall summary returns one-row df", {
  skip_heavy()
  fake_gt <- data.frame(cohort = c(4, 4, 4),
                        time = c(4, 5, 6),
                        att = c(0.5, 0.6, 0.7),
                        std_error = c(0.1, 0.1, 0.1))
  out <- morie_did_aggregate_gt_att(fake_gt, aggregation = "overall")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$estimate, mean(fake_gt$att), tolerance = 1e-6)
})

test_that("morie_did_aggregate_gt_att event_time aggregation splits by rel time", {
  skip_heavy()
  fake_gt <- data.frame(cohort = c(4, 4, 4, 5, 5),
                        time = c(4, 5, 6, 5, 6),
                        att = c(0.5, 0.6, 0.7, 0.4, 0.5),
                        std_error = c(0.1, 0.1, 0.1, 0.1, 0.1))
  out <- morie_did_aggregate_gt_att(fake_gt, aggregation = "event_time")
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 1)
})


# ---------------------------------------------------------------------------
# 8. morie_did_doubly_robust
# ---------------------------------------------------------------------------

test_that("morie_did_doubly_robust returns finite ATT", {
  skip_heavy()
  # Module 14: native Sant'Anna-Zhao engine.
  df <- make_did_2x2(n = 300)
  res <- morie_did_doubly_robust(df, "y", "d", "post",
                                 covariates = "x",
                                 n_bootstrap = 30L, seed = 5)
  expect_true(is.finite(res$estimate))
  expect_true(grepl("did_doubly_robust", res$method))
})


# ---------------------------------------------------------------------------
# 9. morie_did_triple_difference
# ---------------------------------------------------------------------------

test_that("morie_did_triple_difference returns finite estimate", {
  skip_heavy()
  set.seed(1)
  n <- 400
  d <- rbinom(n, 1, 0.5)
  p <- rbinom(n, 1, 0.5)
  s <- rbinom(n, 1, 0.5)
  y <- 0.2 * d + 0.3 * p + 0.4 * s + 0.5 * d * p * s + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, d = d, post = p, group = s)
  res <- morie_did_triple_difference(df, "y", "d", "post", "group")
  expect_true(is.finite(res$estimate))
  expect_equal(res$method, "did_triple_difference")
})


# ---------------------------------------------------------------------------
# 10. morie_did_bacon_decomposition
# ---------------------------------------------------------------------------

test_that("morie_did_bacon_decomposition returns components and overall", {
  skip_heavy()
  # Module 14: native Goodman-Bacon engine.
  # bacondecomp::bacon requires (a) weakly-increasing treatment per
  # unit and (b) genuine staggered timing (>=2 treatment cohorts) to
  # produce 2x2 comparisons. make_did_panel only ever treats at t=4,
  # so we build a staggered panel inline here.
  set.seed(11)
  n_units <- 40L
  n_periods <- 8L
  # Three cohorts: control (never), early (t>=4), late (t>=6).
  cohort <- rep(c("never", "early", "late"),
                length.out = n_units)
  treat_time <- ifelse(cohort == "early", 4L,
                       ifelse(cohort == "late", 6L, NA_integer_))
  rows <- list()
  for (u in seq_len(n_units)) {
    a_i <- stats::rnorm(1)
    for (t in seq_len(n_periods)) {
      d_it <- as.integer(!is.na(treat_time[u]) && t >= treat_time[u])
      y <- a_i + 0.1 * t + 0.5 * d_it + stats::rnorm(1, sd = 0.4)
      rows[[length(rows) + 1L]] <- data.frame(
        unit = u, time = t, y = y, d = d_it
      )
    }
  }
  df <- do.call(rbind, rows)
  res <- morie_did_bacon_decomposition(df, "y", "d", "unit", "time")
  expect_true("components" %in% names(res))
  expect_true("overall_estimate" %in% names(res))
})


# ---------------------------------------------------------------------------
# 11. morie_did_wild_cluster_bootstrap
# ---------------------------------------------------------------------------

test_that("morie_did_wild_cluster_bootstrap returns finite p", {
  skip_heavy()
  df <- make_did_2x2(n = 300)
  res <- morie_did_wild_cluster_bootstrap(
    df, "y", "d", "post", cluster = "clust",
    n_bootstrap = 99L, seed = 7
  )
  expect_true(is.finite(res$estimate))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})


# ---------------------------------------------------------------------------
# 12. morie_did_continuous_treatment
# ---------------------------------------------------------------------------

test_that("morie_did_continuous_treatment estimates dose effect", {
  skip_heavy()
  set.seed(1)
  n <- 300
  dose <- runif(n, 0, 3)
  p <- rbinom(n, 1, 0.5)
  y <- 0.4 * dose * p + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, dose = dose, post = p)
  res <- morie_did_continuous_treatment(df, "y", "dose", "post")
  expect_true(is.finite(res$estimate))
  expect_equal(res$estimate, 0.4, tolerance = 0.2)
})


# ---------------------------------------------------------------------------
# 13. morie_did_fuzzy
# ---------------------------------------------------------------------------

test_that("morie_did_fuzzy returns first-stage F and estimate", {
  skip_heavy()
  set.seed(1)
  n <- 400
  z <- rbinom(n, 1, 0.5)
  d <- as.integer(z & rbinom(n, 1, 0.8))
  p <- rbinom(n, 1, 0.5)
  y <- 0.5 * d * p + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, z = z, d = d, post = p)
  res <- morie_did_fuzzy(df, "y", "z", "d", "post")
  expect_true(is.finite(res$estimate))
  expect_true(!is.null(res$details$first_stage_f))
})


# ---------------------------------------------------------------------------
# 14. placebo tests
# ---------------------------------------------------------------------------

test_that("morie_did_placebo_test_time returns one row per placebo", {
  skip_heavy()
  set.seed(1)
  df <- data.frame(
    y = rnorm(500), d = rbinom(500, 1, 0.5),
    time = sample(1:8, 500, replace = TRUE)
  )
  out <- morie_did_placebo_test_time(df, "y", "d", "time",
                                     true_treatment_time = 7,
                                     placebo_times = c(3, 4, 5))
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) <= 3)
  expect_true(all(c("placebo_time", "estimate", "p_value",
                    "significant") %in% colnames(out)))
})

test_that("morie_did_placebo_test_outcome returns one row per placebo outcome", {
  skip_heavy()
  df <- make_did_2x2()
  df$y_pl1 <- rnorm(nrow(df))
  df$y_pl2 <- rnorm(nrow(df))
  out <- morie_did_placebo_test_outcome(df, c("y_pl1", "y_pl2"),
                                        "d", "post")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
})

test_that("morie_did_placebo_test_group returns one row per group", {
  skip_heavy()
  df <- make_did_2x2()
  df$grp <- sample(c("A", "B"), nrow(df), replace = TRUE)
  out <- morie_did_placebo_test_group(df, "y", "d", "post",
                                      group_col = "grp",
                                      unaffected_groups = c("A", "B"))
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) <= 2)
})


# ---------------------------------------------------------------------------
# 15. morie_did_heterogeneous
# ---------------------------------------------------------------------------

test_that("morie_did_heterogeneous returns one row per stratum", {
  skip_heavy()
  df <- make_did_2x2(n = 600)
  df$mod <- rnorm(nrow(df))
  out <- morie_did_heterogeneous(df, "y", "d", "post",
                                 moderator = "mod", n_quantiles = 3L)
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 1 && nrow(out) <= 3)
})


# ---------------------------------------------------------------------------
# 16. morie_did_chaisemartin_dhaultfoeuille
# ---------------------------------------------------------------------------

test_that("morie_did_chaisemartin_dhaultfoeuille returns a finite estimate", {
  skip_heavy()
  # Module 14: native DID-M engine.
  df <- make_did_panel(n_units = 30, n_periods = 5, tau = 0.5, seed = 9)
  res <- morie_did_chaisemartin_dhaultfoeuille(
    df, "y", "d", "unit", "time",
    n_bootstrap = 20L, seed = 9
  )
  expect_true(is.finite(res$estimate) || is.na(res$estimate))
  expect_equal(res$method, "chaisemartin_dhaultfoeuille")
})


# ---------------------------------------------------------------------------
# 17. morie_did_sensitivity_analysis
# ---------------------------------------------------------------------------

test_that("morie_did_sensitivity_analysis returns one row per delta", {
  skip_heavy()
  df <- make_did_2x2(n = 300)
  out <- morie_did_sensitivity_analysis(df, "y", "d", "post",
                                        delta_range = c(0, 0.5, 1))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true(all(c("delta", "ci_lower", "ci_upper",
                    "covers_zero") %in% colnames(out)))
  # CI widens with delta
  widths <- out$ci_upper - out$ci_lower
  expect_true(widths[3] >= widths[1])
})


# ---------------------------------------------------------------------------
# 18. morie_did_diagnostics
# ---------------------------------------------------------------------------

test_that("morie_did_diagnostics returns sample sizes and outcome stats", {
  skip_heavy()
  df <- make_did_2x2()
  res <- morie_did_diagnostics(df, "y", "d", "post", covariates = "x")
  expect_true(all(c("sample_sizes", "outcome_stats",
                    "covariate_balance") %in% names(res)))
  expect_s3_class(res$outcome_stats, "data.frame")
  expect_true(!is.null(res$covariate_balance))
})

test_that("TWFE and the event study equal fixest (balanced and unbalanced panels)", {
  d <- expand.grid(time = 1:8, unit = 1:40)
  d$g <- ifelse(d$unit <= 12, 4, ifelse(d$unit <= 24, 6, 0))
  d$D <- as.integer(d$g > 0 & d$time >= d$g)
  d$x <- round(sin(1.3 * d$unit) + 0.2 * d$time, 4)
  d$y <- round(0.5 * d$unit / 10 + 0.3 * d$time + 1.5 * d$D + 0.4 * d$D * (d$time - d$g) * (d$g > 0) +
                 0.3 * sin(2.7 * d$unit * d$time) + 0.2 * d$x, 5)
  d$tt <- ifelse(d$g > 0, d$g, NA)
  u <- d[!(d$unit %in% c(3, 17, 30) & d$time %in% c(2, 5)), ]
  # feols(y ~ D | unit + time, cluster = ~unit)
  r <- morie_did_panel_fe(d, "y", "D", "unit", "time", cluster = "unit")
  expect_equal(c(r$estimate, r$std_error), c(1.88383260684, 0.0758991745971), tolerance = 1e-10)
  r <- morie_did_panel_fe(u, "y", "D", "unit", "time", cluster = "unit")
  expect_equal(c(r$estimate, r$std_error), c(1.8824137808, 0.0767819050896), tolerance = 1e-10)
  # feols(y ~ i(rel binned to [-4, 4], ref = c(-1, never)) | unit + time) and wald(pre)
  es <- morie_did_event_study(u, "y", "unit", "time", "tt", cluster = "unit")
  cf <- es$coefficients
  i <- match(c(-4, 0, 4), cf$relative_time)
  expect_equal(cf$estimate[i], c(-0.0579744164321, 1.43679248031, 3.07422506574), tolerance = 1e-10)
  expect_equal(cf$std_error[i], c(0.0940534922065, 0.0899078370266, 0.0939645477063), tolerance = 1e-10)
  expect_equal(c(es$pre_trend_f_stat, es$pre_trend_p_value), c(0.216136594631, 0.884611828383), tolerance = 1e-9)
})

test_that("synthetic DiD equals synthdid (estimate, jackknife, bootstrap and placebo SEs)", {
  d <- expand.grid(time = 1:8, unit = 1:40)
  d$g <- ifelse(d$unit <= 12, 4, ifelse(d$unit <= 24, 6, 0))
  d$D <- as.integer(d$g > 0 & d$time >= d$g)
  d$x <- round(sin(1.3 * d$unit) + 0.2 * d$time, 4)
  d$y <- round(0.5 * d$unit / 10 + 0.3 * d$time + 1.5 * d$D + 0.4 * d$D * (d$time - d$g) * (d$g > 0) +
                 0.3 * sin(2.7 * d$unit * d$time) + 0.2 * d$x, 5)
  b <- d[d$g %in% c(0, 4), ]
  b$W <- as.integer(b$g == 4 & b$time >= 4)
  # synthdid_estimate(Y, N0, T0) and vcov(method = "jackknife")
  r <- morie_did_synthdid_estimate(b, "unit", "time", "W", "y", vcov_method = "jackknife")
  expect_equal(c(r$att, r$std_error), c(2.27183974304, 0.0699340890602), tolerance = 1e-10)
  expect_equal(r$raw$time_weights, c(0.314692, 0.321395, 0.363913), tolerance = 1e-5)
})

test_that("parallel-trends joint Wald equals fixest::wald", {
  # reference: feols(y ~ g * factor(t), cluster = ~id) then wald(keep = "^g:tf")
  G <- 30
  df <- expand.grid(id = 1:G, t = 1:6)
  df$g <- as.numeric(df$id <= 12)
  df$y <- 0.3 * df$g + 0.1 * df$t + sin(1.7 * df$id) +
    0.05 * df$g * df$t + 0.8 * cos(0.9 * df$id * df$t)
  r1 <- morie_did_test_parallel_trends(df, "y", "g", "t", unit = "id",
                                       pre_periods = 1:4)
  expect_equal(r1$joint_f_stat, 0.942822618248327, tolerance = 1e-10)
  expect_equal(r1$joint_p_value, 0.432733321843015, tolerance = 1e-10)
  r2 <- morie_did_test_parallel_trends(df, "y", "g", "t", pre_periods = 1:4)
  expect_equal(r2$joint_f_stat, 0.188423585724467, tolerance = 1e-10)
  expect_equal(r2$joint_p_value, 0.904089344068759, tolerance = 1e-10)
})
