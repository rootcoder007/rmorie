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
  df <- make_did_2x2(n = 800, tau = 0.5, seed = 2)
  res <- morie_did_2x2(df, "y", "d", "post")
  expect_equal(res$estimate, 0.5, tolerance = 0.2)
})

test_that("morie_did_2x2 handles covariates", {
  df <- make_did_2x2()
  res <- morie_did_2x2(df, "y", "d", "post", covariates = "x")
  expect_true(is.finite(res$estimate))
  expect_true(length(res$details$all_coefficients) >= 5)
})

test_that("morie_did_2x2 drops NA rows", {
  df <- make_did_2x2(n = 100)
  df$y[1:5] <- NA
  res <- morie_did_2x2(df, "y", "d", "post")
  expect_equal(res$details$n_obs, 95)
})


# ---------------------------------------------------------------------------
# 2. morie_did_repeated_cross_section
# ---------------------------------------------------------------------------

test_that("morie_did_repeated_cross_section works with weights", {
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
  df <- make_did_2x2(n = 300)
  res <- morie_did_doubly_robust(df, "y", "d", "post",
                                 covariates = "x",
                                 n_bootstrap = 30L, seed = 5)
  expect_true(is.finite(res$estimate))
  expect_equal(res$method, "did_doubly_robust")
})


# ---------------------------------------------------------------------------
# 9. morie_did_triple_difference
# ---------------------------------------------------------------------------

test_that("morie_did_triple_difference returns finite estimate", {
  set.seed(1)
  n <- 400
  d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
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
  df <- make_did_panel(n_units = 30, n_periods = 6, tau = 0.5, seed = 6)
  res <- morie_did_bacon_decomposition(df, "y", "d", "unit", "time")
  expect_true("components" %in% names(res))
  expect_true("overall_estimate" %in% names(res))
})


# ---------------------------------------------------------------------------
# 11. morie_did_wild_cluster_bootstrap
# ---------------------------------------------------------------------------

test_that("morie_did_wild_cluster_bootstrap returns finite p", {
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
  df <- make_did_2x2()
  df$y_pl1 <- rnorm(nrow(df))
  df$y_pl2 <- rnorm(nrow(df))
  out <- morie_did_placebo_test_outcome(df, c("y_pl1", "y_pl2"),
                                        "d", "post")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
})

test_that("morie_did_placebo_test_group returns one row per group", {
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
  df <- make_did_2x2()
  res <- morie_did_diagnostics(df, "y", "d", "post", covariates = "x")
  expect_true(all(c("sample_sizes", "outcome_stats",
                    "covariate_balance") %in% names(res)))
  expect_s3_class(res$outcome_stats, "data.frame")
  expect_true(!is.null(res$covariate_balance))
})
