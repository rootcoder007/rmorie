# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 16: Sun-Abraham, Borusyak imputation, Gardner did2s --
# known-truth recovery + cross-validation vs did2s/didimputation.

.dm_panel <- function(n_id = 120L, n_t = 10L, tau = 2, seed = 42) {
  set.seed(seed)
  df <- expand.grid(id = seq_len(n_id), t = seq_len(n_t))
  df$g <- ifelse(df$id <= n_id / 3, 5L,
                 ifelse(df$id <= 2 * n_id / 3, 7L, NA))
  unit_fe <- rnorm(n_id)[df$id]
  time_fe <- 0.25 * seq_len(n_t)[df$t]
  df$y <- unit_fe + time_fe +
    ifelse(!is.na(df$g) & df$t >= df$g, tau, 0) +
    rnorm(nrow(df), sd = 0.4)
  df
}

test_that("Sun-Abraham event study recovers the dynamic truth", {
  df <- .dm_panel()
  es <- morie_did_sun_abraham(df, "y", "id", "t", "g",
                              leads = 3L, lags = 3L)
  expect_s3_class(es, "morie_event_study")
  pre <- es[es$rel_time < 0, ]
  post <- es[es$rel_time >= 0, ]
  # Pre-period effects ~ 0; post ~ tau = 2.
  expect_lt(max(abs(pre$estimate)), 3 * max(pre$std.error))
  expect_true(all(abs(post$estimate - 2) < 4 * post$std.error))
  expect_false(-1 %in% es$rel_time) # reference period omitted
  expect_true(is.data.frame(attr(es, "cells")))
})

test_that("Borusyak imputation recovers the ATT", {
  df <- .dm_panel()
  fit <- morie_did_borusyak(df, "y", "id", "t", "g",
                            n_bootstrap = 59L)
  expect_s3_class(fit, "morie_did")
  expect_lt(abs(fit$estimate - 2), 4 * fit$std.error)
  expect_true(fit$conf.int[1] < 2 && 2 < fit$conf.int[2])
})

test_that("did2s recovers the ATT and matches Borusyak closely", {
  df <- .dm_panel()
  f1 <- morie_did_did2s(df, "y", "id", "t", "g", n_bootstrap = 59L)
  f2 <- morie_did_borusyak(df, "y", "id", "t", "g", n_bootstrap = 29L)
  expect_lt(abs(f1$estimate - 2), 4 * f1$std.error)
  # Both are imputation-flavoured: point estimates near-identical.
  expect_lt(abs(f1$estimate - f2$estimate), 0.05)
})

test_that("cross-validation vs did2s package", {
  skip_if_not_installed("did2s")
  skip_if_not_installed("fixest")
  df <- .dm_panel(n_id = 90L)
  df$g2 <- ifelse(is.na(df$g), Inf, df$g)
  df$treat <- !is.na(df$g) & df$t >= df$g
  suppressWarnings(suppressMessages(
    ref <- did2s::did2s(df, yname = "y",
                        first_stage = ~ 0 | id + t,
                        second_stage = ~ i(treat, ref = FALSE),
                        treatment = "treat", cluster_var = "id",
                        verbose = FALSE)
  ))
  ours <- morie_did_did2s(df, "y", "id", "t", "g", n_bootstrap = 59L)
  expect_lt(abs(ours$estimate - as.numeric(stats::coef(ref)[1])), 0.02)
})

test_that("cross-validation vs didimputation package", {
  skip_if_not_installed("didimputation")
  df <- .dm_panel(n_id = 90L)
  df$g0 <- ifelse(is.na(df$g), 0, df$g)
  suppressWarnings(suppressMessages(
    ref <- didimputation::did_imputation(df, yname = "y", gname = "g0",
                                         tname = "t", idname = "id")
  ))
  ours <- morie_did_borusyak(df, "y", "id", "t", "g", n_bootstrap = 59L)
  expect_lt(abs(ours$estimate - as.numeric(ref$estimate[1])), 0.02)
})

test_that("degenerate inputs error cleanly", {
  df <- .dm_panel()
  df_none <- df
  df_none$g <- NA
  expect_error(morie_did_sun_abraham(df_none, "y", "id", "t", "g"),
               "No treated units")
  expect_error(morie_did_borusyak(df, "y", "id", "t", "missing_col"),
               "missing")
})
