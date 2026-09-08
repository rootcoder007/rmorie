# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 14 unified front-ends: routing correctness + cross-validation
# against did / AER / rdrobust on known-truth DGPs.

.qx_panel <- function(staggered = TRUE, n_id = 60L, n_t = 8L,
                      tau = 2, seed = 7) {
  set.seed(seed)
  df <- expand.grid(id = seq_len(n_id), t = seq_len(n_t))
  df$g <- if (staggered) {
    ifelse(df$id <= n_id / 3, 4L, ifelse(df$id <= 2 * n_id / 3, 6L, NA))
  } else {
    ifelse(df$id <= n_id / 2, 4L, NA)
  }
  unit_fe <- rnorm(n_id)[df$id]
  time_fe <- (seq_len(n_t) * 0.3)[df$t]
  df$y <- unit_fe + time_fe +
    ifelse(!is.na(df$g) & df$t >= df$g, tau, 0) + rnorm(nrow(df), sd = 0.5)
  df
}

test_that("morie_did auto-detects staggered adoption and warns", {
  df <- .qx_panel(staggered = TRUE)
  expect_warning(fit <- morie_did(df, "y", "id", "t", "g"),
                 "Staggered adoption")
  expect_s3_class(fit, "morie_did")
  expect_match(fit$method, "Callaway")
  expect_false(is.null(fit$att_gt))
  # Recovers the simulated tau = 2.
  expect_lt(abs(fit$estimate - 2), 4 * fit$std.error)
})

test_that("morie_did uses TWFE for uniform timing (no warning)", {
  df <- .qx_panel(staggered = FALSE)
  expect_no_warning(fit <- morie_did(df, "y", "id", "t", "g"))
  expect_match(fit$method, "TWFE")
  expect_null(fit$att_gt)
  expect_lt(abs(fit$estimate - 2), 4 * fit$std.error)
})

test_that("morie_did staggered overall ATT matches did::att_gt aggregation", {
  skip_if_not_installed("did")
  df <- .qx_panel(staggered = TRUE, n_id = 90L)
  df$g_did <- ifelse(is.na(df$g), 0L, df$g)
  suppressWarnings({
    ref <- did::att_gt(yname = "y", tname = "t", idname = "id",
                       gname = "g_did", data = df, est_method = "reg",
                       bstrap = FALSE, cband = FALSE)
    ref_overall <- did::aggte(ref, type = "simple", bstrap = FALSE,
                              cband = FALSE)
    fit <- morie_did(df, "y", "id", "t", "g", n_bootstrap = 0L)
  })
  expect_lt(abs(fit$estimate - ref_overall$overall.att), 0.1)
})

test_that("morie_iv_2sls matches AER::ivreg on strong instruments", {
  set.seed(11)
  n <- 400
  z <- rnorm(n)
  u <- rnorm(n)
  x1 <- rnorm(n)
  d <- 1.2 * z + 0.5 * u + 0.3 * x1 + rnorm(n)
  y <- 2 * d + u + 0.7 * x1 + rnorm(n)
  df <- data.frame(y, d, z, x1)
  fit <- morie_iv_2sls(df, "y", "d", "z", exogenous = "x1")
  expect_s3_class(fit, "morie_iv")
  expect_false(fit$weak_instruments)
  expect_gt(fit$first_stage_F, 10)
  expect_equal(fit$stock_yogo_10, 16.38)
  expect_lt(abs(fit$estimate - 2), 4 * fit$std.error)

  skip_if_not_installed("AER")
  ref <- AER::ivreg(y ~ d + x1 | z + x1, data = df)
  expect_lt(abs(fit$estimate - unname(stats::coef(ref)["d"])), 1e-6)
})

test_that("morie_iv_2sls refuses the point estimate on weak instruments", {
  set.seed(12)
  n <- 300
  z <- rnorm(n)
  u <- rnorm(n)
  d <- 0.05 * z + u + rnorm(n) # weak first stage
  y <- 2 * d + u + rnorm(n)
  df <- data.frame(y, d, z)
  expect_warning(fit <- morie_iv_2sls(df, "y", "d", "z"),
                 "refusing")
  expect_true(fit$weak_instruments)
  expect_true(is.na(fit$estimate))
  # AR set still reported (may be wide or empty, but present).
  expect_length(fit$ar_confidence_set, 2L)
})

test_that("morie_rdd bundles estimate + manipulation + placebo (sharp)", {
  set.seed(13)
  n <- 800
  x <- runif(n, -1, 1)
  y <- 1 + 2.5 * (x >= 0) + x + 0.5 * x^2 + rnorm(n, sd = 0.4)
  df <- data.frame(y, x)
  fit <- morie_rdd(df, "y", "x")
  expect_s3_class(fit, "morie_rdd")
  expect_equal(fit$kind, "sharp")
  expect_lt(abs(fit$estimate - 2.5), 4 * fit$std.error)
  expect_true(is.finite(fit$bandwidth) && fit$bandwidth > 0)
  # No manipulation in a uniform running variable.
  expect_gt(fit$manipulation$p.value, 0.01)
  expect_true(is.data.frame(fit$placebo) && nrow(fit$placebo) == 2L)
})

test_that("morie_rdd sharp estimate is near rdrobust's conventional", {
  skip_if_not_installed("rdrobust")
  set.seed(14)
  n <- 1000
  x <- runif(n, -1, 1)
  y <- 1 + 2 * (x >= 0) + x + rnorm(n, sd = 0.4)
  fit <- morie_rdd(data.frame(y, x), "y", "x")
  ref <- rdrobust::rdrobust(y, x, c = 0)
  # Different bandwidth selectors -> comparable, not identical.
  expect_lt(abs(fit$estimate - ref$coef[1]),
            3 * (fit$std.error + ref$se[1]))
})

test_that("morie_rdd fuzzy path routes and scales the jump", {
  set.seed(15)
  n <- 900
  x <- runif(n, -1, 1)
  d <- rbinom(n, 1, ifelse(x >= 0, 0.8, 0.2)) # imperfect compliance
  y <- 1 + 2 * d + x + rnorm(n, sd = 0.4)
  fit <- morie_rdd(data.frame(y, x, d), "y", "x", treatment = "d")
  expect_equal(fit$kind, "fuzzy")
  # LATE ~ 2 (jump in y ~ 1.2 divided by jump in d ~ 0.6).
  expect_lt(abs(fit$estimate - 2), 5 * fit$std.error)
})
