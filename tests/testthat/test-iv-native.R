# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 17 — structural tests for the native IV + ITS engines.
#' @srrstats {G5.4} 2SLS/LIML/GMM recover known structural parameters
#'   on endogenous DGPs; ITS recovers known level/slope changes.

.mk_iv <- function(n = 1500, beta = 0.8, seed = 50, k_z = 2) {
  set.seed(seed)
  x <- rnorm(n)
  z <- matrix(rnorm(n * k_z), n, k_z)
  u <- rnorm(n)                       # endogeneity
  d <- 0.6 * rowSums(z) + 0.4 * x + u + rnorm(n)
  y <- beta * d + 0.5 * x + 0.8 * u + rnorm(n)
  df <- data.frame(y = y, d = d, x = x)
  for (j in seq_len(k_z)) df[[paste0("z", j)]] <- z[, j]
  df
}

test_that("native 2SLS beats OLS on an endogenous DGP", {
  df <- .mk_iv(seed = 51)
  iv <- morie_iv_tsls(df, "y", "d", c("z1", "z2"), exogenous = "x")
  ols <- stats::coef(stats::lm(y ~ d + x, data = df))[["d"]]
  expect_match(iv$method, "rmorie native")
  expect_equal(unname(iv$coefficients[["d"]]), 0.8, tolerance = 0.1)
  expect_gt(abs(ols - 0.8), abs(iv$coefficients[["d"]] - 0.8))
  # robust and classical SEs both finite
  iv2 <- morie_iv_tsls(df, "y", "d", c("z1", "z2"), exogenous = "x",
                       robust = FALSE)
  expect_equal(unname(iv2$coefficients[["d"]]),
               unname(iv$coefficients[["d"]]), tolerance = 1e-12)
  expect_true(all(is.finite(iv$std_errors)))
})

test_that("native LIML: kappa >= 1, close to 2SLS when strongly identified", {
  df <- .mk_iv(seed = 52)
  li <- morie_iv_liml(df, "y", "d", c("z1", "z2"), exogenous = "x")
  expect_match(li$method, "rmorie native")
  expect_gte(li$details$kappa, 1 - 1e-8)
  expect_equal(unname(li$coefficients[["d"]]), 0.8, tolerance = 0.1)
  # just-identified: LIML == 2SLS exactly
  ji <- .mk_iv(seed = 53, k_z = 1)
  li1 <- morie_iv_liml(ji, "y", "d", "z1", exogenous = "x")
  iv1 <- morie_iv_tsls(ji, "y", "d", "z1", exogenous = "x")
  expect_equal(unname(li1$coefficients[["d"]]),
               unname(iv1$coefficients[["d"]]), tolerance = 1e-6)
})

test_that("native GMM two-step + CUE + Hansen J behave", {
  df <- .mk_iv(seed = 54)
  gm <- morie_iv_gmm(df, "y", "d", c("z1", "z2"), exogenous = "x")
  expect_match(gm$method, "rmorie native")
  expect_equal(unname(gm$coefficients[["d"]]), 0.8, tolerance = 0.1)
  cue <- morie_iv_cue_gmm(df, "y", "d", c("z1", "z2"), exogenous = "x")
  expect_equal(unname(cue$coefficients[["d"]]),
               unname(gm$coefficients[["d"]]), tolerance = 0.05)
  j <- morie_iv_hansen_j(df, "y", "d", c("z1", "z2"), exogenous = "x")
  expect_match(j$name, "rmorie native")
  expect_equal(j$df, 1L)
  expect_gt(j$p_value, 0.01)  # instruments are valid by construction
})

test_that("morie_estimate_late covariate path uses the native engine", {
  set.seed(55)
  n <- 4000
  z <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  t <- rbinom(n, 1, plogis(-1.2 + 2.5 * z + 0.3 * x))
  y <- 0.8 * t + 0.5 * x + rnorm(n)
  df <- data.frame(t = t, y = y, z = z, x = x)
  res <- morie_estimate_late(df, "t", "y", "z", covariates = "x")
  expect_equal(unname(res$late), 0.8, tolerance = 0.3)
  expect_true(is.finite(res$se) && res$se > 0)
})

test_that("morie_its recovers level and slope changes with HAC SEs", {
  set.seed(56)
  n <- 120; t0 <- 80
  t <- seq_len(n)
  e <- as.numeric(stats::arima.sim(list(ar = 0.4), n = n, sd = 0.8))
  y <- 10 + 0.2 * t + ifelse(t >= t0, 4, 0) +
    ifelse(t >= t0, 0.3 * (t - t0), 0) + e
  fit <- morie_its(data.frame(tt = t, y = y), "y", "tt",
                   interruption_time = t0)
  expect_equal(fit$level_change$estimate, 4, tolerance = 1)
  expect_equal(fit$slope_change$estimate, 0.3, tolerance = 0.15)
  expect_lt(fit$level_change$p_value, 0.01)
  expect_equal(fit$n_pre, t0 - 1)
  expect_equal(nrow(fit$counterfactual), n)
  # counterfactual = fitted in the pre-period
  pre <- fit$counterfactual[!fit$counterfactual$post, ]
  expect_equal(pre$fitted, pre$no_intervention, tolerance = 1e-10)
  expect_error(morie_its(data.frame(tt = 1:5, y = rnorm(5)), "y", "tt",
                         interruption_time = 5), "post-interruption")
})
