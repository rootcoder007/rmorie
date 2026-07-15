# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native design-based weighted GLM vs survey::svyglm
# (module 8). Suggests allowed here ONLY.

test_that("cross: native svyglm-equivalent reproduces survey::svyglm", {
  skip_if_not_installed("survey")
  set.seed(81)
  n <- 800L
  x1 <- rnorm(n); x2 <- sample(1:4, n, TRUE)
  w <- runif(n, 0.5, 3)
  eta <- -0.5 + 0.8 * x1 + 0.2 * x2
  yb <- rbinom(n, 1, plogis(eta))
  yl <- eta + rnorm(n)
  df <- data.frame(x1, x2, yb, yl, w)

  des <- survey::svydesign(ids = ~1, weights = ~w, data = df)

  ref_b <- survey::svyglm(yb ~ x1 + x2, design = des,
                          family = stats::quasibinomial())
  our_b <- rmorie:::.morie_svyglm_native(yb ~ x1 + x2, data = df,
                                         weights = df$w,
                                         family = stats::quasibinomial())
  expect_equal(unname(our_b$coefficients[, "Estimate"]),
               unname(coef(ref_b)), tolerance = 1e-8)
  expect_equal(unname(our_b$coefficients[, "Std. Error"]),
               unname(summary(ref_b)$coefficients[, "Std. Error"]),
               tolerance = 1e-6)

  ref_l <- survey::svyglm(yl ~ x1 + x2, design = des)
  our_l <- rmorie:::.morie_svyglm_native(yl ~ x1 + x2, data = df,
                                         weights = df$w)
  expect_equal(unname(our_l$coefficients[, "Estimate"]),
               unname(coef(ref_l)), tolerance = 1e-8)
  expect_equal(unname(our_l$coefficients[, "Std. Error"]),
               unname(summary(ref_l)$coefficients[, "Std. Error"]),
               tolerance = 1e-6)
  # CI agreement within df-convention slack
  ref_ci <- suppressWarnings(stats::confint(ref_l))
  expect_equal(unname(our_l$confint), unname(ref_ci), tolerance = 5e-3)
})

test_that("cross: ebac selection IPW pipeline matches the survey-based run", {
  skip_if_not_installed("survey")
  set.seed(82)
  n <- 400L
  cpads <- data.frame(
    weight = runif(n, 0.5, 2),
    alcohol_past12m = rbinom(n, 1, 0.85),
    heavy_drinking_30d = rbinom(n, 1, 0.3),
    ebac_tot = ifelse(rbinom(n, 1, 0.7) == 1, abs(rnorm(n, 0.05, 0.03)), NA),
    ebac_legal = rbinom(n, 1, 0.7),
    cannabis_any_use = rbinom(n, 1, 0.3),
    age_group = sample(1:6, n, TRUE),
    gender = sample(1:2, n, TRUE),
    province_region = sample(1:5, n, TRUE),
    mental_health = sample(1:5, n, TRUE),
    physical_health = sample(1:5, n, TRUE)
  )
  out <- morie_run_ebac_selection_ipw_analysis(cpads)
  # reproduce the OR row with survey directly on the same frame
  obs <- out$analysis_frame
  des <- survey::svydesign(ids = ~1, weights = ~w_combined_trim, data = obs)
  ref <- survey::svyglm(
    ebac_legal ~ cannabis_any_use + age_group + gender +
      province_region + mental_health + physical_health,
    design = des, family = stats::quasibinomial())
  ref_row <- summary(ref)$coefficients["cannabis_any_use", ]
  expect_equal(out$ebac_final_ipw_or$log_odds, unname(ref_row[1]),
               tolerance = 1e-8)
  expect_equal(out$ebac_final_ipw_or$se, unname(ref_row[2]),
               tolerance = 1e-6)
})

test_that("cross: weighted logistic analysis matches svyglm directly", {
  skip_if_not_installed("survey")
  set.seed(83)
  df <- data.frame(
    y = rbinom(300, 1, 0.4),
    x1 = rnorm(300), x2 = rnorm(300),
    w = runif(300, 0.5, 1.5)
  )
  ours <- morie_run_weighted_logistic_analysis(
    df, outcome = "y", predictors = c("x1", "x2"), weights_col = "w")
  des <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  ref <- survey::svyglm(y ~ x1 + x2, design = des,
                        family = stats::quasibinomial())
  expect_equal(unname(ours$coefficients), unname(coef(ref)),
               tolerance = 1e-8)
  expect_equal(unname(ours$std_errors),
               unname(summary(ref)$coefficients[, "Std. Error"]),
               tolerance = 1e-6)
})
