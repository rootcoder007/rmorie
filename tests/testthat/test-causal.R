library(testthat)

# Shared synthetic data for causal tests
make_df <- function(n = 300, seed = 42) {
  set.seed(seed)
  x <- rnorm(n)
  ps_true <- 1 / (1 + exp(-0.5 * x))
  t <- rbinom(n, 1, ps_true)
  y <- 0.3 * t + 0.5 * x + rnorm(n, sd = 0.5)
  data.frame(
    t = t, y = y, x = x,
    g = sample(c("A", "B"), n, replace = TRUE)
  )
}

df <- make_df()

# ── morie_estimate_propensity_scores ──────────────────────────────────────────────

test_that("morie_estimate_propensity_scores returns values in (0, 1)", {
  ps <- morie_estimate_propensity_scores(df, "t", "x")
  expect_true(all(ps > 0 & ps < 1))
  expect_length(ps, nrow(df))
})

# ── morie_estimate_ate ─────────────────────────────────────────────────────────────

test_that("morie_estimate_ate returns a named list with expected fields", {
  result <- morie_estimate_ate(df, "t", "y", "x")
  expect_named(result, c("ate", "se", "ci_lower", "ci_upper", "n", "ess"))
  expect_type(result$ate, "double")
  expect_true(result$ci_lower < result$ci_upper)
})

test_that("morie_estimate_ate point estimate is near 0.3 (true ATE)", {
  result <- morie_estimate_ate(df, "t", "y", "x")
  expect_lt(abs(result$ate - 0.3), 0.15)
})

# ── morie_estimate_att ─────────────────────────────────────────────────────────────

test_that("morie_estimate_att returns expected fields", {
  result <- morie_estimate_att(df, "t", "y", "x")
  expect_named(result, c("att", "se", "ci_lower", "ci_upper", "n_treated"))
  expect_gt(result$n_treated, 0)
  expect_true(result$ci_lower < result$ci_upper)
})

# ── morie_estimate_atc ─────────────────────────────────────────────────────────────

test_that("morie_estimate_atc returns expected fields", {
  result <- morie_estimate_atc(df, "t", "y", "x")
  expect_named(result, c("atc", "se", "ci_lower", "ci_upper", "n_control"))
  expect_gt(result$n_control, 0)
})

# ── morie_estimate_aipw ────────────────────────────────────────────────────────────

test_that("morie_estimate_aipw returns ATE close to 0.3", {
  result <- morie_estimate_aipw(df, "t", "y", "x")
  expect_named(result, c("ate", "se", "ci_lower", "ci_upper", "n"))
  expect_lt(abs(result$ate - 0.3), 0.15)
})

test_that("AIPW CI contains the true ATE (0.3) in most simulations", {
  # Re-run 10 simulations; > 8/10 should cover 0.3
  cover <- replicate(10, {
    d <- make_df(seed = sample.int(1e6, 1))
    r <- morie_estimate_aipw(d, "t", "y", "x")
    r$ci_lower < 0.3 & 0.3 < r$ci_upper
  })
  expect_gt(mean(cover), 0.7)
})

# ── morie_estimate_gate ────────────────────────────────────────────────────────────

test_that("morie_estimate_gate returns a data frame with group column", {
  result <- morie_estimate_gate(df, "t", "y", "x", "g")
  expect_s3_class(result, "data.frame")
  expect_true("group" %in% names(result))
  expect_true("ate" %in% names(result))
  expect_equal(nrow(result), 2) # Two groups: A and B
})

# ── morie_estimate_cate ─────────────────────────────────────────────────────────────

test_that("morie_estimate_cate (T-learner) returns n values", {
  cate <- morie_estimate_cate(df, "t", "y", "x", meta_learner = "t_learner")
  expect_length(cate, nrow(df))
  expect_type(cate, "double")
})

test_that("morie_estimate_cate (S-learner) returns n values", {
  cate <- morie_estimate_cate(df, "t", "y", "x", meta_learner = "s_learner")
  expect_length(cate, nrow(df))
})

test_that("mean CATE is approximately the ATE", {
  cate <- morie_estimate_cate(df, "t", "y", "x")
  expect_lt(abs(mean(cate) - 0.3), 0.2)
})

# ── morie_estimate_late ─────────────────────────────────────────────────────────────

test_that("morie_estimate_late (Wald) returns LATE near 0.3", {
  set.seed(99)
  n <- 500
  z <- rbinom(n, 1, 0.5) # instrument
  t <- rbinom(n, 1, 0.2 + 0.5 * z) # compliance rate 50%
  y <- 0.3 * t + rnorm(n, sd = 0.5)
  d <- data.frame(t = t, y = y, z = z)
  result <- morie_estimate_late(d, "t", "y", "z")
  expect_named(result, c(
    "late", "se", "ci_lower", "ci_upper",
    "first_stage_f", "n"
  ))
  expect_lt(abs(result$late - 0.3), 0.3)
  expect_gt(result$first_stage_f, 4) # relevant instrument
})

# ── morie_e_value ───────────────────────────────────────────────────────────────────

test_that("morie_e_value matches reference (RR = 3.9 → E ≈ 7.26)", {
  r <- morie_e_value(3.9)
  expect_lt(abs(r$morie_e_value - (3.9 + sqrt(3.9 * 2.9))), 0.01)
})

test_that("morie_e_value for CI bound is computed when rr_lower provided", {
  r <- morie_e_value(3.9, rr_lower = 2.4)
  expect_false(is.na(r$e_value_ci))
})

# ── morie_estimate_g_computation ────────────────────────────────────────────────────

test_that("morie_estimate_g_computation returns ATE near 0.3", {
  result <- morie_estimate_g_computation(df, "t", "y", "x")
  expect_named(result, c("ate", "se", "ci_lower", "ci_upper"))
  expect_lt(abs(result$ate - 0.3), 0.15)
})

# ── morie_sensitivity_rosenbaum ─────────────────────────────────────────────────────

test_that("morie_sensitivity_rosenbaum returns data frame with expected columns", {
  treated <- rnorm(50, mean = 0.3)
  control <- rnorm(50, mean = 0.0)
  result <- morie_sensitivity_rosenbaum(treated, control,
    gamma_range = c(1.0, 1.5, 2.0)
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("gamma", "p_lower", "p_upper") %in% names(result)))
  expect_true(all(result$gamma == c(1.0, 1.5, 2.0)))
})
