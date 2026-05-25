library(testthat)

set.seed(42)
x1 <- rnorm(50, mean = 0.5, sd = 1)
x2 <- rnorm(50, mean = 0.0, sd = 1)

# ── morie_two_sample_t_test ─────────────────────────────────────────────────────────

test_that("morie_two_sample_t_test returns named list with expected fields", {
  result <- morie_two_sample_t_test(x1, x2)
  expect_named(result, c("t", "df", "p_value", "ci_diff", "morie_cohens_d"))
  expect_type(result$t, "double")
  expect_gt(abs(result$t), 0)
  expect_lt(result$p_value, 1)
})

test_that("morie_two_sample_t_test detects large effect (p < 0.05 when delta=2)", {
  a <- rnorm(100, mean = 2)
  b <- rnorm(100, mean = 0)
  r <- morie_two_sample_t_test(a, b)
  expect_lt(r$p_value, 0.05)
})

# ── morie_one_sample_t_test ─────────────────────────────────────────────────────────

test_that("morie_one_sample_t_test rejects null when mean is far from mu0", {
  result <- morie_one_sample_t_test(rnorm(100, mean = 3), mu0 = 0)
  expect_lt(result$p_value, 0.001)
})

# ── morie_chi_square_test ───────────────────────────────────────────────────────────

test_that("morie_chi_square_test on independent matrix returns chi_sq and p_value", {
  m <- matrix(c(30, 20, 20, 30), nrow = 2)
  result <- morie_chi_square_test(m)
  expect_named(result, c("chi_sq", "df", "p_value", "morie_cramers_v"))
  expect_gt(result$chi_sq, 0)
  expect_true(result$morie_cramers_v >= 0 & result$morie_cramers_v <= 1)
})

# ── morie_fisher_exact_test ─────────────────────────────────────────────────────────

test_that("morie_fisher_exact_test returns OR and CI", {
  m <- matrix(c(10, 5, 3, 15), nrow = 2)
  result <- morie_fisher_exact_test(m)
  expect_named(result, c("odds_ratio", "ci", "p_value"))
  expect_gt(result$odds_ratio, 0)
  expect_length(result$ci, 2)
})

# ── morie_anova_one_way ─────────────────────────────────────────────────────────────

test_that("morie_anova_one_way detects between-group differences", {
  result <- morie_anova_one_way(rnorm(30, 0), rnorm(30, 1), rnorm(30, 2))
  expect_named(result, c("F", "df_between", "df_within", "p_value", "morie_eta_squared"))
  expect_lt(result$p_value, 0.05)
  expect_true(result$morie_eta_squared > 0 & result$morie_eta_squared < 1)
})

# ── morie_shapiro_wilk_test ─────────────────────────────────────────────────────────

test_that("morie_shapiro_wilk_test returns is_normal = TRUE for normal data", {
  result <- morie_shapiro_wilk_test(rnorm(50))
  expect_named(result, c("W", "p_value", "is_normal"))
  expect_true(result$is_normal)
})

test_that("morie_shapiro_wilk_test flags non-normal data", {
  result <- morie_shapiro_wilk_test(rexp(50, rate = 1))
  # Exponential distribution — should often fail normality
  expect_false(result$is_normal)
})

# ── morie_proportion_ci ─────────────────────────────────────────────────────────────

test_that("morie_proportion_ci (Wilson) contains true proportion", {
  # True p = 0.3, n = 200
  ci <- morie_proportion_ci(60, 200)
  expect_true(ci$ci_lower < 0.3 & 0.3 < ci$ci_upper)
})

test_that("morie_proportion_ci methods produce bounds in [0, 1]", {
  for (m in c("wilson", "exact", "wald")) {
    r <- morie_proportion_ci(10, 100, method = m)
    expect_true(r$ci_lower >= 0 && r$ci_upper <= 1,
      info = paste("method =", m)
    )
  }
})

# ── morie_odds_ratio_ci ─────────────────────────────────────────────────────────────

test_that("morie_odds_ratio_ci returns OR > 1 when cases concentrated in exposed", {
  m <- matrix(c(40, 10, 20, 30), nrow = 2)
  r <- morie_odds_ratio_ci(m)
  expect_named(r, c("or", "ci_lower", "ci_upper", "p_value"))
  expect_gt(r$or, 1)
})

# ── morie_cohens_d ─────────────────────────────────────────────────────────────────

test_that("morie_cohens_d is near 0 for equal distributions", {
  a <- rnorm(200)
  b <- rnorm(200)
  expect_lt(abs(morie_cohens_d(a, b)), 0.3)
})

test_that("morie_cohens_d is near 1 for shift of one SD", {
  a <- rnorm(200, mean = 1)
  b <- rnorm(200, mean = 0)
  expect_lt(abs(morie_cohens_d(a, b) - 1), 0.3)
})

# ── morie_hedges_g ─────────────────────────────────────────────────────────────────

test_that("morie_hedges_g is slightly smaller than morie_cohens_d", {
  a <- rnorm(20, 1)
  b <- rnorm(20, 0)
  expect_lt(abs(morie_hedges_g(a, b)), abs(morie_cohens_d(a, b)) + 0.01)
})

# ── morie_cramers_v ────────────────────────────────────────────────────────────────

test_that("morie_cramers_v returns value in [0, 1]", {
  m <- matrix(c(50, 10, 10, 50), nrow = 2)
  v <- morie_cramers_v(m)
  expect_true(v >= 0 && v <= 1)
})

test_that("morie_cramers_v is near 0 for independent table", {
  m <- matrix(c(25, 25, 25, 25), nrow = 2)
  expect_lt(morie_cramers_v(m), 0.1)
})

# ── morie_power_t_test ──────────────────────────────────────────────────────────────

test_that("morie_power_t_test solves for n correctly", {
  result <- morie_power_t_test(
    n = NULL, delta = 0.5, sd = 1,
    sig_level = 0.05, power = 0.80
  )
  # Expected n ≈ 64 per group
  expect_true(result$n >= 50 && result$n <= 80)
})

test_that("morie_power_t_test solves for power correctly", {
  result <- morie_power_t_test(
    n = 100, delta = 0.5, sd = 1,
    sig_level = 0.05, power = NULL
  )
  expect_gt(result$power, 0.80)
})

# ── morie_sample_size_logistic ──────────────────────────────────────────────────────

test_that("morie_sample_size_logistic returns positive integer", {
  n <- morie_sample_size_logistic(p0 = 0.3, or = 2.0, power = 0.80)
  expect_type(n, "integer")
  expect_gt(n, 0)
})
