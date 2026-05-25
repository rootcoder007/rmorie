# SPDX-License-Identifier: AGPL-3.0-or-later

# Comprehensive tests for the MRM statistics modules:
#   R/mrm_design.R, R/mrm_doe.R, R/mrm_mathstats.R, R/mrm_diagnostics.R

# ---------------------------------------------------------------------------
# mrm_design.R : mrm_two_treatment_test
# ---------------------------------------------------------------------------

test_that("mrm_two_treatment_test returns the documented structure", {
  set.seed(2026)
  a <- rnorm(40, mean = 5, sd = 1.2)
  b <- rnorm(40, mean = 5.5, sd = 1.5)
  res <- mrm_two_treatment_test(a, b)

  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "se", "t_statistic", "df",
    "p_welch", "p_student", "p_mannwhitney",
    "ci_lower", "ci_upper", "n_a", "n_b",
    "interpretation"
  ))
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_gte(res$se, 0)
  expect_true(is.finite(res$t_statistic))
  expect_gte(res$p_welch, 0)
  expect_lte(res$p_welch, 1)
  expect_gte(res$p_student, 0)
  expect_lte(res$p_student, 1)
  expect_gte(res$p_mannwhitney, 0)
  expect_lte(res$p_mannwhitney, 1)
  expect_lte(res$ci_lower, res$ci_upper)
  expect_equal(res$n_a, 40L)
  expect_equal(res$n_b, 40L)
  expect_type(res$interpretation, "character")
})

test_that("mrm_two_treatment_test honours a non-default alpha and drops non-finite values", {
  set.seed(7)
  a <- c(rnorm(30, 0, 1), NA, Inf)
  b <- c(rnorm(30, 0.4, 1), NaN)
  res <- mrm_two_treatment_test(a, b, alpha = 0.10)
  expect_equal(res$n_a, 30L)
  expect_equal(res$n_b, 30L)
  expect_lte(res$ci_lower, res$ci_upper)
})

# ---------------------------------------------------------------------------
# mrm_design.R : mrm_anova_oneway
# ---------------------------------------------------------------------------

test_that("mrm_anova_oneway returns F-test plus Tukey HSD", {
  set.seed(2026)
  n <- 30L
  df <- data.frame(
    y = c(rnorm(n, 0), rnorm(n, 0.5), rnorm(n, 1)),
    g = rep(c("A", "B", "C"), each = n)
  )
  res <- mrm_anova_oneway(df, response_col = "y", group_col = "g")

  expect_true(is.list(res))
  expect_named(res, c(
    "f_statistic", "p_value", "df_between", "df_within",
    "means", "n_per_group", "tukey_hsd", "interpretation"
  ))
  expect_true(is.finite(res$f_statistic))
  expect_gte(res$f_statistic, 0)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$df_between, 2)
  expect_equal(res$df_within, 87)
  expect_length(res$means, 3)
  expect_length(res$n_per_group, 3)
  expect_s3_class(res$tukey_hsd, "data.frame")
  expect_true("pair" %in% names(res$tukey_hsd))
  expect_equal(nrow(res$tukey_hsd), 3L)
})

test_that("mrm_anova_oneway tolerates incomplete rows", {
  set.seed(11)
  n <- 20L
  df <- data.frame(
    y = c(rnorm(n, 0), rnorm(n, 1)),
    g = rep(c("A", "B"), each = n)
  )
  df$y[1] <- NA
  res <- mrm_anova_oneway(df, response_col = "y", group_col = "g")
  expect_true(is.finite(res$f_statistic))
  expect_s3_class(res$tukey_hsd, "data.frame")
})

# ---------------------------------------------------------------------------
# mrm_design.R : mrm_factorial_2k
# ---------------------------------------------------------------------------

test_that("mrm_factorial_2k computes main and interaction effects", {
  set.seed(2026)
  lvl <- c(-1, 1)
  df <- expand.grid(A = lvl, B = lvl, C = lvl)
  df$y <- 10 + 2 * df$A + 1.5 * df$B + 0.5 * df$A * df$B + rnorm(8, 0, 0.2)
  res <- mrm_factorial_2k(df,
    response_col = "y",
    factor_cols = c("A", "B", "C")
  )

  expect_true(is.list(res))
  expect_named(res, c(
    "main_effects", "interaction_effects",
    "half_normal_coords", "n", "k", "interpretation"
  ))
  expect_length(res$main_effects, 3)
  expect_true(all(vapply(res$main_effects, is.finite, logical(1))))
  expect_length(res$interaction_effects, 4)
  expect_s3_class(res$half_normal_coords, "data.frame")
  expect_named(
    res$half_normal_coords,
    c(
      "effect_name", "effect_magnitude",
      "quantile", "half_normal_quantile"
    )
  )
  expect_equal(nrow(res$half_normal_coords), 7L)
  expect_equal(res$n, 8L)
  expect_equal(res$k, 3L)
  expect_true(all(res$half_normal_coords$effect_magnitude >= 0))
  expect_true(all(res$half_normal_coords$quantile > 0 &
    res$half_normal_coords$quantile < 1))
})

test_that("mrm_factorial_2k re-codes non-(-1,1) factor columns", {
  set.seed(5)
  df <- expand.grid(A = c(0, 1), B = c(0, 1))
  df <- df[rep(seq_len(4), 3), ]
  df$y <- 1 + df$A + 0.5 * df$B + rnorm(nrow(df), 0, 0.1)
  res <- mrm_factorial_2k(df,
    response_col = "y",
    factor_cols = c("A", "B")
  )
  expect_equal(res$k, 2L)
  expect_length(res$main_effects, 2)
})

test_that("mrm_factorial_2k errors when fewer than 2 factors", {
  df <- data.frame(A = c(-1, 1), y = c(0, 1))
  expect_error(mrm_factorial_2k(df, response_col = "y", factor_cols = "A"))
})

# ---------------------------------------------------------------------------
# mrm_design.R : mrm_causal_design
# ---------------------------------------------------------------------------

test_that("mrm_causal_design IPW estimator returns documented fields", {
  set.seed(2026)
  n <- 200L
  x <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.5 * x))
  y <- 0.7 * D + 0.3 * x + rnorm(n, 0, 0.5)
  df <- data.frame(D = D, y = y, age = x)
  res <- mrm_causal_design(df,
    treatment_col = "D", outcome_col = "y",
    covariates = "age", estimator = "ipw"
  )

  expect_true(is.list(res))
  expect_named(res, c(
    "estimator", "estimate", "se", "ci_lower",
    "ci_upper", "p_value", "n", "n_treated",
    "interpretation"
  ))
  expect_equal(res$estimator, "ipw")
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_gte(res$se, 0)
  expect_lte(res$ci_lower, res$ci_upper)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$n, 200L)
  expect_gt(res$n_treated, 0L)
  expect_lt(res$n_treated, 200L)
})

test_that("mrm_causal_design diff_in_means estimator works", {
  set.seed(2026)
  n <- 200L
  x <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.5 * x))
  y <- 0.7 * D + 0.3 * x + rnorm(n, 0, 0.5)
  df <- data.frame(D = D, y = y, age = x)
  res <- mrm_causal_design(df,
    treatment_col = "D", outcome_col = "y",
    estimator = "diff_in_means"
  )
  expect_equal(res$estimator, "diff_in_means")
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_lte(res$ci_lower, res$ci_upper)
})

test_that("mrm_causal_design with ipw but no covariates falls back to diff path", {
  set.seed(3)
  n <- 120L
  D <- rbinom(n, 1, 0.5)
  y <- 0.5 * D + rnorm(n)
  df <- data.frame(D = D, y = y)
  res <- mrm_causal_design(df,
    treatment_col = "D", outcome_col = "y",
    estimator = "ipw"
  )
  expect_equal(res$estimator, "ipw")
  expect_true(is.finite(res$estimate))
})

test_that("mrm_causal_design rejects an unknown estimator", {
  df <- data.frame(D = c(0, 1, 0, 1), y = c(1, 2, 1.5, 2.5))
  expect_error(mrm_causal_design(df,
    treatment_col = "D", outcome_col = "y",
    estimator = "bogus"
  ))
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_anova_bonferroni
# ---------------------------------------------------------------------------

test_that("mrm_anova_bonferroni returns ANOVA plus pairwise table", {
  set.seed(2026)
  n <- 30L
  df <- data.frame(
    y = c(rnorm(n, 0), rnorm(n, 0.5), rnorm(n, 1)),
    g = rep(c("A", "B", "C"), each = n)
  )
  res <- mrm_anova_bonferroni(df, response_col = "y", group_col = "g")

  expect_true(is.list(res))
  expect_named(res, c(
    "f_statistic", "p_value", "n_groups", "n_pairs",
    "alpha", "alpha_per_pair", "pairs", "interpretation"
  ))
  expect_true(is.finite(res$f_statistic))
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$n_groups, 3L)
  expect_equal(res$n_pairs, 3L)
  expect_equal(res$alpha, 0.05)
  expect_equal(res$alpha_per_pair, 0.05 / 3)
  expect_s3_class(res$pairs, "data.frame")
  expect_true(all(c(
    "group_a", "group_b", "diff", "t", "p_raw",
    "p_bonferroni", "significant"
  ) %in% names(res$pairs)))
  expect_true(all(res$pairs$p_bonferroni >= 0 & res$pairs$p_bonferroni <= 1))
  expect_type(res$pairs$significant, "logical")
})

test_that("mrm_anova_bonferroni honours a custom alpha", {
  set.seed(9)
  n <- 25L
  df <- data.frame(
    y = c(rnorm(n, 0), rnorm(n, 2)),
    g = rep(c("A", "B"), each = n)
  )
  res <- mrm_anova_bonferroni(df,
    response_col = "y", group_col = "g",
    alpha = 0.01
  )
  expect_equal(res$alpha, 0.01)
  expect_equal(res$n_pairs, 1L)
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_rcbd
# ---------------------------------------------------------------------------

test_that("mrm_rcbd fits a randomised complete block design", {
  set.seed(2026)
  df <- expand.grid(
    treatment = c("A", "B", "C"),
    block = c("B1", "B2", "B3", "B4")
  )
  df$y <- as.numeric(df$treatment) * 2 +
    as.numeric(df$block) * 0.5 + rnorm(nrow(df), 0, 0.3)
  res <- mrm_rcbd(df,
    response_col = "y",
    treatment_col = "treatment", block_col = "block"
  )

  expect_true(is.list(res))
  expect_named(res, c(
    "anova", "n", "n_treatments", "n_blocks",
    "interpretation"
  ))
  expect_s3_class(res$anova, "data.frame")
  expect_true("source" %in% names(res$anova))
  expect_equal(res$n, 12L)
  expect_equal(res$n_treatments, 3L)
  expect_equal(res$n_blocks, 4L)
  expect_type(res$interpretation, "character")
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_latin_square
# ---------------------------------------------------------------------------

test_that("mrm_latin_square fits a three-way Latin-square ANOVA", {
  sq <- mrm_random_latin(k = 4, seed = 2026)
  df <- expand.grid(row = paste0("R", 1:4), col = paste0("C", 1:4))
  df$treatment <- LETTERS[as.integer(as.vector(sq)) + 1L]
  set.seed(2026)
  df$y <- match(df$treatment, LETTERS) * 1.5 + rnorm(16, 0, 0.4)
  res <- mrm_latin_square(df,
    response_col = "y",
    row_col = "row", col_col = "col",
    treatment_col = "treatment"
  )

  expect_true(is.list(res))
  expect_named(res, c("anova", "n", "k", "interpretation"))
  expect_s3_class(res$anova, "data.frame")
  expect_true("source" %in% names(res$anova))
  expect_equal(res$n, 16L)
  expect_equal(res$k, 4L)
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_graeco_latin
# ---------------------------------------------------------------------------

test_that("mrm_graeco_latin fits a four-way Graeco-Latin ANOVA", {
  L <- matrix(c(
    "A", "B", "C", "D",
    "B", "A", "D", "C",
    "C", "D", "A", "B",
    "D", "C", "B", "A"
  ), nrow = 4L, byrow = TRUE)
  G <- matrix(c(
    "a", "b", "c", "d",
    "c", "d", "a", "b",
    "d", "c", "b", "a",
    "b", "a", "d", "c"
  ), nrow = 4L, byrow = TRUE)
  set.seed(2026)
  df <- expand.grid(row = paste0("R", 1:4), col = paste0("C", 1:4))
  df$latin <- as.vector(L)
  df$greek <- as.vector(G)
  df$y <- match(df$latin, LETTERS) * 1.2 +
    match(df$greek, letters) * 0.5 + rnorm(16, 0, 0.3)
  res <- mrm_graeco_latin(df,
    response_col = "y",
    row_col = "row", col_col = "col",
    latin_col = "latin", greek_col = "greek"
  )

  expect_true(is.list(res))
  expect_named(res, c("anova", "n", "interpretation"))
  expect_s3_class(res$anova, "data.frame")
  expect_true("source" %in% names(res$anova))
  expect_equal(res$n, 16L)
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_fractional_factorial
# ---------------------------------------------------------------------------

test_that("mrm_fractional_factorial computes main effects without a generator", {
  set.seed(2026)
  df <- data.frame(
    A = c(-1, 1, -1, 1),
    B = c(-1, -1, 1, 1),
    C = c(1, -1, -1, 1)
  )
  df$y <- 5 + 2 * df$A + 1.5 * df$B + rnorm(4, 0, 0.3)
  res <- mrm_fractional_factorial(df,
    response_col = "y",
    factor_cols = c("A", "B", "C")
  )

  expect_true(is.list(res))
  expect_named(res, c(
    "main_effects", "alias_structure", "n", "k",
    "interpretation"
  ))
  expect_length(res$main_effects, 3)
  expect_true(all(vapply(res$main_effects, is.finite, logical(1))))
  expect_length(res$alias_structure, 0)
  expect_equal(res$n, 4L)
  expect_equal(res$k, 3L)
})

test_that("mrm_fractional_factorial parses a generator string into aliases", {
  set.seed(13)
  df <- data.frame(
    A = c(-1, 1, -1, 1),
    B = c(-1, -1, 1, 1),
    C = c(1, -1, -1, 1)
  )
  df$y <- 3 + df$A + df$B + rnorm(4, 0, 0.2)
  res <- mrm_fractional_factorial(df,
    response_col = "y",
    factor_cols = c("A", "B", "C"),
    generator = "C=AB"
  )
  expect_equal(length(res$alias_structure), 1L)
  expect_true("C" %in% names(res$alias_structure))
  expect_equal(res$alias_structure[["C"]], "AB")
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_response_surface
# ---------------------------------------------------------------------------

test_that("mrm_response_surface fits a second-order model and a stationary point", {
  set.seed(2026)
  df <- expand.grid(
    x1 = c(-1.4, -1, 0, 1, 1.4),
    x2 = c(-1.4, -1, 0, 1, 1.4)
  )
  df$y <- 10 + 2 * df$x1 + 1.5 * df$x2 -
    df$x1^2 - 1.2 * df$x2^2 + rnorm(nrow(df), 0, 0.2)
  res <- mrm_response_surface(df,
    response_col = "y",
    factor_cols = c("x1", "x2")
  )

  expect_true(is.list(res))
  expect_named(res, c(
    "coefficients", "stationary_point", "stationary_y",
    "stationary_nature", "eigenvalues", "n",
    "interpretation"
  ))
  expect_true(is.list(res$coefficients))
  expect_length(res$coefficients, 6)
  expect_true(res$stationary_nature %in% c("maximum", "minimum", "saddle"))
  expect_length(res$eigenvalues, 2)
  expect_true(all(is.finite(res$eigenvalues)))
  expect_equal(res$n, 25L)
  expect_equal(res$stationary_nature, "maximum")
  expect_length(res$stationary_point, 2)
  expect_true(is.finite(res$stationary_y))
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_anova_power
# ---------------------------------------------------------------------------

test_that("mrm_anova_power computes a valid power value", {
  res <- mrm_anova_power(
    k_groups = 4, n_per_group = 30,
    effect_size_f = 0.25, alpha = 0.05
  )
  expect_true(is.list(res))
  expect_named(res, c(
    "k_groups", "n_per_group", "N_total", "effect_size_f",
    "alpha", "df1", "df2", "noncentrality", "F_critical",
    "power", "interpretation"
  ))
  expect_equal(res$k_groups, 4L)
  expect_equal(res$n_per_group, 30L)
  expect_equal(res$N_total, 120L)
  expect_equal(res$df1, 3L)
  expect_equal(res$df2, 116L)
  expect_gte(res$power, 0)
  expect_lte(res$power, 1)
  expect_gt(res$F_critical, 0)
  expect_gte(res$noncentrality, 0)
})

test_that("mrm_anova_power is monotone increasing in sample size", {
  powers <- vapply(c(10, 20, 30, 50, 100), function(n) {
    mrm_anova_power(
      k_groups = 3, n_per_group = n,
      effect_size_f = 0.25
    )$power
  }, numeric(1))
  expect_true(all(is.finite(powers)))
  expect_true(all(diff(powers) >= 0))
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_mc_power
# ---------------------------------------------------------------------------

test_that("mrm_mc_power estimates empirical power from a simulator", {
  my_sim <- function(seed) {
    set.seed(seed)
    x <- rnorm(30, mean = 0.4, sd = 1)
    stats::t.test(x, mu = 0)$p.value
  }
  res <- mrm_mc_power(my_sim, n_sims = 200L, alpha = 0.05)
  expect_true(is.list(res))
  expect_named(res, c(
    "n_sims", "alpha", "empirical_power", "se",
    "ci95_lower", "ci95_upper", "interpretation"
  ))
  expect_equal(res$n_sims, 200L)
  expect_gte(res$empirical_power, 0)
  expect_lte(res$empirical_power, 1)
  expect_gte(res$se, 0)
  expect_gte(res$ci95_lower, 0)
  expect_lte(res$ci95_upper, 1)
  expect_lte(res$ci95_lower, res$ci95_upper)
})

test_that("mrm_mc_power is reproducible for a fixed outer seed", {
  sim <- function(seed) {
    set.seed(seed)
    stats::t.test(rnorm(20, 0.5))$p.value
  }
  r1 <- mrm_mc_power(sim, n_sims = 100L, seed = 99L)
  r2 <- mrm_mc_power(sim, n_sims = 100L, seed = 99L)
  expect_equal(r1$empirical_power, r2$empirical_power)
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_perm_block
# ---------------------------------------------------------------------------

test_that("mrm_perm_block runs a within-block permutation test", {
  set.seed(2026)
  df <- expand.grid(
    block = paste0("B", 1:6),
    treatment = c("ctrl", "drug")
  )
  df$y <- as.numeric(df$block) * 1.2 +
    ifelse(df$treatment == "drug", 0.7, 0) +
    rnorm(nrow(df), 0, 0.4)
  res <- mrm_perm_block(df,
    response_col = "y",
    treatment_col = "treatment",
    block_col = "block", n_perm = 300L
  )

  expect_true(is.list(res))
  expect_named(res, c(
    "observed_statistic", "n_perm", "p_value",
    "interpretation"
  ))
  expect_true(is.finite(res$observed_statistic))
  expect_equal(res$n_perm, 300L)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
})

test_that("mrm_perm_block is reproducible for a fixed seed", {
  set.seed(1)
  df <- expand.grid(
    block = paste0("B", 1:5),
    treatment = c("ctrl", "drug")
  )
  df$y <- rnorm(nrow(df))
  r1 <- mrm_perm_block(df, "y", "treatment", "block", n_perm = 100L, seed = 5L)
  r2 <- mrm_perm_block(df, "y", "treatment", "block", n_perm = 100L, seed = 5L)
  expect_equal(r1$p_value, r2$p_value)
})

# ---------------------------------------------------------------------------
# mrm_doe.R : mrm_random_latin
# ---------------------------------------------------------------------------

test_that("mrm_random_latin produces a valid Latin square", {
  sq <- mrm_random_latin(k = 4, seed = 42L)
  expect_true(is.matrix(sq))
  expect_equal(dim(sq), c(4L, 4L))
  expect_equal(rownames(sq), paste0("R", 1:4))
  expect_equal(colnames(sq), paste0("C", 1:4))
  expect_true(all(sq %in% 0:3))
  for (i in seq_len(4)) {
    expect_setequal(sq[i, ], 0:3)
    expect_setequal(sq[, i], 0:3)
  }
})

test_that("mrm_random_latin is reproducible across runs with the same seed", {
  expect_identical(
    mrm_random_latin(5, seed = 7),
    mrm_random_latin(5, seed = 7)
  )
})

# ---------------------------------------------------------------------------
# mrm_mathstats.R : mrm_oneprop_test
# ---------------------------------------------------------------------------

test_that("mrm_oneprop_test returns exact and Wald results", {
  res <- mrm_oneprop_test(x = 58, n = 100, p0 = 0.5)
  expect_true(is.list(res))
  expect_named(res, c(
    "p_hat", "p0", "n", "z_wald", "p_value_wald",
    "p_value_exact", "ci95_wald_lower", "ci95_wald_upper",
    "ci95_exact_lower", "ci95_exact_upper",
    "interpretation"
  ))
  expect_equal(res$p_hat, 0.58)
  expect_equal(res$n, 100L)
  expect_gte(res$p_value_wald, 0)
  expect_lte(res$p_value_wald, 1)
  expect_gte(res$p_value_exact, 0)
  expect_lte(res$p_value_exact, 1)
  expect_gte(res$ci95_wald_lower, 0)
  expect_lte(res$ci95_wald_upper, 1)
  expect_lte(res$ci95_wald_lower, res$ci95_wald_upper)
  expect_lte(res$ci95_exact_lower, res$ci95_exact_upper)
})

test_that("mrm_oneprop_test handles boundary success counts", {
  res0 <- mrm_oneprop_test(x = 0, n = 50, p0 = 0.3)
  expect_equal(res0$p_hat, 0)
  expect_true(is.finite(res0$p_value_exact))
  resn <- mrm_oneprop_test(x = 50, n = 50, p0 = 0.7)
  expect_equal(resn$p_hat, 1)
})

test_that("mrm_oneprop_test rejects invalid x or n", {
  expect_error(mrm_oneprop_test(x = 60, n = 50, p0 = 0.5))
  expect_error(mrm_oneprop_test(x = -1, n = 50, p0 = 0.5))
  expect_error(mrm_oneprop_test(x = 5, n = 0, p0 = 0.5))
})

# ---------------------------------------------------------------------------
# mrm_mathstats.R : mrm_twoprop_test
# ---------------------------------------------------------------------------

test_that("mrm_twoprop_test returns chi-square, Fisher and Wald results", {
  res <- mrm_twoprop_test(x1 = 47, n1 = 100, x2 = 31, n2 = 100)
  expect_true(is.list(res))
  expect_named(res, c(
    "p1", "p2", "diff", "chi2", "df", "p_value_chi2",
    "p_value_fisher", "z_wald", "p_value_wald",
    "ci95_diff_lower", "ci95_diff_upper",
    "interpretation"
  ))
  expect_equal(res$p1, 0.47)
  expect_equal(res$p2, 0.31)
  expect_equal(res$diff, 0.16)
  expect_gte(res$chi2, 0)
  expect_equal(res$df, 1L)
  expect_gte(res$p_value_chi2, 0)
  expect_lte(res$p_value_chi2, 1)
  expect_gte(res$p_value_fisher, 0)
  expect_lte(res$p_value_fisher, 1)
  expect_gte(res$p_value_wald, 0)
  expect_lte(res$p_value_wald, 1)
  expect_lte(res$ci95_diff_lower, res$ci95_diff_upper)
})

test_that("mrm_twoprop_test rejects invalid sample sizes", {
  expect_error(mrm_twoprop_test(x1 = 5, n1 = 0, x2 = 3, n2 = 10))
  expect_error(mrm_twoprop_test(x1 = -1, n1 = 10, x2 = 3, n2 = 10))
})

# ---------------------------------------------------------------------------
# mrm_mathstats.R : mrm_var_test
# ---------------------------------------------------------------------------

test_that("mrm_var_test runs a chi-square test for variance", {
  set.seed(2026)
  x <- rnorm(50, mean = 0, sd = 1.2)
  res <- mrm_var_test(sample = x, sigma0_sq = 1)
  expect_true(is.list(res))
  expect_named(res, c(
    "s_sq", "sigma0_sq", "chi2_stat", "df",
    "p_value_two_sided", "p_value_one_sided_greater",
    "p_value_one_sided_less", "ci95_lower",
    "ci95_upper", "interpretation"
  ))
  expect_gt(res$s_sq, 0)
  expect_gt(res$chi2_stat, 0)
  expect_equal(res$df, 49L)
  expect_gte(res$p_value_two_sided, 0)
  expect_lte(res$p_value_two_sided, 1)
  expect_gte(res$p_value_one_sided_greater, 0)
  expect_lte(res$p_value_one_sided_greater, 1)
  expect_gte(res$p_value_one_sided_less, 0)
  expect_lte(res$p_value_one_sided_less, 1)
  expect_lt(res$ci95_lower, res$ci95_upper)
})

test_that("mrm_var_test errors with fewer than two observations", {
  expect_error(mrm_var_test(sample = 5, sigma0_sq = 1))
  expect_error(mrm_var_test(sample = c(NA, Inf), sigma0_sq = 1))
})

# ---------------------------------------------------------------------------
# mrm_mathstats.R : mrm_qq_plot
# ---------------------------------------------------------------------------

test_that("mrm_qq_plot returns Q-Q coordinates for the normal reference", {
  set.seed(2026)
  x <- rnorm(100)
  qq <- mrm_qq_plot(x, dist = "norm")
  expect_s3_class(qq, "data.frame")
  expect_named(qq, c(
    "rank", "empirical", "theoretical",
    "plotting_position"
  ))
  expect_equal(nrow(qq), 100L)
  expect_equal(qq$rank, seq_len(100))
  expect_true(!is.unsorted(qq$empirical))
  expect_true(all(qq$plotting_position > 0 & qq$plotting_position < 1))
  expect_true(all(is.finite(qq$theoretical)))
})

test_that("mrm_qq_plot accepts other reference distributions and parameters", {
  set.seed(31)
  x <- rexp(60, rate = 2)
  qq <- mrm_qq_plot(x, dist = "exp", rate = 2)
  expect_equal(nrow(qq), 60L)
  qt <- mrm_qq_plot(rnorm(40), dist = "t", df = 5)
  expect_equal(nrow(qt), 40L)
  expect_true(all(is.finite(qt$theoretical)))
})

test_that("mrm_qq_plot errors with fewer than two observations", {
  expect_error(mrm_qq_plot(3, dist = "norm"))
})

# ---------------------------------------------------------------------------
# mrm_mathstats.R : mrm_clt_demo
# ---------------------------------------------------------------------------

test_that("mrm_clt_demo generates standardised sample means", {
  res <- mrm_clt_demo(
    base_distribution = "exp", n_samples = 1000L,
    sample_size = 30L, seed = 42L, rate = 1
  )
  expect_s3_class(res, "data.frame")
  expect_named(res, c("sample_index", "sample_mean", "z_score"))
  expect_equal(nrow(res), 1000L)
  expect_equal(res$sample_index, seq_len(1000))
  expect_true(all(is.finite(res$sample_mean)))
  expect_true(all(is.finite(res$z_score)))
})

test_that("mrm_clt_demo is reproducible and supports a uniform base", {
  r1 <- mrm_clt_demo(
    base_distribution = "unif", n_samples = 200L,
    sample_size = 20L, seed = 7L
  )
  r2 <- mrm_clt_demo(
    base_distribution = "unif", n_samples = 200L,
    sample_size = 20L, seed = 7L
  )
  expect_equal(r1$sample_mean, r2$sample_mean)
})

# ---------------------------------------------------------------------------
# mrm_mathstats.R : mrm_pit
# ---------------------------------------------------------------------------

test_that("mrm_pit applies the probability integral transform", {
  set.seed(2026)
  x <- rnorm(200)
  pit <- mrm_pit(x, dist = "norm")
  expect_s3_class(pit, "data.frame")
  expect_named(pit, c("raw", "U"))
  expect_equal(nrow(pit), 200L)
  expect_true(all(pit$U >= 0 & pit$U <= 1))
  ksp <- attr(pit, "ks_pvalue")
  kss <- attr(pit, "ks_stat")
  expect_true(is.finite(ksp))
  expect_gte(ksp, 0)
  expect_lte(ksp, 1)
  expect_true(is.finite(kss))
  expect_gte(kss, 0)
})

test_that("mrm_pit accepts a misspecified reference distribution", {
  set.seed(2026)
  x <- rnorm(200)
  pit_wrong <- mrm_pit(x, dist = "t", df = 3)
  expect_s3_class(pit_wrong, "data.frame")
  expect_true(all(pit_wrong$U >= 0 & pit_wrong$U <= 1))
  expect_true(is.finite(attr(pit_wrong, "ks_pvalue")))
})

# ---------------------------------------------------------------------------
# mrm_diagnostics.R : mrm_standardised_difference
# ---------------------------------------------------------------------------

test_that("mrm_standardised_difference returns one row per covariate", {
  set.seed(2026)
  n <- 200L
  df <- data.frame(
    D = rbinom(n, 1, 0.4),
    age = rnorm(n, 50, 10),
    bmi = rnorm(n, 27, 4)
  )
  df$age[df$D == 1] <- df$age[df$D == 1] + 3
  tbl <- mrm_standardised_difference(df,
    treatment_col = "D",
    covariates = c("age", "bmi")
  )
  expect_s3_class(tbl, "data.frame")
  expect_named(tbl, c(
    "covariate", "mean_treated", "mean_control",
    "pooled_sd", "smd_pct", "imbalanced"
  ))
  expect_equal(nrow(tbl), 2L)
  expect_setequal(tbl$covariate, c("age", "bmi"))
  expect_true(all(tbl$pooled_sd >= 0))
  expect_true(all(is.finite(tbl$smd_pct)))
  expect_type(tbl$imbalanced, "logical")
})

# ---------------------------------------------------------------------------
# mrm_diagnostics.R : mrm_check_balancing
# ---------------------------------------------------------------------------

test_that("mrm_check_balancing yields a composite balance verdict", {
  set.seed(2026)
  n <- 200L
  df <- data.frame(
    D = rbinom(n, 1, 0.4),
    age = rnorm(n, 50, 10),
    bmi = rnorm(n, 27, 4)
  )
  df$age[df$D == 1] <- df$age[df$D == 1] + 3
  bal <- mrm_check_balancing(df,
    treatment_col = "D",
    covariates = c("age", "bmi")
  )
  expect_true(is.list(bal))
  expect_named(bal, c(
    "table", "threshold_pct", "n_imbalanced",
    "overall_balanced", "interpretation"
  ))
  expect_s3_class(bal$table, "data.frame")
  expect_equal(bal$threshold_pct, 10)
  expect_gte(bal$n_imbalanced, 0L)
  expect_lte(bal$n_imbalanced, 2L)
  expect_type(bal$overall_balanced, "logical")
})

test_that("mrm_check_balancing honours a custom threshold", {
  set.seed(8)
  n <- 150L
  df <- data.frame(D = rbinom(n, 1, 0.5), x = rnorm(n))
  bal <- mrm_check_balancing(df,
    treatment_col = "D",
    covariates = "x", threshold_pct = 25
  )
  expect_equal(bal$threshold_pct, 25)
})

# ---------------------------------------------------------------------------
# mrm_diagnostics.R : mrm_check_overlap
# ---------------------------------------------------------------------------

test_that("mrm_check_overlap reports propensity-score support diagnostics", {
  set.seed(2026)
  n <- 300L
  x <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.5 * x))
  df <- data.frame(D = D, age = x)
  ovl <- mrm_check_overlap(df, treatment_col = "D", covariates = "age")
  expect_true(is.list(ovl))
  expect_named(ovl, c(
    "e_treated_quantiles", "e_control_quantiles",
    "common_support_lower", "common_support_upper",
    "n_outside_support", "positivity_violations",
    "interpretation"
  ))
  expect_length(ovl$e_treated_quantiles, 5)
  expect_length(ovl$e_control_quantiles, 5)
  expect_gte(ovl$common_support_lower, 0)
  expect_lte(ovl$common_support_upper, 1)
  expect_lte(ovl$common_support_lower, ovl$common_support_upper)
  expect_gte(ovl$n_outside_support, 0L)
  expect_gte(ovl$positivity_violations, 0L)
})

# ---------------------------------------------------------------------------
# mrm_diagnostics.R : mrm_median_causal_effect
# ---------------------------------------------------------------------------

test_that("mrm_median_causal_effect estimates a matched median effect", {
  set.seed(2026)
  n <- 200L
  x <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.5 * x))
  y <- 0.7 * D + 0.3 * x + rnorm(n, 0, 0.5)
  df <- data.frame(D = D, y = y, age = x)
  res <- mrm_median_causal_effect(df,
    treatment_col = "D",
    outcome_col = "y", covariates = "age"
  )
  expect_true(is.list(res))
  expect_named(res, c(
    "median_y1", "median_y0",
    "median_treatment_effect", "n_matched",
    "interpretation"
  ))
  expect_true(is.finite(res$median_y1))
  expect_true(is.finite(res$median_y0))
  expect_gt(res$n_matched, 0L)
  expect_lte(res$n_matched, min(sum(D == 1), sum(D == 0)))
})

# ---------------------------------------------------------------------------
# mrm_diagnostics.R : mrm_assumptions_check
# ---------------------------------------------------------------------------

test_that("mrm_assumptions_check returns the three identifiability sub-lists", {
  set.seed(2026)
  n <- 300L
  x <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.5 * x))
  y <- 0.7 * D + 0.3 * x + rnorm(n)
  df <- data.frame(D = D, y = y, age = x)
  chk <- mrm_assumptions_check(df,
    treatment_col = "D",
    outcome_col = "y", covariates = "age"
  )
  expect_true(is.list(chk))
  expect_named(chk, c(
    "sutva", "unconfoundedness",
    "probabilistic_assignment", "overall_verdict"
  ))
  expect_named(chk$sutva, c("verdict", "evidence"))
  expect_named(chk$unconfoundedness, c("verdict", "evidence"))
  expect_named(chk$probabilistic_assignment, c("verdict", "evidence"))
  expect_type(chk$overall_verdict, "character")
})

test_that("mrm_assumptions_check works with multiple covariates", {
  set.seed(4)
  n <- 250L
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.3 * x1 - 0.2 * x2))
  y <- 0.5 * D + rnorm(n)
  df <- data.frame(D = D, y = y, a = x1, b = x2)
  chk <- mrm_assumptions_check(df,
    treatment_col = "D",
    outcome_col = "y", covariates = c("a", "b")
  )
  expect_named(chk, c(
    "sutva", "unconfoundedness",
    "probabilistic_assignment", "overall_verdict"
  ))
  expect_type(chk$overall_verdict, "character")
})
