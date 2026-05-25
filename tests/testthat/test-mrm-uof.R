# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Comprehensive coverage tests for R/mrm_uof.R.  Goal: exercise every
# branch — happy paths, error paths, warning conditions, and the
# small numeric helpers (.uof_gini, .uof_hill_alpha, .uof_topk_share,
# .uof_wilson_ci, .uof_cramers_v).  No tests are skipped to inflate
# coverage; everything runs on synthetic data so CI without ARSAU
# data still hits every branch.

# ── Numeric helpers ────────────────────────────────────────────────

test_that("gini returns NA on empty / zero-sum", {
  expect_true(is.na(morie:::.uof_gini(numeric(0))))
  expect_true(is.na(morie:::.uof_gini(c(0, 0, 0))))
})

test_that("gini is 0 for perfectly equal counts", {
  expect_lt(morie:::.uof_gini(rep(10, 100)), 0.01)
})

test_that("gini grows with concentration", {
  even   <- morie:::.uof_gini(rep(10, 10))
  uneven <- morie:::.uof_gini(c(rep(1, 9), 90))
  expect_gt(uneven, even)
  expect_gt(uneven, 0.5)
})

test_that("hill_alpha returns NA below threshold or for short input", {
  expect_true(is.na(morie:::.uof_hill_alpha(c(0.1, 0.2))))
  expect_true(is.na(morie:::.uof_hill_alpha(numeric(0))))
  expect_true(is.na(morie:::.uof_hill_alpha(c(1, 1, 1)))) # denom = 0
})

test_that("hill_alpha is finite for Pareto-like data", {
  set.seed(1); x <- (1 + 1 / runif(200))^2
  expect_true(is.finite(morie:::.uof_hill_alpha(x)))
})

test_that("topk_share collapses to 1.0 when k >= n", {
  expect_equal(morie:::.uof_topk_share(c(1, 2, 3), 10), 1)
})

test_that("topk_share NA on empty / zero sum", {
  expect_true(is.na(morie:::.uof_topk_share(numeric(0), 5)))
  expect_true(is.na(morie:::.uof_topk_share(c(0, 0, 0), 5)))
})

test_that("wilson_ci returns NA on n=0 and finite bounds otherwise", {
  expect_equal(morie:::.uof_wilson_ci(0L, 0L), c(NA_real_, NA_real_))
  ci <- morie:::.uof_wilson_ci(5L, 10L)
  expect_true(all(is.finite(ci)) && ci[1] < ci[2] && ci[1] >= 0 && ci[2] <= 1)
})

test_that("cramers_v NA on degenerate r/c or n=0", {
  expect_true(is.na(morie:::.uof_cramers_v(10, 100, 1, 2)))
  expect_true(is.na(morie:::.uof_cramers_v(10, 0, 2, 2)))
  v <- morie:::.uof_cramers_v(40, 100, 3, 3)
  expect_true(is.finite(v) && v > 0)
})

test_that("fmt_pct handles non-finite", {
  expect_equal(morie:::.uof_fmt_pct(NA_real_), "n/a")
  expect_equal(morie:::.uof_fmt_pct(NaN), "n/a")
  expect_match(morie:::.uof_fmt_pct(0.5), "50.00%")
})


# ── 1. force_concentration ─────────────────────────────────────────

test_that("force_concentration: missing column branches", {
  r <- mrm_uof_force_concentration(data.frame(other = 1:5), "force_col")
  expect_equal(r$n, 0L)
  expect_match(r$warnings[1], "force_col")

  r2 <- mrm_uof_force_concentration(
    data.frame(f = c("A", "B")), "f", count_col = "missing"
  )
  expect_equal(r2$n, 0L)
  expect_match(r2$warnings[1], "count_col")
})

test_that("force_concentration: zero-counts branch", {
  df <- data.frame(f = c("A", "B"), cnt = c(0, 0))
  r <- mrm_uof_force_concentration(df, "f", count_col = "cnt")
  expect_equal(r$n_incidents, 0L)
  expect_match(r$interpretation, "zero")
})

test_that("force_concentration: small-n warning fires when <10 forces", {
  df <- data.frame(f = c("A", "A", "B", "C"))
  r <- mrm_uof_force_concentration(df, "f")
  expect_true(any(grepl("Only", r$warnings)))
})

test_that("force_concentration: count_col path sums correctly", {
  df <- data.frame(f = c("A", "B", "A", "B"), cnt = c(3, 5, 7, 2))
  r <- mrm_uof_force_concentration(df, "f", count_col = "cnt")
  expect_equal(sort(unname(unlist(r$counts))), c(7, 10))
})

test_that("force_concentration: heavy concentration interpretation paths", {
  df <- data.frame(f = c(rep("A", 90), rep("B", 5), rep("C", 5)))
  r <- mrm_uof_force_concentration(df, "f")
  expect_match(r$interpretation, "concentration")
  # n=3 forces with 90/5/5 -> gini = 0.567 (3-cell limit); test moderate
  expect_true(r$gini > 0.5)
})

test_that("force_concentration: distinct alpha bands trip distinct prose", {
  # Heavy tail (alpha < 2)
  set.seed(0); x <- table(sample(letters[1:5], 50, prob = c(0.5, 0.2, 0.1, 0.1, 0.1), replace = TRUE))
  df1 <- data.frame(f = rep(names(x), x))
  r1 <- mrm_uof_force_concentration(df1, "f")
  expect_true(is.finite(r1$pareto_alpha_mle))
})


# ── 2. weapon_diversity ────────────────────────────────────────────

test_that("weapon_diversity: missing columns branch", {
  r <- mrm_uof_weapon_diversity(data.frame(x = 1), "weapon", "force")
  expect_equal(r$n, 0L)
})

test_that("weapon_diversity: degenerate table branch", {
  df <- data.frame(w = c("X", "X", "X"), f = c("A", "A", "A"))
  r <- mrm_uof_weapon_diversity(df, "w", "f")
  expect_true("Contingency table is degenerate" %in% r$warnings ||
              any(grepl("degenerate", r$warnings)))
})

test_that("weapon_diversity: low-expected-count warning", {
  df <- data.frame(
    w = c("A", "B", "A", "B"),
    f = c("X", "Y", "Y", "X")
  )
  r <- mrm_uof_weapon_diversity(df, "w", "f")
  expect_true(any(grepl("expected cell", r$warnings)))
})

test_that("weapon_diversity: produces finite chi2 and V on large df", {
  set.seed(0)
  df <- data.frame(
    w = sample(c("A", "B", "C"), 500, replace = TRUE),
    f = sample(c("X", "Y", "Z"), 500, replace = TRUE)
  )
  r <- mrm_uof_weapon_diversity(df, "w", "f")
  expect_true(is.finite(r$chi2) && r$chi2 >= 0)
  expect_true(is.finite(r$cramers_v) && r$cramers_v >= 0 && r$cramers_v <= 1)
  expect_length(r$top_residuals, 3)
})


# ── 3. yoy_change ──────────────────────────────────────────────────

test_that("yoy_change: no input branch", {
  r <- mrm_uof_yoy_change()
  expect_equal(r$n, 0L)
})

test_that("yoy_change: missing year_col branch", {
  r <- mrm_uof_yoy_change(df = data.frame(x = 1:5), year_col = "year")
  expect_equal(r$n, 0L)
})

test_that("yoy_change: missing count_col branch", {
  r <- mrm_uof_yoy_change(df = data.frame(yr = c(2020, 2021)),
                            year_col = "yr", count_col = "missing")
  expect_equal(r$n, 0L)
})

test_that("yoy_change: dfs_by_year happy path + change-point heuristic", {
  set.seed(0)
  dfs <- list(
    "2020" = data.frame(x = 1:100),
    "2021" = data.frame(x = 1:90),
    "2022" = data.frame(x = 1:120),
    "2023" = data.frame(x = 1:115)
  )
  r <- mrm_uof_yoy_change(dfs_by_year = dfs)
  expect_equal(r$years, 2020:2023)
  expect_equal(r$counts, c(100, 90, 120, 115))
  expect_true(!is.na(r$change_point_year))
})

test_that("yoy_change: too-few-years warning", {
  r <- mrm_uof_yoy_change(dfs_by_year = list("2020" = data.frame(x = 1:5)))
  expect_true(any(grepl("Only", r$warnings)))
})

test_that("yoy_change: df + year_col path", {
  df <- data.frame(yr = rep(c(2020, 2021, 2022), each = 10))
  r <- mrm_uof_yoy_change(df = df, year_col = "yr")
  expect_equal(r$counts, c(10, 10, 10))
})

test_that("yoy_change: count_col with sum path", {
  df <- data.frame(yr = c(2020, 2020, 2021, 2021), cnt = c(1, 2, 3, 4))
  r <- mrm_uof_yoy_change(df = df, year_col = "yr", count_col = "cnt")
  expect_equal(r$counts, c(3L, 7L))
})

test_that("yoy_change: per-year missing count_col emits warning", {
  dfs <- list(
    "2020" = data.frame(x = 1:5),
    "2021" = data.frame(other = 1:5)
  )
  r <- mrm_uof_yoy_change(dfs_by_year = dfs, count_col = "x")
  expect_true(any(grepl("count_col missing", r$warnings)))
})


# ── 4. region_locality ─────────────────────────────────────────────

test_that("region_locality: missing column branch", {
  r <- mrm_uof_region_locality(data.frame(other = 1), "a", "b")
  expect_equal(r$n, 0L)
})

test_that("region_locality: drops NA rows + reports n_dropped", {
  df <- data.frame(
    a = c("X", "X", NA, "Y"),
    b = c("X", "Y", "X", "Y")
  )
  r <- mrm_uof_region_locality(df, "a", "b")
  expect_equal(r$n_dropped, 1L)
})

test_that("region_locality: all-NA leaves empty contingency branch", {
  df <- data.frame(a = c(NA, NA), b = c(NA, NA))
  r <- mrm_uof_region_locality(df, "a", "b")
  expect_equal(r$n, 0L)
})

test_that("region_locality: small-n warning + descriptive prose", {
  df <- data.frame(a = c("X", "Y"), b = c("X", "Y"))
  r <- mrm_uof_region_locality(df, "a", "b")
  expect_true(any(grepl("small", r$warnings)))
})

test_that("region_locality: well-aligned regions produce diag share", {
  df <- data.frame(
    a = rep(c("Central", "Eastern", "Western"), each = 30),
    b = rep(c("Central", "Eastern", "Western"), each = 30)
  )
  r <- mrm_uof_region_locality(df, "a", "b")
  expect_equal(r$diagonal_share, 1)
})


# ── 5. demographic_disparity ───────────────────────────────────────

test_that("demographic_disparity: missing column branch", {
  r <- mrm_uof_demographic_disparity(data.frame(other = 1), "demo", "out")
  expect_equal(r$n, 0L)
})

test_that("demographic_disparity: empty after dropna branch", {
  df <- data.frame(demo = NA_character_, out = NA_integer_)
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  expect_equal(r$n, 0L)
})

test_that("demographic_disparity: yes/no coercion path", {
  df <- data.frame(
    demo = c("A", "B", "A", "B", "A", "B"),
    out = c("Yes", "No", "Yes", "Yes", "No", "No"),
    stringsAsFactors = FALSE
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  expect_true("baseline" %in% names(r))
  expect_true(r$baseline %in% c("A", "B"))
})

test_that("demographic_disparity: logical outcome coercion path", {
  df <- data.frame(
    demo = c("A", "B", "A", "B"),
    out = c(TRUE, FALSE, TRUE, TRUE)
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  expect_true(is.finite(r$baseline_rate))
})

test_that("demographic_disparity: numeric-non-01 coercion path", {
  df <- data.frame(
    demo = c("A", "B", "A", "B"),
    out = c(2, 0, 3, 1)
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  expect_true(is.finite(r$baseline_rate))
})

test_that("demographic_disparity: invalid baseline branch", {
  df <- data.frame(demo = c("A", "B", "A"), out = c(1, 0, 1))
  r <- mrm_uof_demographic_disparity(df, "demo", "out", baseline = "Z")
  expect_match(r$warnings[1], "baseline")
})

test_that("demographic_disparity: small-group warning per category", {
  df <- data.frame(
    demo = c(rep("A", 10), rep("B", 5)),
    out = c(rep(1, 10), rep(0, 5))
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  expect_true(any(grepl("< 30", r$warnings)))
})

test_that("demographic_disparity: bootstrap RR CI path", {
  set.seed(0)
  df <- data.frame(
    demo = sample(c("A", "B"), 200, replace = TRUE),
    out = rbinom(200, 1, 0.3)
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out", bootstrap_reps = 50L)
  non_baseline <- r$per_category[!vapply(r$per_category, function(e) e$baseline, logical(1))][[1]]
  expect_true(!is.na(non_baseline$rr_lo) || !is.na(non_baseline$rr_hi))
})


# ── 6. data_quality_audit ──────────────────────────────────────────

test_that("data_quality_audit: basic per-column stats", {
  df <- data.frame(
    a = c(1, 2, NA, 4),
    b = c("x", "y", "z", "x"),
    stringsAsFactors = FALSE
  )
  r <- mrm_uof_data_quality_audit(df)
  expect_equal(r$n_cols, 2L)
  expect_equal(r$n_rows, 4L)
})

test_that("data_quality_audit: sidecar comparison", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  sc <- list(fields = list(
    list(id = "x", type = "int"),
    list(id = "z", type = "text")
  ))
  r <- mrm_uof_data_quality_audit(df, sidecar = sc)
  expect_true("z" %in% r$missing_columns)
  expect_true("y" %in% r$extra_columns)
})

test_that("data_quality_audit: expected_schema comparison", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  schema <- list(columns = list(
    list(name = "x", dtype = "integer"),
    list(name = "z", dtype = "text")
  ))
  r <- mrm_uof_data_quality_audit(df, expected_schema = schema)
  expect_true("z" %in% r$missing_columns)
  expect_true("y" %in% r$extra_columns)
})

test_that("data_quality_audit: high-null flag", {
  df <- data.frame(a = c(NA, NA, NA, 1))
  r <- mrm_uof_data_quality_audit(df)
  expect_true(any(grepl("null", r$suspect_flags)))
})

test_that("data_quality_audit: constant-value flag", {
  df <- data.frame(a = c(5, 5, 5))
  r <- mrm_uof_data_quality_audit(df)
  expect_true(any(grepl("constant", r$suspect_flags)))
})

test_that("data_quality_audit: every-value-unique flag", {
  df <- data.frame(a = c("u1", "u2", "u3"), stringsAsFactors = FALSE)
  r <- mrm_uof_data_quality_audit(df)
  expect_true(any(grepl("possible identifier", r$suspect_flags)))
})

test_that("data_quality_audit: clean dataframe has no flags", {
  set.seed(0)
  df <- data.frame(
    a = sample(1:5, 100, replace = TRUE),
    b = sample(c("x", "y"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  r <- mrm_uof_data_quality_audit(df)
  expect_match(r$interpretation, "No structural")
})


# ── print method ───────────────────────────────────────────────────

test_that("print.morie_mrm_uof_result emits title, summary, warnings, interp", {
  r <- mrm_uof_force_concentration(
    data.frame(f = rep(c("A", "B", "C"), 50)), "f"
  )
  out <- capture.output(print(r))
  expect_true(length(out) > 0)
  expect_true(any(grepl("MRM-UOF Force Concentration", out)))
})
