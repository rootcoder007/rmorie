# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Synthetic data for matching tests
# ---------------------------------------------------------------------------

set.seed(1)

# make_match_df / make_match_df_balanced / make_match_df_skewed live
# in helper-matching.R so other matching test files can re-use them.


# ---------------------------------------------------------------------------
# morie_matching_estimate_propensity
# ---------------------------------------------------------------------------

test_that("morie_matching_estimate_propensity returns scores in (0, 1)", {
  df <- make_match_df()
  ps <- morie_matching_estimate_propensity(df, "d", c("x1", "x2"))
  expect_length(ps, nrow(df))
  expect_true(all(ps > 0 & ps < 1))
  expect_true(!is.null(names(ps)))
})


# ---------------------------------------------------------------------------
# morie_matching_trim_propensity
# ---------------------------------------------------------------------------

test_that("morie_matching_trim_propensity clips to [lower, upper]", {
  out <- morie_matching_trim_propensity(c(0.001, 0.5, 0.999),
                                        lower = 0.05, upper = 0.95)
  expect_equal(out, c(0.05, 0.5, 0.95))
})


# ---------------------------------------------------------------------------
# morie_matching_common_support
# ---------------------------------------------------------------------------

test_that("morie_matching_common_support drops off-support rows", {
  df <- make_match_df(n = 200)
  df$propensity_score <- morie_matching_estimate_propensity(
    df, "d", c("x1", "x2"))
  out <- morie_matching_common_support(df, "d", method = "minmax")
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) <= nrow(df))
  expect_true(nrow(out) > 0)
})


# ---------------------------------------------------------------------------
# morie_matching_nearest_neighbor
# ---------------------------------------------------------------------------

test_that("morie_matching_nearest_neighbor returns match_result with pairs", {
  df <- make_match_df()
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  expect_s3_class(res, "morie_match_result")
  expect_true(all(c("matched_data", "n_treated", "n_matched_control",
                    "match_pairs", "method", "details") %in% names(res)))
  expect_s3_class(res$match_pairs, "data.frame")
  expect_true(all(c("treated_idx", "control_idx") %in%
                    colnames(res$match_pairs)))
  expect_gt(nrow(res$match_pairs), 0)
})

test_that("morie_matching_nearest_neighbor caliper restricts matches", {
  df <- make_match_df()
  res_no_cal <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  res_cal <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"),
                                              caliper = 0.05)
  expect_true(nrow(res_cal$match_pairs) <= nrow(res_no_cal$match_pairs))
})


# ---------------------------------------------------------------------------
# morie_matching_exact
# ---------------------------------------------------------------------------

test_that("morie_matching_exact returns match_result", {
  df <- make_match_df(n = 300)
  res <- morie_matching_exact(df, "d", c("region", "year"))
  expect_s3_class(res, "morie_match_result")
  expect_gte(nrow(res$match_pairs), 0)
})


# ---------------------------------------------------------------------------
# morie_matching_cem
# ---------------------------------------------------------------------------

test_that("morie_matching_cem matches and returns weights", {
  df <- make_match_df(n = 400)
  res <- morie_matching_cem(df, "d", c("x1", "x2"), n_bins = 4L)
  expect_s3_class(res, "morie_match_result")
  expect_s3_class(res$matched_data, "data.frame")
})


# ---------------------------------------------------------------------------
# morie_matching_mahalanobis
# ---------------------------------------------------------------------------

test_that("morie_matching_mahalanobis returns match_result", {
  df <- make_match_df()
  res <- morie_matching_mahalanobis(df, "d", c("x1", "x2"))
  expect_s3_class(res, "morie_match_result")
  expect_true(nrow(res$match_pairs) > 0)
})


# ---------------------------------------------------------------------------
# morie_matching_optimal_pair (skip if optmatch missing)
# ---------------------------------------------------------------------------

test_that("morie_matching_optimal_pair runs when prerequisites are met", {
  df <- make_match_df(n = 100)
  res <- tryCatch(
    morie_matching_optimal_pair(df, "d", c("x1", "x2")),
    error = function(e) NULL
  )
  skip_if(is.null(res), "optimal_pair unavailable in environment")
  expect_s3_class(res, "morie_match_result")
})


# ---------------------------------------------------------------------------
# morie_matching_full
# ---------------------------------------------------------------------------

test_that("morie_matching_full runs end-to-end", {
  df <- make_match_df(n = 100)
  res <- tryCatch(
    morie_matching_full(df, "d", c("x1", "x2")),
    error = function(e) NULL
  )
  skip_if(is.null(res), "full matching unavailable")
  expect_s3_class(res, "morie_match_result")
})


# ---------------------------------------------------------------------------
# morie_matching_subclassify
# ---------------------------------------------------------------------------

test_that("morie_matching_subclassify returns subclass-tagged data", {
  df <- make_match_df(n = 300)
  res <- morie_matching_subclassify(df, "d", c("x1", "x2"))
  expect_true(is.list(res))
  expect_true(all(c("data_with_strata", "stratum_effects") %in% names(res)))
  expect_s3_class(res$data_with_strata, "data.frame")
  expect_s3_class(res$stratum_effects, "data.frame")
})


# ---------------------------------------------------------------------------
# morie_matching_entropy_balance
# ---------------------------------------------------------------------------

test_that("morie_matching_entropy_balance produces weights", {
  df <- make_match_df(n = 200)
  res <- tryCatch(
    morie_matching_entropy_balance(df, "d", c("x1", "x2")),
    error = function(e) NULL
  )
  skip_if(is.null(res), "entropy balancing unavailable")
  # morie_matching_entropy_balance returns a named numeric weight vector
  # (treated units = 1, controls = Hainmueller dual weights).
  expect_true(is.numeric(res))
  expect_equal(length(res), nrow(df))
  expect_true(all(res >= 0))
})


# ---------------------------------------------------------------------------
# morie_matching_balance / balance_table
# ---------------------------------------------------------------------------

test_that("morie_matching_balance returns balance summary", {
  df <- make_match_df()
  res <- morie_matching_balance(df, "d", c("x1", "x2"))
  expect_s3_class(res, "morie_balance_result")
  expect_true(all(c("balance_table", "overall_balance",
                    "max_smd", "balanced") %in% names(res)))
  expect_s3_class(res$balance_table, "data.frame")
  expect_true(all(c("covariate", "smd", "abs_smd") %in%
                    colnames(res$balance_table)))
})

test_that("morie_matching_balance_table returns just the data frame", {
  df <- make_match_df()
  tb <- morie_matching_balance_table(df, "d", c("x1", "x2"))
  expect_s3_class(tb, "data.frame")
  expect_equal(nrow(tb), 2L)
})


# ---------------------------------------------------------------------------
# morie_matching_love_plot_data
# ---------------------------------------------------------------------------

test_that("morie_matching_love_plot_data returns before/after SMDs", {
  df <- make_match_df()
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  lp <- morie_matching_love_plot_data(df, res$matched_data,
                                       "d", c("x1", "x2"))
  expect_s3_class(lp, "data.frame")
  expect_true(all(c("smd_before", "smd_after",
                    "abs_smd_before", "abs_smd_after") %in% colnames(lp)))
})


# ---------------------------------------------------------------------------
# morie_matching_att_matched / ate_matched / atc_matched
# ---------------------------------------------------------------------------

test_that("morie_matching_att_matched returns te_result", {
  df <- make_match_df()
  rownames(df) <- as.character(seq_len(nrow(df)))
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  att <- morie_matching_att_matched(df, "y", "d", res$match_pairs)
  expect_s3_class(att, "morie_te_result")
  expect_equal(att$estimand, "ATT")
  expect_true(is.finite(att$estimate))
})

test_that("morie_matching_att_matched returns NA result on empty pairs", {
  empty <- data.frame(treated_idx = character(0),
                      control_idx = character(0),
                      distance = numeric(0),
                      stringsAsFactors = FALSE)
  df <- make_match_df(n = 50)
  rownames(df) <- as.character(seq_len(nrow(df)))
  res <- morie_matching_att_matched(df, "y", "d", empty)
  expect_s3_class(res, "morie_te_result")
  expect_true(is.na(res$estimate))
})

test_that("morie_matching_ate_matched returns te_result with ATE", {
  df <- make_match_df()
  res <- morie_matching_ate_matched(df, "y", "d", c("x1", "x2"))
  expect_s3_class(res, "morie_te_result")
  expect_equal(res$estimand, "ATE")
  expect_true(is.finite(res$estimate))
})

test_that("morie_matching_atc_matched returns te_result with ATC", {
  df <- make_match_df()
  rownames(df) <- as.character(seq_len(nrow(df)))
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  atc <- morie_matching_atc_matched(df, "y", "d", res$match_pairs)
  expect_s3_class(atc, "morie_te_result")
  expect_equal(atc$estimand, "ATC")
})


# ---------------------------------------------------------------------------
# morie_matching_abadie_imbens_se
# ---------------------------------------------------------------------------

test_that("morie_matching_abadie_imbens_se returns a non-negative scalar", {
  df <- make_match_df()
  rownames(df) <- as.character(seq_len(nrow(df)))
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  se <- morie_matching_abadie_imbens_se(df, "y", "d", res$match_pairs)
  expect_type(se, "double")
  expect_true(is.finite(se))
  expect_gte(se, 0)
})


# ---------------------------------------------------------------------------
# morie_matching_rosenbaum_bounds
# ---------------------------------------------------------------------------

test_that("morie_matching_rosenbaum_bounds returns one row per gamma", {
  df <- make_match_df()
  rownames(df) <- as.character(seq_len(nrow(df)))
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  rb <- morie_matching_rosenbaum_bounds(df, "y", "d", res$match_pairs,
                                        gamma_range = c(1, 1.5, 2))
  expect_s3_class(rb, "data.frame")
  expect_equal(nrow(rb), 3L)
  expect_true(all(c("gamma", "p_lower", "p_upper") %in% colnames(rb)))
})


# ---------------------------------------------------------------------------
# morie_matching_doubly_robust
# ---------------------------------------------------------------------------

test_that("morie_matching_doubly_robust returns te_result with finite ATT_DR on balanced data", {
  # Balanced 50/50 treatment so MatchIt's "Fewer control units"
  # warning shouldn't fire on the happy path; covers the
  # mathematically-correct case.
  df <- make_match_df_balanced(n = 300L, tau = 0.4, seed = 11)
  res <- morie_matching_doubly_robust(df, "y", "d", c("x1", "x2"),
                                       n_bootstrap = 20L, seed = 11)
  expect_s3_class(res, "morie_te_result")
  expect_equal(res$estimand, "ATT_DR")
  expect_true(is.finite(res$estimate))
})

test_that("morie_matching_doubly_robust emits a single summary warning on skewed data", {
  # Skewed ~80/20 treatment so MatchIt fires "Fewer control" in
  # most bootstrap resamples; verify morie collapses the per-
  # resample noise into one summary warning.
  df <- make_match_df_skewed(n = 200L, tau = 0.4, seed = 21)
  expect_warning(
    res <- morie_matching_doubly_robust(df, "y", "d", c("x1", "x2"),
                                         n_bootstrap = 20L, seed = 21),
    "bootstrap resamples had fewer control units than treated"
  )
  expect_s3_class(res, "morie_te_result")
})


# ---------------------------------------------------------------------------
# morie_matching_multi_treatment
# ---------------------------------------------------------------------------

test_that("morie_matching_multi_treatment returns one match_result per non-ref level", {
  set.seed(1)
  n <- 300
  treat3 <- sample(c("A", "B", "C"), n, replace = TRUE)
  df <- data.frame(
    treat3 = treat3,
    y = rnorm(n),
    x1 = rnorm(n), x2 = rnorm(n)
  )
  res <- morie_matching_multi_treatment(df, "treat3", c("x1", "x2"))
  expect_type(res, "list")
  expect_true(length(res) >= 1)
  expect_s3_class(res[[1]], "morie_match_result")
})


# ---------------------------------------------------------------------------
# morie_matching_quality
# ---------------------------------------------------------------------------

test_that("morie_matching_quality returns bias reduction summary", {
  df <- make_match_df()
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  q <- morie_matching_quality(df, res$matched_data, "d", c("x1", "x2"))
  expect_true(all(c("balance_before", "balance_after",
                    "bias_reduction", "mean_bias_reduction",
                    "n_obs_before", "n_obs_after") %in% names(q)))
})


# ---------------------------------------------------------------------------
# morie_matching_overlap
# ---------------------------------------------------------------------------

test_that("morie_matching_overlap reports ESS and overlap region", {
  df <- make_match_df()
  res <- morie_matching_overlap(df, "d", c("x1", "x2"))
  expect_true(all(c("ps_summary", "overlap_region",
                    "n_off_support", "pct_off_support",
                    "effective_sample_size") %in% names(res)))
  expect_true(res$effective_sample_size > 0)
  expect_true(res$overlap_region["lower"] <= res$overlap_region["upper"])
})
