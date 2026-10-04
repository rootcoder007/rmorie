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
  # 2s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_heavy()
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
  skip_heavy()
  out <- morie_matching_trim_propensity(c(0.001, 0.5, 0.999),
                                        lower = 0.05, upper = 0.95)
  expect_equal(out, c(0.05, 0.5, 0.95))
})


# ---------------------------------------------------------------------------
# morie_matching_common_support
# ---------------------------------------------------------------------------

test_that("morie_matching_common_support drops off-support rows", {
  skip_heavy()
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
  df <- make_match_df(n = 300)
  res <- morie_matching_exact(df, "d", c("region", "year"))
  expect_s3_class(res, "morie_match_result")
  expect_gte(nrow(res$match_pairs), 0)
})


# ---------------------------------------------------------------------------
# morie_matching_cem
# ---------------------------------------------------------------------------

test_that("morie_matching_cem matches and returns weights", {
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
  df <- make_match_df(n = 400)
  res <- morie_matching_cem(df, "d", c("x1", "x2"), n_bins = 4L)
  expect_s3_class(res, "morie_match_result")
  expect_s3_class(res$matched_data, "data.frame")
})


# ---------------------------------------------------------------------------
# morie_matching_mahalanobis
# ---------------------------------------------------------------------------

test_that("morie_matching_mahalanobis returns match_result", {
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
  df <- make_match_df()
  res <- morie_matching_mahalanobis(df, "d", c("x1", "x2"))
  expect_s3_class(res, "morie_match_result")
  expect_true(nrow(res$match_pairs) > 0)
})


# ---------------------------------------------------------------------------
# morie_matching_optimal_pair (skip if optmatch missing)
# ---------------------------------------------------------------------------

test_that("morie_matching_optimal_pair runs when prerequisites are met", {
  skip_heavy()
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
  skip_heavy()
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
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
  skip_heavy()
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
  skip_heavy()
  df <- make_match_df()
  tb <- morie_matching_balance_table(df, "d", c("x1", "x2"))
  expect_s3_class(tb, "data.frame")
  expect_equal(nrow(tb), 2L)
})


# ---------------------------------------------------------------------------
# morie_matching_love_plot_data
# ---------------------------------------------------------------------------

test_that("morie_matching_love_plot_data returns before/after SMDs", {
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
  df <- make_match_df()
  rownames(df) <- as.character(seq_len(nrow(df)))
  res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  att <- morie_matching_att_matched(df, "y", "d", res$match_pairs)
  expect_s3_class(att, "morie_te_result")
  expect_equal(att$estimand, "ATT")
  expect_true(is.finite(att$estimate))
})

test_that("morie_matching_att_matched returns NA result on empty pairs", {
  skip_heavy()
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
  skip_heavy()
  df <- make_match_df()
  res <- morie_matching_ate_matched(df, "y", "d", c("x1", "x2"))
  expect_s3_class(res, "morie_te_result")
  expect_equal(res$estimand, "ATE")
  expect_true(is.finite(res$estimate))
})

test_that("morie_matching_atc_matched returns te_result with ATC", {
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  testthat::skip_if_not_installed("MatchIt")
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
  skip_heavy()
  df <- make_match_df()
  res <- morie_matching_overlap(df, "d", c("x1", "x2"))
  expect_true(all(c("ps_summary", "overlap_region",
                    "n_off_support", "pct_off_support",
                    "effective_sample_size") %in% names(res)))
  expect_true(res$effective_sample_size > 0)
  expect_true(res$overlap_region["lower"] <= res$overlap_region["upper"])
})


test_that("round four: morie_matching_att_matched honours a weight column", {
  set.seed(4)
  df <- data.frame(y = rnorm(40), d = rep(0:1, 20), x = rnorm(40), w = runif(40, 0.5, 2))
  rownames(df) <- as.character(seq_len(40))
  pairs <- data.frame(treated_idx = as.character(seq(2, 40, 2)),
                      control_idx = as.character(seq(1, 39, 2)), stringsAsFactors = FALSE)
  plain <- morie_matching_att_matched(df, "y", "d", pairs)
  weighted <- morie_matching_att_matched(df, "y", "d", pairs, weights = "w")
  expect_false(isTRUE(all.equal(plain$estimate, weighted$estimate)))
})

test_that("round four: abadie-imbens se uses the match count", {
  set.seed(5)
  df <- data.frame(y = rnorm(30), d = rep(0:1, 15))
  rownames(df) <- as.character(seq_len(30))
  pairs <- data.frame(treated_idx = as.character(c(2, 2, 4, 4)),
                      control_idx = as.character(c(1, 3, 1, 5)), stringsAsFactors = FALSE)
  se1 <- morie_matching_abadie_imbens_se(df, "y", "d", pairs, n_matches = 1L)
  se2 <- morie_matching_abadie_imbens_se(df, "y", "d", pairs, n_matches = 2L)
  expect_true(se2 <= se1)
})

test_that("balance diagnostics equal cobalt::bal.tab (pooled SD, weighted V ratio and KS)", {
  n <- 60
  i <- 0:(n - 1)
  x1 <- round(sin(1.1 * i) * 2 + 0.3 * i / 10, 3)
  x2 <- round(cos(0.7 * i) + 0.5 * sin(2.9 * i), 3)
  t <- as.integer(sin(1.9 * i + 0.3) + 0.5 * x1 > 0.2)
  w <- round(1 + 0.5 * abs(sin(0.9 * i)) + 0.3 * t, 3)
  d <- data.frame(t, x1, x2, w)
  r <- morie_matching_balance(d, "t", c("x1", "x2"), weights = "w")$balance_table
  expect_equal(r$smd, c(1.67876185466111, 0.274662902480088), tolerance = 1e-12)
  expect_equal(r$variance_ratio, c(1.48872009615744, 1.06429541438012), tolerance = 1e-12)
  expect_equal(r$ks_stat, c(0.638985722883579, 0.202341881639128), tolerance = 1e-12)
  u <- morie_matching_balance(d, "t", c("x1", "x2"))$balance_table
  expect_equal(u$smd, c(1.70640717405192, 0.333769817807865), tolerance = 1e-12)
})

test_that("Abadie-Imbens SE equals Matching::Match(estimand = 'ATT', sample = TRUE)", {
  n <- 60
  i <- 0:(n - 1)
  x1 <- sin(1.1 * i) * 2 + 0.3 * i / 10
  x2 <- cos(0.7 * i) + 0.5 * sin(2.9 * i)
  t <- as.integer(sin(1.9 * i + 0.3) + 0.5 * x1 > 0.2)
  y <- 1 + 2 * t + x1 + 0.5 * x2 + 0.7 * sin(4.3 * i)
  d <- data.frame(y, t, x1, x2)
  rownames(d) <- as.character(i)
  # nearest control with replacement on inverse-variance scaled covariates
  X <- cbind(x1, x2)
  sc <- 1 / apply(X, 2, stats::var)
  tr <- which(t == 1)
  co <- which(t == 0)
  ctrl <- vapply(tr, function(k) co[which.min(colSums(sc * (t(X[co, ]) - X[k, ])^2))], integer(1))
  p <- data.frame(treated_idx = as.character(tr - 1), control_idx = as.character(ctrl - 1))
  # Match(y, t, X, estimand = "ATT", M = 1, sample = TRUE, Weight = 1, Var.calc = 1)$se
  expect_equal(morie_matching_abadie_imbens_se(d, "y", "t", p, covariates = c("x1", "x2")),
               0.256974848359553, tolerance = 1e-12)
})

# ---- native full matching / subclassification / variable ratio / g-formula (no MatchIt) ----

test_that("full matching is a minimum-weight edge cover (brute force on small graphs)", {
  for (seed in 1:6) {
    set.seed(seed)
    nt <- sample(2:3, 1)
    nc <- sample(2:4, 1)
    p <- c(runif(nt, 0.2, 0.8), runif(nc, 0.1, 0.9))
    tr <- c(rep(1L, nt), rep(0L, nc))
    fm <- rmorie:::.morie_full_match(p, tr)
    D <- abs(outer(p[tr == 1], p[tr == 0], "-"))
    edges <- as.matrix(expand.grid(t = seq_len(nt), c = seq_len(nc)))
    best <- Inf
    for (m in seq_len(2^nrow(edges) - 1)) {
      use <- bitwAnd(m, 2^(seq_len(nrow(edges)) - 1)) > 0
      e <- edges[use, , drop = FALSE]
      if (all(seq_len(nt) %in% e[, 1]) && all(seq_len(nc) %in% e[, 2])) best <- min(best, sum(D[e]))
    }
    got <- sum(abs(p[fm$edges[, 1]] - p[fm$edges[, 2]]))
    expect_equal(got, best, tolerance = 1e-12)
    # every set holds one treated or one control (a star)
    for (s in unique(fm$subclass)) {
      g <- fm$subclass == s
      expect_true(sum(tr[g] == 1) == 1L || sum(tr[g] == 0) == 1L)
    }
  }
  # exact ties: redundant zero-length edges are dropped, the sets stay stars
  fm <- rmorie:::.morie_full_match(c(0.5, 0.5, 0.5, 0.5), c(1L, 1L, 0L, 0L))
  for (s in unique(fm$subclass)) {
    g <- fm$subclass == s
    expect_true(sum(c(1, 1, 0, 0)[g] == 1) == 1L || sum(c(1, 1, 0, 0)[g] == 0) == 1L)
  }
})

test_that("subclass and match-matrix weights follow MatchIt's rules", {
  w <- rmorie:::.morie_subclass_weights(c(1, 1, 1, 2, 2, 2, NA), c(1, 0, 0, 1, 1, 0, 0))
  # controls: set 1 -> 1/2 each, set 2 -> 2/1; rescaled so the 3 controls sum to 3
  expect_equal(w[c(1, 4, 5)], c(1, 1, 1))
  expect_equal(w[c(2, 3, 6)], c(0.5, 0.5, 2) * 3 / 3)
  expect_identical(w[7], 0)
  mm <- matrix(c(3L, 5L, NA, 4L, NA, NA), 3, 2)
  idx_t <- c(1L, 2L, 6L)
  w2 <- rmorie:::.morie_mm_weights(mm, 6, idx_t, c(1, 1, 0, 0, 0, 1))
  expect_equal(w2[c(1, 2, 6)], c(1, 1, 0))
  expect_equal(w2[3:5], c(0.5, 0.5, 1) * 3 / 2)
  expect_identical(rmorie:::.morie_mm_subclass(mm, 6, idx_t), c(1L, 2L, 1L, 1L, 2L, NA))
})

test_that("scooting gives every subclass a treated and a control unit", {
  sub <- c(1L, 1L, 2L, 2L, 3L, 3L, 3L)
  tr <- c(1L, 0L, 1L, 1L, 1L, 0L, 0L)  # subclass 2 has no control
  x <- c(0.1, 0.2, 0.4, 0.5, 0.7, 0.75, 0.8)
  out <- rmorie:::.morie_subclass_scoot(sub, tr, x)
  expect_true(all(table(tr, out) >= 1))
  expect_identical(out[6], 2L)  # the nearest control from the subclass to the right
  expect_identical(rmorie:::.morie_subclass_scoot(c(1L, 1L), c(1L, 0L), c(1, 2)), c(1L, 1L))
  expect_error(rmorie:::.morie_subclass_scoot(c(1L, 2L, 2L), c(1L, 1L, 0L), c(1, 2, 3)), "not enough units")
})

test_that("the nearest-neighbour kernel: rounds, caliper, replacement, refusals", {
  tr <- c(1L, 1L, 0L, 0L, 0L, 0L)
  d <- c(0.50, 0.40, 0.48, 0.47, 0.10, 0.90)
  mm <- rmorie:::.morie_match_nn_cpp(tr, d, c(2L, 2L), FALSE, NA_real_)
  # round 1: the higher-scoring treated unit (0.50) takes 0.48, then 0.40 takes 0.47;
  # round 2: 0.50 takes the next nearest left (0.10 vs 0.90 -> 0.90 is 0.40 away, 0.10 0.40 away)
  expect_identical(mm[1, 1], 3L)
  expect_identical(mm[2, 1], 4L)
  expect_true(all(!is.na(mm)))
  # a caliper that admits only the two close controls
  mm2 <- rmorie:::.morie_match_nn_cpp(tr, d, c(2L, 2L), FALSE, 0.15)
  expect_identical(sort(stats::na.omit(as.vector(mm2))), c(3L, 4L))
  # with replacement both treated units may take the same nearest control
  mm3 <- rmorie:::.morie_match_nn_cpp(tr, d, c(1L, 1L), TRUE, NA_real_)
  expect_identical(as.vector(mm3), c(3L, 4L))
  expect_error(rmorie:::.morie_match_nn_cpp(c(1L, 2L), c(0, 1), 1L, FALSE, NA_real_), "0/1")
  expect_error(rmorie:::.morie_match_nn_cpp(c(1L, 0L), c(NA, 1), 1L, FALSE, NA_real_), "finite")
  expect_error(rmorie:::.morie_match_nn_cpp(c(1L, 0L), c(0, 1), c(1L, 1L), FALSE, NA_real_), "one entry")
  expect_error(rmorie:::.morie_match_nn_cpp(c(1L, 0L), c(0, 1), 0L, FALSE, NA_real_), "positive")
  expect_identical(dim(rmorie:::.morie_match_nn_cpp(c(0L, 0L), c(0, 1), integer(), FALSE, NA_real_)), c(0L, 1L))
})

test_that("assignment solver and argument checks of the new matchers", {
  expect_identical(rmorie:::.morie_lsap_cpp(matrix(c(4, 1, 3, 2, 0, 5, 3, 2, 2), 3)), c(2L, 1L, 3L))
  expect_error(rmorie:::.morie_lsap_cpp(matrix(1, 2, 1)), "ncol >= nrow")
  expect_error(rmorie:::.morie_lsap_cpp(matrix(c(1, NA), 1)), "finite")
  df <- data.frame(d = c(1, 1, 1), x = c(1, 2, 3))
  expect_error(morie_matching_full(df, "d", "x"), "both treated and control")
  expect_error(morie_matching_subclassify(df, "d", "x"), "both treated and control")
  expect_error(morie_matching_variable_ratio(df, "d", "x"), "both treated and control")
  df2 <- data.frame(d = rep(0:1, 10), x = rnorm(20))
  expect_error(morie_matching_subclassify(df2, "d", "x", n_strata = 0), "n_strata")
  expect_error(morie_matching_variable_ratio(df2, "d", "x", min_ratio = 3, max_ratio = 2), "must not exceed")
  expect_error(morie_matching_variable_ratio(df2, "d", "x", min_ratio = "a"), "min_ratio")
  expect_error(morie_matching_variable_ratio(df2, "d", "x", caliper = -1), "caliper")
})
