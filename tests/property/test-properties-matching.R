# SPDX-License-Identifier: AGPL-3.0-or-later
# Property-based invariants for the native matcher, across random DGPs.
library(testthat)
library(rmorie)

test_that("matching invariants hold across random DGPs", {
  for (seed in c(1L, 17L, 99L, 1234L)) {
    set.seed(seed)
    n <- sample(200:800, 1)
    x1 <- rnorm(n); x2 <- runif(n)
    d <- rbinom(n, 1, plogis(x1 - x2))
    df <- data.frame(d = d, x1 = x1, x2 = x2)
    m <- sample(1:2, 1)
    r <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"),
                                         n_neighbors = m)
    p <- r$match_pairs
    # (a) pair count bounded by n_treated * m and by n_control
    expect_lte(nrow(p), sum(d == 1) * m)
    expect_lte(nrow(p), sum(d == 0))
    # (b) every pair's members exist and have the right arms
    expect_true(all(df[p$treated_idx, "d"] == 1))
    expect_true(all(df[p$control_idx, "d"] == 0))
    # (c) no control reused without replacement
    expect_false(any(duplicated(p$control_idx)))
    # (d) distances are finite and non-negative
    expect_true(all(is.finite(p$distance) & p$distance >= 0))
    # (e) matched_data contains exactly the matched units
    expect_setequal(rownames(r$matched_data),
                    unique(c(p$treated_idx, p$control_idx)))
  }
})

test_that("property: exact/CEM invariants hold across random seeds", {
  for (seed in c(3L, 17L, 29L, 71L)) {
    set.seed(seed)
    n <- 400L
    g <- sample(letters[1:5], n, replace = TRUE)
    x <- rnorm(n)
    d <- rbinom(n, 1, plogis(-0.8 + 0.4 * (g %in% c("a", "b")) + 0.3 * x))
    if (length(unique(d)) < 2) next
    df <- data.frame(g = g, x = x, d = d)

    ex <- morie_matching_exact(df, "d", "g")
    md <- ex$matched_data
    # every retained stratum has both arms
    for (k in unique(md$g)) {
      expect_setequal(unique(md$d[md$g == k]), c(0, 1))
    }
    # weights positive, treated weight exactly 1
    expect_true(all(md$weights > 0))
    expect_true(all(md$weights[md$d == 1] == 1))
    # matched data is a subset of the input rows
    expect_lte(nrow(md), n)

    cm <- morie_matching_cem(df, "d", c("g", "x"), n_bins = 4L)
    cmd <- cm$matched_data
    expect_true(all(cmd$weights > 0))
    expect_lte(nrow(cmd), n)
    expect_true(cm$details$l1_before >= 0 && cm$details$l1_before <= 1)
    # CEM strata are at least as fine as dropping x entirely
    expect_gte(cm$details$n_strata, 1L)
  }
})

test_that("property: optimal matching invariants across seeds", {
  for (seed in c(5L, 19L, 53L)) {
    set.seed(seed)
    n <- 300L
    x1 <- rnorm(n); x2 <- rnorm(n)
    d <- rbinom(n, 1, plogis(-1.4 + 0.5 * x1))
    if (sum(d) < 2 || sum(d) > sum(1 - d)) next
    df <- data.frame(x1, x2, d)
    r <- morie_matching_optimal_pair(df, "d", c("x1", "x2"))
    # every treated matched exactly once, no control reused
    expect_equal(nrow(r$match_pairs), sum(d))
    expect_false(any(duplicated(r$match_pairs$control_idx)))
    expect_false(any(duplicated(r$match_pairs$treated_idx)))
    # pairs reference real rows of opposite arms
    expect_true(all(df[r$match_pairs$treated_idx, "d"] == 1))
    expect_true(all(df[r$match_pairs$control_idx, "d"] == 0))
    expect_true(all(is.finite(r$match_pairs$distance)))
  }
})

test_that("property: genetic matching invariants across seeds", {
  for (seed in c(7L, 23L)) {
    set.seed(seed)
    n <- 300L
    x1 <- rnorm(n); x2 <- rnorm(n)
    d <- rbinom(n, 1, plogis(-1.4 + 0.5 * x1))
    if (sum(d) < 5 || sum(d) > sum(1 - d)) next
    df <- data.frame(x1, x2, d)
    r <- morie_matching_genetic(df, "d", c("x1", "x2"),
                                pop_size = 8L, n_generations = 2L,
                                seed = seed)
    expect_true(all(r$details$best_weights > 0))
    expect_true(all(is.finite(r$match_pairs$distance)))
    expect_false(any(duplicated(r$match_pairs$control_idx)))
    expect_true(all(df[r$match_pairs$treated_idx, "d"] == 1))
    expect_true(all(df[r$match_pairs$control_idx, "d"] == 0))
  }
})

test_that("property: cardinality invariants across seeds", {
  for (seed in c(11L, 37L)) {
    set.seed(seed)
    n <- 1200L
    x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rbinom(n, 1, 0.4)
    dd <- rbinom(n, 1, plogis(-1.7 + 0.4 * x1 - 0.35 * x2 + 0.4 * x3))
    d <- data.frame(y = rnorm(n), d = dd, x1 = x1, x2 = x2, x3 = x3)
    r <- morie_matching_cardinality(d, "d", c("x1", "x2", "x3"))
    expect_identical(r$method, "cardinality")
    expect_true(nrow(r$matched_data) > 0)
    expect_false(any(duplicated(r$match_pairs$control_idx)))
    if (is.null(r$details$warning)) {
      bal <- morie_matching_balance(r$matched_data, "d",
                                    c("x1", "x2", "x3"))
      expect_lte(bal$max_smd, r$details$balance_threshold)
    }
  }
})

test_that("property: causal forest invariants across seeds", {
  for (seed in c(13L, 41L)) {
    set.seed(seed)
    n <- 700L
    X <- matrix(rnorm(n * 2), n, 2)
    w <- rbinom(n, 1, plogis(0.4 * X[, 1]))
    if (length(unique(w)) < 2) next
    y <- 0.6 * w + X[, 1] + rnorm(n)
    nf <- rmorie:::.morie_causal_forest_native(X, y, w, n_trees = 100L,
                                               random_state = seed)
    expect_true(all(is.finite(nf$tau)))
    expect_true(all(nf$ps > 0 & nf$ps < 1))
    # determinism given seed
    nf2 <- rmorie:::.morie_causal_forest_native(X, y, w, n_trees = 100L,
                                                random_state = seed)
    expect_identical(nf$tau, nf2$tau)
  }
})

test_that("property: meta-learner invariants across seeds", {
  for (seed in c(9L, 31L)) {
    set.seed(seed)
    n <- 900L
    x1 <- rnorm(n); x2 <- rnorm(n)
    d <- rbinom(n, 1, plogis(0.4 * x1))
    if (length(unique(d)) < 2) next
    y <- (1 + 0.5 * x2) * d + x1 + rnorm(n)
    df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
    for (ml in c("x_learner", "dr_learner")) {
      tau <- morie_estimate_cate(df, "d", "y", c("x1", "x2"),
                                 meta_learner = ml)
      expect_true(all(is.finite(tau)))
      # determinism
      tau2 <- morie_estimate_cate(df, "d", "y", c("x1", "x2"),
                                  meta_learner = ml)
      expect_identical(tau, tau2)
    }
  }
})
