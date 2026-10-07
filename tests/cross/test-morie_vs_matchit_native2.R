# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation of the native matchers that replaced the last MatchIt / optmatch / stdReg
# calls: nearest neighbour with several controls (MatchIt's rounds), variable-ratio (extremal)
# matching, propensity-score subclassification (with scooting), optimal full matching, and the
# g-formula's sandwich SE. MatchIt, optmatch and stdReg are TEST-ONLY dependencies.
library(testthat)
library(rmorie)

sim <- function(seed, n) {
  set.seed(seed)
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = sample(0:2, n, TRUE))
  df$d <- rbinom(n, 1, plogis(-0.7 + 0.8 * df$x1 - 0.5 * df$x2 + 0.3 * df$x3))
  rownames(df) <- paste0("u", seq_len(n))
  df
}
pk <- function(t, c) sort(paste(t, c))
mm_pairs <- function(mm) pk(rep(rownames(mm), ncol(mm))[!is.na(mm)], as.character(mm)[!is.na(mm)])

test_that("nearest neighbour is pair-identical to MatchIt for k = 1..3, with/without replacement and caliper", {
  skip_if_not_installed("MatchIt")
  for (seed in 1:4) {
    df <- sim(seed, c(80, 200, 500, 150)[seed])
    for (k in 1:3) for (rep_ in c(FALSE, TRUE)) for (cal in list(NULL, 0.2)) {
      nat <- suppressWarnings(morie_matching_nearest_neighbor(df, "d", c("x1", "x2", "x3"), n_neighbors = k,
                                                              caliper = cal, replace = rep_))
      mi <- suppressWarnings(MatchIt::matchit(d ~ x1 + x2 + x3, data = df, method = "nearest",
                                              distance = "glm", link = "linear.logit",
                                              m.order = if (rep_) NULL else "largest",
                                              ratio = k, replace = rep_, caliper = cal))
      expect_identical(pk(nat$match_pairs$treated_idx, nat$match_pairs$control_idx), mm_pairs(mi$match.matrix),
                       label = sprintf("seed %d k %d replace %s caliper %s", seed, k, rep_, format(cal)))
    }
  }
})

test_that("variable-ratio matching reproduces MatchIt's pairs, weights and subclasses", {
  skip_if_not_installed("MatchIt")
  for (seed in 1:4) {
    df <- sim(seed, c(80, 200, 500, 150)[seed])
    for (b in list(c(1, 3), c(1, 4), c(2, 5))) {
      vr <- suppressWarnings(morie_matching_variable_ratio(df, "d", c("x1", "x2", "x3"),
                                                           min_ratio = b[1], max_ratio = b[2]))
      target <- max(min(b[2] - 1, ceiling(sum(b) / 2)), b[1])
      mi <- suppressWarnings(MatchIt::matchit(d ~ x1 + x2 + x3, data = df, method = "nearest", ratio = target,
                                              min.controls = b[1], max.controls = b[2], caliper = 0.2))
      expect_identical(pk(vr$match_pairs$treated_idx, vr$match_pairs$control_idx), mm_pairs(mi$match.matrix))
      expect_equal(unname(vr$matched_data$weights), unname(mi$weights[mi$weights > 0]), tolerance = 1e-12)
      expect_identical(as.integer(vr$matched_data$subclass), as.integer(mi$subclass[mi$weights > 0]))
    }
  }
})

test_that("subclassification reproduces MatchIt's subclasses and weights, scooting included", {
  skip_if_not_installed("MatchIt")
  for (seed in 1:4) {
    df <- sim(seed, c(80, 200, 500, 150)[seed])
    for (k in c(5L, 8L)) {
      sc <- morie_matching_subclassify(df, "d", c("x1", "x2", "x3"), n_strata = k)
      ms <- suppressWarnings(MatchIt::matchit(d ~ x1 + x2 + x3, data = df, method = "subclass",
                                              subclass = k, estimand = "ATT"))
      expect_identical(as.integer(sc$data_with_strata$subclass), as.integer(ms$subclass))
      expect_equal(sc$data_with_strata$weights, unname(ms$weights), tolerance = 1e-12)
    }
  }
  set.seed(9)
  df <- data.frame(x1 = c(rnorm(70), rnorm(10, 3)))
  df$d <- c(rbinom(70, 1, 0.3), rep(1, 10))
  sc <- suppressWarnings(morie_matching_subclassify(df, "d", "x1", n_strata = 6))
  ms <- suppressWarnings(MatchIt::matchit(d ~ x1, data = df, method = "subclass", subclass = 6, estimand = "ATT"))
  expect_identical(as.integer(sc$data_with_strata$subclass), as.integer(ms$subclass))
})

test_that("optimal full matching attains optmatch's optimum (exactly, at a tight tolerance)", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("optmatch")
  for (seed in 1:4) {
    df <- sim(seed, c(60, 200, 400, 150)[seed])
    nat <- morie_matching_full(df, "d", c("x1", "x2", "x3"))
    mi <- MatchIt::matchit(d ~ x1 + x2 + x3, data = df, method = "full", estimand = "ATT", tol = 1e-9)
    ps <- mi$distance
    tot <- sum(vapply(split(seq_len(nrow(df)), mi$subclass), function(ix) {
      t <- ix[df$d[ix] == 1]
      c <- ix[df$d[ix] == 0]
      if (length(t) == 1L) sum(abs(ps[c] - ps[t])) else sum(abs(ps[t] - ps[c]))
    }, 0))
    expect_equal(nat$details$total_distance, tot, tolerance = 1e-10)
    expect_equal(nat$matched_data$distance, unname(ps), tolerance = 1e-12)
  }
})

test_that("the g-formula SE is stdReg's sandwich", {
  skip_if_not_installed("stdReg")
  for (seed in 1:3) for (model in c("linear", "logistic")) {
    set.seed(seed)
    n <- 300
    df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
    df$a <- rbinom(n, 1, plogis(0.4 * df$x1))
    df$y <- if (model == "linear") 1 + 0.7 * df$a + df$x1 - 0.5 * df$x2 + rnorm(n) else
      rbinom(n, 1, plogis(-0.3 + 0.8 * df$a + 0.6 * df$x1))
    df$x2[c(5, 40)] <- NA
    fit <- glm(y ~ a + x1 + x2, data = df, family = if (model == "linear") gaussian() else binomial())
    s <- summary(stdReg::stdGlm(fit = fit, data = df, X = "a", x = c(0, 1)),
                 contrast = "difference", reference = 0)$est.table
    r <- morie_estimate_g_computation(df, "a", "y", c("x1", "x2"), outcome_model = model)
    expect_equal(c(r$ate, r$se), unname(c(s["1", "Estimate"], s["1", "Std. Error"])), tolerance = 1e-10)
  }
})
