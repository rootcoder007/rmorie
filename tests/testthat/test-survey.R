# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for R/survey.R -- design constructor, HT total, Hajek mean, ratio,
# post-stratify, raking calibrate, subpop, complex GLM.

.make_survey_df <- function(n = 100L, seed = 1L) {
  set.seed(seed)
  data.frame(
    id     = seq_len(n),
    y      = rnorm(n, 10, 2),
    x      = rnorm(n, 5, 1),
    w      = runif(n, 0.5, 1.5),
    s      = sample(c("A", "B"), n, replace = TRUE),
    cl     = sample(seq_len(10), n, replace = TRUE),
    fpc    = rep(1000L, n)
  )
}

# ---------------------------------------------------------------------------
# morie_survey_design
# ---------------------------------------------------------------------------

test_that("morie_survey_design with survey installed returns survey.design", {
  df <- .make_survey_df()
  d <- morie_survey_design(df, weights_col = "w")
  expect_true(inherits(d, "survey.design") ||
              inherits(d, "survey.design2"))
})

test_that("morie_survey_design errors on missing weights column", {
  df <- .make_survey_df()
  expect_error(morie_survey_design(df, weights_col = "nope"),
               regexp = "not in data")
})

test_that("morie_survey_design with stratum + cluster builds object", {
  df <- .make_survey_df()
  d <- morie_survey_design(df, weights_col = "w",
                           strata_col = "s", cluster_col = "cl",
                           nest = TRUE)
  expect_true(inherits(d, "survey.design") ||
              inherits(d, "survey.design2"))
})

# ---------------------------------------------------------------------------
# morie_survey_ht_total
# ---------------------------------------------------------------------------

test_that("morie_survey_ht_total: equal pi recovers sum(y)/pi", {
  y <- c(1, 2, 3, 4, 5)
  pi <- rep(0.5, 5)
  res <- morie_survey_ht_total(y, pi)
  expect_equal(res$total, sum(y) / 0.5, tolerance = 1e-3)
  expect_true(res$se >= 0)
  expect_true(res$ci_lower < res$total && res$ci_upper > res$total)
})

test_that("morie_survey_ht_total has zero SE when pi == 1", {
  res <- morie_survey_ht_total(c(2, 4, 6), c(1, 1, 1))
  expect_equal(res$total, 12, tolerance = 1e-3)
  expect_equal(res$se, 0, tolerance = 1e-6)
})

test_that("morie_survey_ht_total rejects invalid inclusion probs", {
  expect_error(morie_survey_ht_total(c(1, 2), c(0, 1)),
               regexp = "inclusion_probs")
  expect_error(morie_survey_ht_total(c(1, 2), c(0.5, 1.5)),
               regexp = "inclusion_probs")
  expect_error(morie_survey_ht_total(c(1, 2), c(0.5)),
               regexp = "same length")
})

# ---------------------------------------------------------------------------
# morie_survey_hajek_mean
# ---------------------------------------------------------------------------

test_that("morie_survey_hajek_mean: equal weights == simple mean", {
  y <- c(2, 4, 6, 8)
  w <- rep(1, 4)
  res <- morie_survey_hajek_mean(y, w)
  expect_equal(res$mean, mean(y), tolerance = 1e-3)
})

test_that("morie_survey_hajek_mean: weighted mean of c(2,4) w=c(1,3) = 3.5", {
  res <- morie_survey_hajek_mean(c(2, 4), c(1, 3))
  expect_equal(res$mean, 3.5, tolerance = 1e-3)
})

test_that("morie_survey_hajek_mean rejects bad input", {
  expect_error(morie_survey_hajek_mean(c(1, 2), c(1, -1)),
               regexp = "must be > 0")
  expect_error(morie_survey_hajek_mean(c(1, 2, 3), c(1, 1)),
               regexp = "same length")
  expect_error(morie_survey_hajek_mean(c(1), c(1)),
               regexp = ">=")
})

# ---------------------------------------------------------------------------
# morie_survey_mean
# ---------------------------------------------------------------------------

test_that("morie_survey_mean works on fallback design", {
  # build a fallback object directly by skipping survey check via class fallback
  df <- .make_survey_df(50)
  design_fb <- structure(list(data = df, weights = df$w),
                         class = "morie_survey_design_fallback")
  res <- morie_survey_mean(design_fb, "y")
  expect_true(is.numeric(res$mean))
  expect_true(res$se >= 0)
})

test_that("morie_survey_mean wraps svymean for survey designs", {
  df <- .make_survey_df()
  des <- morie_survey_design(df, weights_col = "w")
  res <- morie_survey_mean(des, "y")
  expect_true(is.numeric(res$mean))
})

# ---------------------------------------------------------------------------
# morie_survey_ratio
# ---------------------------------------------------------------------------

test_that("morie_survey_ratio recovers known ratio for proportional data", {
  set.seed(1)
  x <- runif(100, 1, 10)
  y <- 3 * x  # exact ratio = 3
  w <- rep(1, 100)
  res <- morie_survey_ratio(y, x, w, X_population_total = sum(x) * 2)
  expect_equal(res$ratio, 3, tolerance = 1e-3)
  expect_equal(res$total_estimate, 3 * sum(x) * 2, tolerance = 1e-3)
})

test_that("morie_survey_ratio rejects invalid inputs", {
  expect_error(morie_survey_ratio(c(1, 2), c(1, 2), c(1, -1), 10),
               regexp = "must be > 0")
  expect_error(morie_survey_ratio(c(1), c(1, 2), c(1, 1), 10),
               regexp = "same length")
  expect_error(morie_survey_ratio(c(1, 2), c(1, 2), c(1, 1), 0),
               regexp = "must be > 0")
  expect_error(morie_survey_ratio(c(1, 2), c(0, 0), c(1, 1), 10),
               regexp = "zero")
})

# ---------------------------------------------------------------------------
# morie_survey_poststratify
# ---------------------------------------------------------------------------

test_that("morie_survey_poststratify gives weights summing close to n", {
  df <- data.frame(s = rep(c("a", "b"), each = 5))
  pop <- list(a = 100, b = 200)
  w <- morie_survey_poststratify(df, "s", pop)
  expect_length(w, nrow(df))
  # Group means: a-stratum weight, b-stratum weight
  expect_equal(unique(w[df$s == "a"]),
               (100 / 300) / (5 / 10), tolerance = 1e-3)
  expect_equal(unique(w[df$s == "b"]),
               (200 / 300) / (5 / 10), tolerance = 1e-3)
})

test_that("morie_survey_poststratify errors on missing stratum in pop counts", {
  df <- data.frame(s = c("a", "b", "c"))
  pop <- list(a = 10, b = 20)
  expect_error(morie_survey_poststratify(df, "s", pop),
               regexp = "present in sample")
})

test_that("morie_survey_poststratify errors on missing column", {
  df <- data.frame(s = c("a", "b"))
  expect_error(morie_survey_poststratify(df, "nope", list(a = 1)),
               regexp = "not in df")
})

# ---------------------------------------------------------------------------
# morie_survey_calibrate (single-var raking)
# ---------------------------------------------------------------------------

test_that("morie_survey_calibrate matches target totals", {
  set.seed(1)
  df <- data.frame(x = rep(1, 50))
  w <- morie_survey_calibrate(df, "x", list(x = 100), max_iter = 20)
  expect_equal(sum(w * df$x), 100, tolerance = 1e-3)
})

test_that("morie_survey_calibrate errors on missing aux var", {
  df <- data.frame(x = rep(1, 5))
  expect_error(morie_survey_calibrate(df, "nope", list(nope = 1)),
               regexp = "not in df")
  expect_error(morie_survey_calibrate(df, "x", list()),
               regexp = "missing")
})

# ---------------------------------------------------------------------------
# morie_survey_subpop
# ---------------------------------------------------------------------------

test_that("morie_survey_subpop returns weighted domain mean", {
  set.seed(1)
  df <- data.frame(d = c(1, 1, 0, 0, 1),
                   y = c(10, 20, 30, 40, 30),
                   w = c(1, 1, 1, 1, 1))
  res <- morie_survey_subpop(df, "d", 1, "y", "w")
  expect_equal(res$mean, mean(c(10, 20, 30)), tolerance = 1e-3)
  expect_equal(res$n_domain, 3)
})

test_that("morie_survey_subpop errors when domain empty", {
  df <- data.frame(d = c(0, 0), y = c(1, 2), w = c(1, 1))
  expect_error(morie_survey_subpop(df, "d", 99, "y", "w"),
               regexp = "match")
})

# ---------------------------------------------------------------------------
# morie_survey_glm / morie_survey_complex_glm
# ---------------------------------------------------------------------------

test_that("morie_survey_complex_glm fits a gaussian svyglm", {
  df <- .make_survey_df(80)
  fit <- morie_survey_complex_glm(df, y ~ x, weight_col = "w",
                                  family = "gaussian")
  expect_s3_class(fit, "svyglm")
})

test_that("morie_survey_complex_glm rejects non-positive weights", {
  df <- .make_survey_df(20)
  df$w[1] <- -1
  expect_error(morie_survey_complex_glm(df, y ~ x, weight_col = "w"),
               regexp = "> 0")
})
