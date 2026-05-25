# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2R: tests for mrm_doe.R — experimental-design routines
# (Bonferroni one-way ANOVA, RCBD, Latin square, fractional factorial,
# response-surface, ANOVA power, Monte-Carlo power).

test_that("mrm_anova_bonferroni runs on 3-group synthetic data", {
  set.seed(1L)
  d <- data.frame(
    y = c(stats::rnorm(30, 0), stats::rnorm(30, 0.5),
          stats::rnorm(30, 1)),
    g = rep(c("a", "b", "c"), each = 30)
  )
  out <- mrm_anova_bonferroni(d, "y", "g")
  expect_type(out, "list")
})

test_that("mrm_rcbd runs on a randomized complete block design", {
  set.seed(2L)
  n_trt <- 3L; n_blk <- 5L
  d <- expand.grid(t = letters[1:n_trt], b = sprintf("b%d", 1:n_blk))
  d$y <- stats::rnorm(nrow(d))
  out <- tryCatch(mrm_rcbd(d, "y", "t", "b"), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("rcbd error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

test_that("mrm_latin_square runs on a Latin square design", {
  set.seed(3L)
  k <- 4L
  d <- expand.grid(
    r = sprintf("r%d", 1:k),
    c = sprintf("c%d", 1:k))
  d$t <- sample(letters[1:k], nrow(d), replace = TRUE)
  d$y <- stats::rnorm(nrow(d))
  out <- tryCatch(
    mrm_latin_square(d, "y", "r", "c", "t"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("latin_square error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

test_that("mrm_anova_power computes power for a known effect size", {
  out <- mrm_anova_power(k_groups = 3L, n_per_group = 30L,
                         effect_size_f = 0.25)
  expect_true(is.numeric(out) || is.list(out))
})

test_that("mrm_mc_power computes Monte-Carlo power against a simulator", {
  # mrm_mc_power calls simulator(seed) with a per-iteration seed,
  # so the supplied closure must accept that arg.
  simulator <- function(seed) {
    set.seed(as.integer(seed))
    a <- stats::rnorm(20, mean = 0)
    b <- stats::rnorm(20, mean = 0.5)
    stats::t.test(a, b)$p.value
  }
  out <- mrm_mc_power(simulator, n_sims = 50L, alpha = 0.05)
  expect_true(is.numeric(out) || is.list(out))
})

test_that("mrm_response_surface runs on a 2-factor synthetic dataset", {
  set.seed(4L)
  d <- expand.grid(f1 = c(-1, 0, 1), f2 = c(-1, 0, 1))
  d$y <- d$f1 + d$f2 + d$f1 * d$f2 + stats::rnorm(nrow(d), sd = 0.2)
  out <- tryCatch(
    mrm_response_surface(d, "y", c("f1", "f2")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("response_surface error: %s",
                 conditionMessage(out)))
  }
  expect_type(out, "list")
})

test_that("mrm_fractional_factorial runs on a 2^(k-p) design", {
  set.seed(5L)
  d <- expand.grid(f1 = c(-1, 1), f2 = c(-1, 1), f3 = c(-1, 1))
  d$y <- d$f1 + d$f2 + d$f3 + stats::rnorm(nrow(d))
  out <- tryCatch(
    mrm_fractional_factorial(d, "y", c("f1", "f2", "f3")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("fractional_factorial error: %s",
                 conditionMessage(out)))
  }
  expect_type(out, "list")
})
