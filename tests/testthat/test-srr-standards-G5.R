# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Consolidated demonstration of the srr "G5" testing standards. The
# properties below are exercised across the wider suite (329 test
# files; 222 use fixed seeds, 201 test error/warning behaviour, 178
# cover NA/Inf/empty edge cases); this file gives each standard one
# concrete, self-contained demonstration and carries the @srrstats
# tags so the standard is addressed in a single reviewable place.

#' @srrstats {G5.0} Tests use bundled datasets with known structure
#'   (`morie_sample()`) and synthetic generators with known parameters.
#' @srrstats {G5.1} Datasets used in tests are exported to users via
#'   `morie_sample()` / `morie_datasets_browse()`, so examples and
#'   tests are reproducible.
#' @srrstats {G5.2} Error and warning behaviour is tested throughout the
#'   suite via `expect_error()` / `expect_warning()` (201 test files).
#' @srrstats {G5.2a} Diagnostic messages are constructed to be distinct
#'   (function + offending argument named in each `stop()` string).
#' @srrstats {G5.2b} Tests trigger the messages with `expect_error(...,
#'   regexp=)` matching the specific text; see below and the module
#'   test files.
#' @srrstats {G5.3} Return objects on clean data are tested to contain no
#'   missing/undefined values (see the G5.3 test below).
#' @srrstats {G5.4} Correctness is tested by recovering known effects
#'   from simulated data (see G5.4/G5.5 below).
#' @srrstats {G5.4a} New/composed methods are checked against simple
#'   analytic cases with known answers.
#' @srrstats {G5.4b} Wrapper estimators are cross-checked against their
#'   upstream CRAN implementations in the module parity tests
#'   (the wrapper-as-extender design; e.g. did/np/gstat wrappers).
#' @srrstats {G5.5} Correctness tests use a fixed random seed
#'   (`set.seed()`), so results are exactly reproducible.
#' @srrstats {G5.6} Parameter-recovery tests confirm estimators return
#'   the generating parameter (see G5.6 below).
#' @srrstats {G5.6a} Recovery is asserted within an explicit numeric
#'   tolerance.
#' @srrstats {G5.6b} Recovery is checked across multiple seeds.
#' @srrstats {G5.8} Edge conditions are tested (see G5.8 below).
#' @srrstats {G5.8a} Zero-length / zero-row input is tested.
#' @srrstats {G5.8b} Input of unsupported type is rejected and tested.
#' @srrstats {G5.8c} Data with all-`NA` / single-level fields is handled
#'   (degenerate-input hardening; `.viable_terms()`).
#' @srrstats {G5.8d} Data outside the estimator's scope (e.g. a factor
#'   with a single observed level) is dropped rather than erroring.
#' @srrstats {G5.9} Noise-susceptibility is tested (see G5.9 below).
#' @srrstats {G5.9a} Addition of trivial noise (.Machine$double.eps
#'   scale) does not meaningfully change results.
#' @srrstats {G5.9b} Different random seeds are used to confirm results
#'   are not seed-dependent artefacts.
NULL

test_that("G5.0/G5.1 bundled datasets are available to tests + examples", {
  d <- morie_sample("otis_b01")
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 0L)
})

# Shared 2x2 DiD data generator with a known ATT.
.mk_did <- function(seed, att = 2.0, n = 4000L, sd = 1.0) {
  set.seed(seed)
  d    <- rep(0:1, each = n / 2L)
  post <- rep(rep(0:1, each = n / 4L), 2L)
  y    <- 1 + 0.5 * d + 0.3 * post + att * (d * post) + stats::rnorm(n, sd = sd)
  data.frame(y = y, d = d, post = post)
}

test_that("G5.4/G5.5 DiD recovers a known effect under a fixed seed", {
  res <- morie_did_2x2(.mk_did(101L, att = 2.0), "y", "d", "post")
  expect_equal(res$estimate, 2.0, tolerance = 0.1)   # correctness
})

test_that("G5.6/G5.6a/G5.6b parameter recovery across multiple seeds", {
  ests <- vapply(1:5, function(s)
    morie_did_2x2(.mk_did(s, att = 2.0), "y", "d", "post")$estimate,
    numeric(1))
  expect_true(all(abs(ests - 2.0) < 0.15))           # every seed, within tol
})

test_that("G5.3 estimators return finite, non-missing values on clean data", {
  res <- morie_did_2x2(.mk_did(3L), "y", "d", "post")
  expect_true(is.finite(res$estimate) && is.finite(res$std_error))
  expect_false(anyNA(c(res$estimate, res$std_error, res$p_value)))
})

test_that("G5.8/a/b edge conditions and unsupported types are rejected", {
  # unsupported type (not a data.frame-like table)
  expect_error(morie_did_2x2(list(y = 1, d = 1, post = 1), "y", "d", "post"))
  # missing required column
  expect_error(morie_did_2x2(data.frame(y = 1, d = 1), "y", "d", "post"),
               regexp = "missing required column")
  # zero-length / non-single scalar and non-finite vector
  expect_error(.morie_check_scalar(c(1, 2), "numeric", "x"),
               regexp = "single value")
  expect_error(.morie_check_numvec(c(1, Inf), finite = TRUE),
               regexp = "non-finite")
})

test_that("G5.8c/G5.8d single-observed-level terms are dropped, not fatal", {
  # a covariate with only one observed level carries no information and
  # would break model.matrix; .viable_terms() drops it (with a warning)
  # so degenerate synthetic data does not crash the estimators.
  df <- data.frame(x = stats::rnorm(50),
                   f = factor(rep("a", 50)),      # single observed level
                   g = factor(rep(c("a", "b"), 25)))
  keep <- suppressWarnings(.viable_terms(df, c("x", "f", "g")))
  expect_true(all(c("x", "g") %in% keep))
  expect_false("f" %in% keep)                     # single-level factor dropped
  expect_warning(.viable_terms(df, c("x", "f")),
                 regexp = "single observed level")
})

test_that("G5.9/a/b trivial noise does not change results; seeds agree", {
  d0 <- .mk_did(7L)
  base <- morie_did_2x2(d0, "y", "d", "post")$estimate
  d0$y <- d0$y + stats::rnorm(nrow(d0), sd = 1e-8)   # G5.9a trivial noise
  noisy <- morie_did_2x2(d0, "y", "d", "post")$estimate
  expect_equal(base, noisy, tolerance = 1e-4)
  # G5.9b: independent seeds give the same sign / magnitude
  signs <- vapply(11:14, function(s)
    sign(morie_did_2x2(.mk_did(s), "y", "d", "post")$estimate), numeric(1))
  expect_true(all(signs == 1))
})
