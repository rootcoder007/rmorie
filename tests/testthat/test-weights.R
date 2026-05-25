# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for R/weights.R -- design, rake, GREG, trim, smooth, nonresponse,
# combined, normalize, ESS, deff, replicate variance (JK1/BRR/Fay/SDR/bootstrap),
# multiframe.

# ---------------------------------------------------------------------------
# morie_weights_design
# ---------------------------------------------------------------------------

test_that("morie_weights_design returns 1/pi", {
  p <- c(0.1, 0.2, 0.5, 1.0)
  w <- morie_weights_design(p)
  expect_equal(w, c(10, 5, 2, 1), tolerance = 1e-6)
})

test_that("morie_weights_design rejects bad probabilities", {
  expect_error(morie_weights_design(c(0, 0.5)), regexp = "> 0")
  expect_error(morie_weights_design(c(0.5, 1.1)), regexp = "<= 1")
})

# ---------------------------------------------------------------------------
# morie_weights_poststratify
# ---------------------------------------------------------------------------

test_that("morie_weights_poststratify aligns within-stratum sums to targets", {
  set.seed(1)
  w <- rep(1, 6)
  s <- c("a", "a", "a", "b", "b", "b")
  pop <- list(a = 60, b = 90)
  out <- morie_weights_poststratify(w, s, pop)
  expect_equal(sum(out[s == "a"]), 60, tolerance = 1e-6)
  expect_equal(sum(out[s == "b"]), 90, tolerance = 1e-6)
})

test_that("morie_weights_poststratify warns on missing strata in pop", {
  expect_warning(morie_weights_poststratify(rep(1, 3),
                                            c("a", "b", "c"), list(a = 10)),
                 regexp = "missing")
})

# ---------------------------------------------------------------------------
# morie_weights_rake
# ---------------------------------------------------------------------------

test_that("morie_weights_rake matches marginal targets on a 2-var design", {
  set.seed(1)
  n <- 100
  df <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE),
    age = sample(c("Y", "O"), n, replace = TRUE)
  )
  margins <- list(
    sex = c(M = 500, F = 500),
    age = c(Y = 400, O = 600)
  )
  res <- morie_weights_rake(rep(1, n), df, margins,
                            max_iter = 200, tol = 1e-8)
  expect_true(res$converged)
  expect_equal(sum(res$weights[df$sex == "M"]), 500, tolerance = 1e-3)
  expect_equal(sum(res$weights[df$sex == "F"]), 500, tolerance = 1e-3)
  expect_equal(sum(res$weights[df$age == "Y"]), 400, tolerance = 1e-3)
  expect_equal(sum(res$weights[df$age == "O"]), 600, tolerance = 1e-3)
})

test_that("morie_weights_rake errors on missing column", {
  df <- data.frame(s = c("a", "b"))
  expect_error(
    morie_weights_rake(c(1, 1), df,
                       list(nope = c(a = 1, b = 1))),
    regexp = "not in df"
  )
})

test_that("morie_weights_rake respects bounds clipping", {
  set.seed(1)
  df <- data.frame(s = c("a", "a", "b", "b"))
  res <- morie_weights_rake(c(1, 1, 1, 1), df,
                            list(s = c(a = 100, b = 100)),
                            bounds = c(0.5, 1.5),
                            max_iter = 5)
  expect_true(is.numeric(res$weights))
})

# ---------------------------------------------------------------------------
# morie_weights_greg
# ---------------------------------------------------------------------------

test_that("morie_weights_greg calibrates a 1-col aux to total", {
  set.seed(1)
  X <- matrix(rep(1, 50), ncol = 1)
  w <- rep(1, 50)
  res <- morie_weights_greg(w, X, c(75))
  expect_equal(sum(res$weights * X[, 1]), 75, tolerance = 1e-3)
  expect_true(res$converged)
})

test_that("morie_weights_greg errors on dimension mismatch", {
  X <- matrix(rnorm(10), ncol = 2)
  expect_error(morie_weights_greg(rep(1, 5), X, c(1)),
               regexp = "population_totals length")
})

# ---------------------------------------------------------------------------
# morie_weights_calibrate_to_totals (dispatch)
# ---------------------------------------------------------------------------

test_that("morie_weights_calibrate_to_totals dispatches raking", {
  set.seed(1)
  df <- data.frame(s = sample(c("a", "b"), 20, replace = TRUE))
  res <- morie_weights_calibrate_to_totals(
    rep(1, 20), df, list(s = 100), method = "raking"
  )
  expect_named(res, c("weights", "converged", "iterations",
                      "max_adjustment", "diagnostics"),
               ignore.order = TRUE)
  expect_equal(sum(res$weights), 100, tolerance = 1e-2)
})

# ---------------------------------------------------------------------------
# morie_weights_trim / smooth
# ---------------------------------------------------------------------------

test_that("morie_weights_trim clips at percentiles", {
  w <- c(0.1, 0.5, 1, 1, 1, 1, 1, 5, 10, 100)
  out <- morie_weights_trim(w, lower_percentile = 10,
                            upper_percentile = 90)
  expect_true(min(out) >= quantile(w, 0.1) - 1e-8)
  expect_true(max(out) <= quantile(w, 0.9) + 1e-8)
})

test_that("morie_weights_trim winsorize option works", {
  w <- c(1, 2, 3, 4, 5, 100)
  out <- morie_weights_trim(w, 10, 90, method = "winsorize")
  expect_equal(length(out), length(w))
})

test_that("morie_weights_smooth linear_shrinkage stays bounded", {
  set.seed(1)
  w <- runif(20, 0.5, 5)
  out <- morie_weights_smooth(w, method = "linear_shrinkage",
                              shrinkage_factor = 0.5)
  # variance reduced
  expect_lt(var(out), var(w))
})

test_that("morie_weights_smooth rejects bad shrinkage factor", {
  expect_error(morie_weights_smooth(c(1, 2), shrinkage_factor = 1.5),
               regexp = "shrinkage_factor")
})

test_that("morie_weights_smooth log_transform preserves total", {
  set.seed(1)
  w <- runif(15, 0.5, 5)
  out <- morie_weights_smooth(w, method = "log_transform",
                              shrinkage_factor = 0.3)
  expect_equal(sum(out), sum(w), tolerance = 1e-3)
})

# ---------------------------------------------------------------------------
# morie_weights_nonresponse / propensity / combined
# ---------------------------------------------------------------------------

test_that("morie_weights_nonresponse zeros non-respondents and scales rest", {
  w <- rep(1, 6)
  r <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
  out <- morie_weights_nonresponse(w, r)
  expect_equal(out[!r], rep(0, sum(!r)))
  expect_equal(sum(out), 6, tolerance = 1e-3)  # total preserved
})

test_that("morie_weights_nonresponse warns when no respondents in a cell", {
  w <- rep(1, 4)
  r <- c(FALSE, FALSE, TRUE, TRUE)
  cells <- c(1, 1, 2, 2)
  expect_warning(out <- morie_weights_nonresponse(w, r, cells),
                 regexp = "no respondents")
  expect_equal(out[cells == 1], c(0, 0))
})

test_that("morie_weights_propensity_nonresponse runs and zeros nonresponders", {
  set.seed(1)
  n <- 60
  X <- data.frame(a = rnorm(n), b = rnorm(n))
  r <- sample(0:1, n, replace = TRUE)
  out <- morie_weights_propensity_nonresponse(rep(1, n), r, X)
  expect_equal(out[r == 0], rep(0, sum(r == 0)))
  expect_true(all(out[r == 1] > 0))
})

test_that("morie_weights_combined chains design + nonresponse pipeline", {
  set.seed(1)
  pi <- rep(0.5, 8)
  r  <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  w  <- morie_weights_combined(pi, r)
  expect_equal(w[!r], rep(0, sum(!r)))
  expect_true(all(w[r] > 0))
})

# ---------------------------------------------------------------------------
# morie_weights_normalize
# ---------------------------------------------------------------------------

test_that("morie_weights_normalize sample_size sums to n", {
  w <- c(1, 2, 3, 4)
  out <- morie_weights_normalize(w, target = "sample_size")
  expect_equal(sum(out), length(w), tolerance = 1e-6)
})

test_that("morie_weights_normalize population sums to N", {
  out <- morie_weights_normalize(c(1, 2, 3), target = "population",
                                 population_size = 1000)
  expect_equal(sum(out), 1000, tolerance = 1e-6)
})

test_that("morie_weights_normalize errors without population_size", {
  expect_error(morie_weights_normalize(c(1, 2), target = "population"),
               regexp = "population_size")
})

test_that("morie_weights_normalize warns on zero-sum weights", {
  expect_warning(out <- morie_weights_normalize(c(0, 0, 0)),
                 regexp = "zero")
  expect_equal(out, c(0, 0, 0))
})

# ---------------------------------------------------------------------------
# Diagnostics: ess / deff / detect_extreme / diagnostics
# ---------------------------------------------------------------------------

test_that("morie_weights_ess: equal weights -> ess == n", {
  expect_equal(morie_weights_ess(rep(1, 10)), 10, tolerance = 1e-6)
  expect_equal(morie_weights_ess(rep(5, 7)), 7, tolerance = 1e-6)
})

test_that("morie_weights_ess: skewed weights -> ess < n", {
  expect_lt(morie_weights_ess(c(1, 1, 1, 100)), 4)
})

test_that("morie_weights_ess returns 0 for zero-sum weights", {
  expect_equal(morie_weights_ess(c(0, 0, 0)), 0)
})

test_that("morie_weights_deff: equal weights -> 1; skewed -> >1", {
  expect_equal(morie_weights_deff(rep(1, 5)), 1, tolerance = 1e-6)
  expect_gt(morie_weights_deff(c(1, 1, 1, 100)), 1)
})

test_that("morie_weights_detect_extreme returns IQR-bounded outliers", {
  w <- c(rep(1, 20), 100)
  res <- morie_weights_detect_extreme(w, k = 1.5)
  expect_true(is.list(res))
  expect_gte(res$n_extreme, 1L)
  expect_true(100 %in% res$extreme_values)
})

test_that("morie_weights_diagnostics returns the documented fields", {
  res <- morie_weights_diagnostics(c(1, 2, 3, 4, 5))
  expected <- c("n", "sum_weights", "mean_weight", "median_weight",
                "std_weight", "min_weight", "max_weight", "cv",
                "effective_sample_size", "design_effect",
                "weight_range_ratio", "n_zero", "n_negative",
                "percentiles")
  expect_true(all(expected %in% names(res)))
  expect_equal(res$n, 5)
  expect_equal(res$sum_weights, 15, tolerance = 1e-6)
})

test_that("morie_weights_diagnostics handles empty input", {
  res <- morie_weights_diagnostics(numeric(0))
  expect_equal(res$n, 0)
})

# ---------------------------------------------------------------------------
# Replicate weights
# ---------------------------------------------------------------------------

test_that("morie_weights_jackknife JK1 returns n x n matrix with zeros on diag", {
  set.seed(1)
  w <- rep(1, 5)
  R <- morie_weights_jackknife(w, jk_type = "JK1")
  expect_equal(dim(R), c(5, 5))
  expect_equal(diag(R), rep(0, 5))
  # Each column's total preserves sum(w)
  expect_equal(colSums(R), rep(sum(w), 5), tolerance = 1e-6)
})

test_that("morie_weights_jackknife JKn requires strata", {
  expect_error(morie_weights_jackknife(rep(1, 4), jk_type = "JKn"),
               regexp = "strata")
})

test_that("morie_weights_jackknife JKn returns one column per PSU", {
  # Wolter (2007) JKn drops ONE PSU per replicate and scales the
  # surviving in-stratum PSUs by n_h/(n_h-1). Matrix is n-by-n.
  w <- rep(1, 6)
  s <- c("a", "a", "a", "b", "b", "b")
  R <- morie_weights_jackknife(w, strata = s, jk_type = "JKn")
  expect_equal(dim(R), c(6, 6))
  expect_equal(attr(R, "morie_jkn_strata"), s)
  # Replicate 1 drops PSU 1 (stratum a, n_h=3): 0 on row 1, 1.5 on rows 2-3.
  expect_equal(R[1, 1], 0)
  expect_equal(R[2, 1], 1.5)
  expect_equal(R[4, 1], 1)
})

test_that("morie_weights_brr returns matrix with correct dims", {
  set.seed(1)
  w <- rep(1, 8)
  s <- rep(c("a", "b", "c", "d"), each = 2)
  R <- morie_weights_brr(w, s, n_replicates = 8)
  expect_equal(dim(R), c(8, 8))
})

test_that("morie_weights_fay_brr rejects fay_coefficient >= 1", {
  expect_error(morie_weights_fay_brr(rep(1, 4),
                                      rep(c("a", "b"), each = 2),
                                      fay_coefficient = 1.0),
               regexp = "fay_coefficient")
})

test_that("morie_weights_fay_brr returns matrix shape", {
  set.seed(1)
  w <- rep(1, 8)
  s <- rep(c("a", "b", "c", "d"), each = 2)
  R <- morie_weights_fay_brr(w, s, fay_coefficient = 0.3,
                             n_replicates = 8)
  expect_equal(dim(R), c(8, 8))
})

test_that("morie_weights_bootstrap returns n x B matrix", {
  set.seed(1)
  R <- morie_weights_bootstrap(rep(1, 20), n_replicates = 50, seed = 1)
  expect_equal(dim(R), c(20, 50))
})

test_that("morie_weights_bootstrap with strata returns rescaled weights", {
  set.seed(1)
  s <- rep(c("a", "b"), each = 10)
  R <- morie_weights_bootstrap(rep(1, 20), n_replicates = 30, strata = s)
  expect_equal(dim(R), c(20, 30))
})

test_that("morie_weights_sdr returns n x R matrix", {
  set.seed(1)
  R <- morie_weights_sdr(rep(1, 20), n_replicates = 20)
  expect_equal(dim(R), c(20, 20))
  expect_true(all(R >= 0))
})

# ---------------------------------------------------------------------------
# morie_weights_replicate_variance
# ---------------------------------------------------------------------------

test_that("morie_weights_replicate_variance JK1 matches formula", {
  reps <- c(10, 11, 9, 10.5)
  full <- 10
  diffs_sq <- (reps - full)^2
  R <- length(reps)
  expected_var <- (R - 1) / R * sum(diffs_sq)
  res <- morie_weights_replicate_variance(full, reps, method = "JK1")
  expect_equal(res$variance, expected_var, tolerance = 1e-6)
  expect_equal(res$se, sqrt(expected_var), tolerance = 1e-6)
})

test_that("morie_weights_replicate_variance BRR matches formula", {
  reps <- c(10, 11, 9, 10.5)
  full <- 10
  expected_var <- sum((reps - full)^2) / length(reps)
  res <- morie_weights_replicate_variance(full, reps, method = "BRR")
  expect_equal(res$variance, expected_var, tolerance = 1e-6)
})

test_that("morie_weights_replicate_variance Fay rejects coef >= 1", {
  expect_error(
    morie_weights_replicate_variance(10, c(10, 11), method = "Fay",
                                     fay_coefficient = 1.0),
    regexp = "fay_coefficient"
  )
})

test_that("morie_weights_replicate_variance SDR uses 4/R factor", {
  reps <- c(1, 2, 3, 4)
  full <- 2.5
  expected_var <- 4 / 4 * sum((reps - full)^2)
  res <- morie_weights_replicate_variance(full, reps, method = "SDR")
  expect_equal(res$variance, expected_var, tolerance = 1e-6)
})

test_that("morie_weights_replicate_variance bootstrap == var(reps)", {
  reps <- c(10, 11, 9, 10.5, 10.1)
  res <- morie_weights_replicate_variance(10, reps, method = "bootstrap")
  expect_equal(res$variance, var(reps), tolerance = 1e-6)
})

test_that("morie_weights_replicate_variance handles empty replicates", {
  res <- morie_weights_replicate_variance(5, numeric(0), method = "JK1")
  expect_equal(res$variance, 0)
  expect_equal(res$se, 0)
})

# ---------------------------------------------------------------------------
# morie_weights_multiframe (Hartley)
# ---------------------------------------------------------------------------

test_that("morie_weights_multiframe hartley applies theta to overlap", {
  wa <- c(1, 1, 1)
  wb <- c(2, 2, 2)
  oa <- c(TRUE, FALSE, TRUE)
  ob <- c(FALSE, TRUE, TRUE)
  res <- morie_weights_multiframe(wa, wb, oa, ob, method = "hartley",
                                  theta = 0.5)
  expect_equal(res$weights_a, c(0.5, 1, 0.5), tolerance = 1e-6)
  expect_equal(res$weights_b, c(2, 1, 1), tolerance = 1e-6)
})

test_that("morie_weights_multiframe optional method is stubbed", {
  expect_error(
    morie_weights_multiframe(c(1), c(1), c(TRUE), c(TRUE),
                             method = "optimal"),
    regexp = "NotYetPorted"
  )
})
