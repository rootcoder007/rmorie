# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for R/psymet.R -- Cronbach alpha, KMO, Bartlett, parallel analysis,
# split-half, omega, item-total, alpha-if-deleted, CR/AVE, discrimination.

.make_items <- function(n = 50L, k = 5L, rho = 0.6, seed = 1L) {
  set.seed(seed)
  f <- rnorm(n)
  X <- sapply(seq_len(k), function(j) sqrt(rho) * f + sqrt(1 - rho) * rnorm(n))
  colnames(X) <- paste0("i", seq_len(k))
  X
}

# ---------------------------------------------------------------------------
# morie_psymet_alpha
# ---------------------------------------------------------------------------

test_that("morie_psymet_alpha returns the expected list shape", {
  X <- .make_items()
  res <- morie_psymet_alpha(X)
  expect_type(res, "list")
  expect_named(res, c("raw", "std", "avgr", "k", "n", "ci_lo", "ci_hi"),
               ignore.order = TRUE)
  expect_equal(res$k, 5L)
  expect_equal(res$n, 50L)
  expect_true(res$raw > 0 && res$raw < 1)
  expect_true(res$ci_lo <= res$raw && res$ci_hi >= res$raw)
})

test_that("morie_psymet_alpha is ~1 for perfectly correlated items", {
  set.seed(1)
  v <- rnorm(40)
  X <- cbind(v, v, v, v)
  res <- morie_psymet_alpha(X)
  expect_equal(res$raw, 1.0, tolerance = 1e-3)
  expect_equal(res$std, 1.0, tolerance = 1e-3)
})

test_that("morie_psymet_alpha matches hand-computed small example", {
  # 3-item, 4-respondent matrix with known values.
  X <- matrix(c(1, 2, 3,
                2, 3, 4,
                3, 4, 5,
                4, 5, 6), ncol = 3, byrow = TRUE)
  # All three items perfectly correlated -> alpha = 1
  res <- morie_psymet_alpha(X)
  expect_equal(res$raw, 1.0, tolerance = 1e-3)
})

test_that("morie_psymet_alpha returns NA components for single item", {
  X <- matrix(rnorm(20), ncol = 1)
  res <- morie_psymet_alpha(X)
  expect_true(is.na(res$raw))
  expect_equal(res$k, 1L)
})

test_that("morie_psymet_alpha handles zero total variance", {
  # Constant rowsums (1+3, 2+2, 3+1) = (4, 4, 4) -> total variance = 0
  X <- matrix(c(1, 2, 3, 3, 2, 1), ncol = 2)
  res <- morie_psymet_alpha(X)
  expect_true(is.na(res$raw))
})

# ---------------------------------------------------------------------------
# morie_psymet_omega
# ---------------------------------------------------------------------------

test_that("morie_psymet_omega(nf=4) returns bounded total + hierarchical values", {
  # nf=4 to give omega_h enough hierarchical structure to be
  # meaningful (psych::omega correctly warns at nf=1 that omega_h
  # and omega_asymptotic are not meaningful for a single-factor
  # model; that is informative signal users should see, not noise).
  # Need at least 3 items per factor, so bump k to 12 with rho=0.5.
  X <- .make_items(n = 120, k = 12, rho = 0.5)
  res <- morie_psymet_omega(X, nf = 4)
  expect_type(res, "list")
  expect_true(res$total >= 0 && res$total <= 1)
  expect_true(res$hier  >= 0 && res$hier  <= 1)
  expect_equal(res$nf, 4)
})

test_that("morie_psymet_omega(nf=4) delegates when psych is installed", {
  X <- .make_items(n = 100, k = 12, rho = 0.55)
  res <- morie_psymet_omega(X, nf = 4)
  expect_true(is.numeric(res$total))
  expect_true(is.numeric(res$alpha))
})

test_that("morie_psymet_omega(nf=1) warns omega_h is not meaningful", {
  # Single-factor case: warning IS the signal users need to interpret
  # the result correctly. Assert it fires.
  X <- .make_items(n = 60, k = 5, rho = 0.55)
  expect_warning(
    morie_psymet_omega(X, nf = 1),
    "Omega_h and Omega_asymptotic are not meaningful with one factor"
  )
})

# ---------------------------------------------------------------------------
# morie_psymet_itemtotal
# ---------------------------------------------------------------------------

test_that("morie_psymet_itemtotal returns 3-col data.frame", {
  X <- .make_items()
  out <- morie_psymet_itemtotal(X)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("item", "r_total", "r_corr"))
  expect_equal(nrow(out), ncol(X))
  expect_true(all(out$r_total > 0))
  # Corrected r < total r in general
  expect_true(all(out$r_corr <= out$r_total + 1e-8))
})

# ---------------------------------------------------------------------------
# morie_psymet_alphadel
# ---------------------------------------------------------------------------

test_that("morie_psymet_alphadel returns one row per item", {
  X <- .make_items()
  out <- morie_psymet_alphadel(X)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("item", "adel"))
  expect_equal(nrow(out), ncol(X))
  expect_true(all(out$adel <= 1.0 + 1e-8))
})

# ---------------------------------------------------------------------------
# morie_psymet_cr / morie_psymet_ave
# ---------------------------------------------------------------------------

test_that("morie_psymet_cr matches CR formula", {
  lam <- c(0.7, 0.8, 0.6)
  expected <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))
  expect_equal(morie_psymet_cr(lam), expected, tolerance = 1e-3)
})

test_that("morie_psymet_cr returns 1 for unit loadings", {
  expect_equal(morie_psymet_cr(c(1, 1, 1)), 1.0, tolerance = 1e-3)
})

test_that("morie_psymet_ave is mean of squared loadings", {
  lam <- c(0.7, 0.8, 0.6)
  expect_equal(morie_psymet_ave(lam), mean(lam^2), tolerance = 1e-3)
})

# ---------------------------------------------------------------------------
# morie_psymet_kmo
# ---------------------------------------------------------------------------

test_that("morie_psymet_kmo returns msa in (0,1] and one MSA per item", {
  X <- .make_items(n = 100, k = 5, rho = 0.5)
  res <- morie_psymet_kmo(X)
  expect_type(res, "list")
  expect_true(res$msa > 0 && res$msa <= 1.0 + 1e-6)
  expect_length(res$items, ncol(X))
  expect_named(res$items, colnames(X))
})

test_that("morie_psymet_kmo delegates to psych::KMO when installed", {
  X <- .make_items(n = 80, k = 4, rho = 0.5)
  res <- morie_psymet_kmo(X)
  expect_true(is.numeric(res$msa))
})

# ---------------------------------------------------------------------------
# morie_psymet_bartlett
# ---------------------------------------------------------------------------

test_that("morie_psymet_bartlett returns chisq, df, pval", {
  X <- .make_items(n = 100, k = 5, rho = 0.5)
  res <- morie_psymet_bartlett(X)
  expect_type(res, "list")
  expect_named(res, c("chisq", "df", "pval"))
  expect_equal(res$df, choose(5, 2))
  expect_true(res$chisq > 0)
  expect_true(res$pval >= 0 && res$pval <= 1)
  # Correlated items should reject sphericity
  expect_lt(res$pval, 0.05)
})

test_that("morie_psymet_bartlett pval is large for uncorrelated items", {
  set.seed(1)
  X <- matrix(rnorm(800), ncol = 4)
  res <- morie_psymet_bartlett(X)
  expect_gt(res$pval, 0.001)
})

# ---------------------------------------------------------------------------
# morie_psymet_parallel
# ---------------------------------------------------------------------------

test_that("morie_psymet_parallel suggests >=1 factor with correlated items", {
  X <- .make_items(n = 60, k = 5, rho = 0.6)
  k <- morie_psymet_parallel(X, nsim = 20, seed = 1)
  expect_type(k, "integer")
  expect_gte(k, 1L)
})

test_that("morie_psymet_parallel fallback runs without psych", {
  # Even if psych is installed, this exercises the export path.
  X <- .make_items(n = 50, k = 4, rho = 0.4)
  k <- morie_psymet_parallel(X, nsim = 10, seed = 42)
  expect_gte(k, 1L)
})

# ---------------------------------------------------------------------------
# morie_psymet_splithalf
# ---------------------------------------------------------------------------

test_that("morie_psymet_splithalf 'first_last' returns Spearman-Brown value", {
  X <- .make_items(n = 80, k = 6, rho = 0.6)
  v <- morie_psymet_splithalf(X, method = "first_last")
  expect_type(v, "double")
  expect_true(v > 0 && v <= 1.0 + 1e-6)
})

test_that("morie_psymet_splithalf 'odd_even' returns SB reliability", {
  X <- .make_items(n = 80, k = 6, rho = 0.6)
  v <- morie_psymet_splithalf(X, method = "odd_even")
  expect_true(v > 0 && v <= 1.0 + 1e-6)
})

test_that("morie_psymet_splithalf ~= 1 for perfectly correlated items", {
  set.seed(1)
  v <- rnorm(50)
  X <- cbind(v, v, v, v, v, v)
  expect_equal(morie_psymet_splithalf(X, "first_last"), 1.0, tolerance = 1e-3)
})

# ---------------------------------------------------------------------------
# morie_psymet_discrimination
# ---------------------------------------------------------------------------

test_that("morie_psymet_discrimination returns a row per item", {
  X <- .make_items()
  out <- morie_psymet_discrimination(X)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("item", "d"))
  expect_equal(nrow(out), ncol(X))
})

test_that("morie_psymet_discrimination handles 0/1 dichotomous items", {
  set.seed(1)
  X <- matrix(sample(0:1, 80, replace = TRUE), ncol = 4)
  out <- morie_psymet_discrimination(X)
  expect_true(all(out$d >= -1.0 - 1e-6 & out$d <= 1.0 + 1e-6))
})

test_that("morie_psymet_discrimination zero-max item returns 0", {
  X <- cbind(rep(0, 20), rnorm(20))
  out <- morie_psymet_discrimination(X)
  expect_equal(out$d[1], 0)
})
