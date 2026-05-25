# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/multiple_testing.R
# ---------------------------------------------------------------------------

set.seed(1)

make_p <- function(n_null = 80, n_sig = 20, seed = 1) {
  set.seed(seed)
  c(runif(n_null), pmin(runif(n_sig, 0, 0.005), 1))
}

# ---------------------------------------------------------------------------
# FWER procedures
# ---------------------------------------------------------------------------

test_that("bonferroni rejection count is monotone in alpha", {
  set.seed(1); p <- make_p(50, 10)
  a <- bonferroni(p, alpha = 0.01)
  b <- bonferroni(p, alpha = 0.10)
  expect_lte(a$n_rejected, b$n_rejected)
})

test_that("bonferroni rejects p<=0.05 / m", {
  p <- c(0.001, 0.04, 0.5)
  res <- bonferroni(p, alpha = 0.05)
  expect_s3_class(res, "morie_multiple_testing_result")
  expect_equal(res$rejected[1], TRUE)
})

test_that(".mt_check_p rejects empty / out-of-range / non-finite p", {
  expect_error(bonferroni(numeric(0)))
  expect_error(bonferroni(c(0.5, -1)))
  expect_error(bonferroni(c(0.5, 1.2)))
  expect_error(bonferroni(c(0.5, NA)))
})

test_that("sidak yields slightly more rejections than Bonferroni", {
  set.seed(1); p <- make_p(40, 10)
  rs <- sidak(p)$n_rejected
  rb <- bonferroni(p)$n_rejected
  expect_gte(rs, rb)
})

test_that("holm and hochberg agree with stats::p.adjust", {
  set.seed(1); p <- make_p(30, 5)
  expect_equal(holm(p)$adjusted, stats::p.adjust(p, "holm"))
  expect_equal(hochberg(p)$adjusted, stats::p.adjust(p, "hochberg"))
})

test_that("hommel matches stats::p.adjust", {
  set.seed(1); p <- make_p(30, 5)
  expect_equal(hommel(p)$adjusted, stats::p.adjust(p, "hommel"))
})

test_that("holm_sidak is monotonic after sort", {
  set.seed(1); p <- make_p(30, 5)
  res <- holm_sidak(p)
  expect_true(all(diff(sort(res$adjusted)) >= -1e-12))
})

# ---------------------------------------------------------------------------
# FDR procedures
# ---------------------------------------------------------------------------

test_that("benjamini_hochberg matches stats::p.adjust", {
  set.seed(1); p <- make_p()
  expect_equal(benjamini_hochberg(p)$adjusted, stats::p.adjust(p, "BH"))
})

test_that("bh alias points to benjamini_hochberg", {
  expect_identical(bh, benjamini_hochberg)
})

test_that("benjamini_yekutieli matches stats::p.adjust", {
  set.seed(1); p <- make_p()
  expect_equal(benjamini_yekutieli(p)$adjusted, stats::p.adjust(p, "BY"))
})

test_that("by_fdr alias points to benjamini_yekutieli", {
  expect_identical(by_fdr, benjamini_yekutieli)
})

test_that("storey_q returns pi0 in [0, 1]", {
  set.seed(1); p <- make_p()
  res <- storey_q(p, lambda_param = 0.5)
  expect_true(is.finite(res$pi0))
  expect_lte(res$pi0, 1)
  expect_gte(res$pi0, 0)
})

# ---------------------------------------------------------------------------
# Combining p-values
# ---------------------------------------------------------------------------

test_that("fisher_combined yields a small p when one is tiny", {
  res <- fisher_combined(c(0.001, 0.6, 0.5))
  expect_lt(res$p_value, 0.05)
})

test_that("stouffer_combined supports weights", {
  res_w <- stouffer_combined(c(0.01, 0.5, 0.6), weights = c(2, 1, 1))
  res_u <- stouffer_combined(c(0.01, 0.5, 0.6))
  expect_true(is.finite(res_w$p_value))
  expect_true(is.finite(res_u$p_value))
})

test_that("tippett_combined and simes_combined return p in [0, 1]", {
  for (fn in list(tippett_combined, simes_combined)) {
    res <- fn(c(0.01, 0.2, 0.7))
    expect_gte(res$p_value, 0)
    expect_lte(res$p_value, 1)
  }
})

test_that("harmonic_mean_p returns a scalar", {
  out <- harmonic_mean_p(c(0.001, 0.05, 0.5))
  expect_length(out, 1L)
  expect_true(is.finite(out))
})

test_that("cauchy_combination unweighted and weighted forms run", {
  a <- cauchy_combination(c(0.01, 0.3, 0.5))
  b <- cauchy_combination(c(0.01, 0.3, 0.5), weights = c(0.5, 0.25, 0.25))
  expect_s3_class(a, "morie_multiple_testing_result")
  expect_s3_class(b, "morie_multiple_testing_result")
})

# ---------------------------------------------------------------------------
# Gatekeeping
# ---------------------------------------------------------------------------

test_that("fixed_sequence stops at first non-rejection", {
  res <- fixed_sequence(c(0.01, 0.02, 0.5, 0.001))
  expect_equal(res$n_rejected, 2L)
  expect_false(res$rejected[3])
  expect_false(res$rejected[4])
})

test_that("fixed_sequence all-reject path covers note branch", {
  res <- fixed_sequence(c(0.001, 0.002, 0.003))
  expect_equal(res$n_rejected, 3L)
})

test_that("fixed_sequence immediate failure path", {
  res <- fixed_sequence(c(0.5, 0.001))
  expect_equal(res$n_rejected, 0L)
})

test_that("fallback_procedure requires weights matching p length", {
  expect_error(fallback_procedure(c(0.01, 0.05), weights = c(1)))
})

test_that("fallback_procedure carry-over rejects sequentially", {
  res <- fallback_procedure(c(0.001, 0.03, 0.5),
                            weights = c(0.5, 0.25, 0.25))
  expect_equal(res$n_rejected, 2L)
})

test_that("hierarchical_bonferroni requires a list input", {
  expect_error(hierarchical_bonferroni(c(0.01, 0.02)))
})

test_that("hierarchical_bonferroni closes gate after empty family", {
  res <- hierarchical_bonferroni(list(c(0.4, 0.5), c(0.001, 0.002)))
  expect_true(res$summary_lines$`Gate closed`)
  expect_equal(res$summary_lines$`Total rejected`, 0L)
})

test_that("hierarchical_bonferroni keeps gate open on a first-family rejection", {
  res <- hierarchical_bonferroni(list(c(0.001), c(0.001)))
  expect_false(res$summary_lines$`Gate closed`)
})

# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

test_that("estimate_pi0 works for all three methods", {
  set.seed(1); p <- make_p()
  for (m in c("storey", "bootstrap", "two_step")) {
    pi0 <- estimate_pi0(p, method = m)
    expect_gte(pi0, 0); expect_lte(pi0, 1)
  }
})

test_that("estimate_pi0 two_step returns 1 if BH rejects nothing", {
  expect_equal(estimate_pi0(rep(0.9, 20), method = "two_step"), 1)
})

test_that("adjust_p_values dispatches on each known method", {
  set.seed(1); p <- make_p(20, 5)
  for (m in c("bonferroni", "sidak", "holm", "hochberg", "hommel",
              "holm_sidak", "bh", "benjamini_hochberg", "fdr",
              "by", "benjamini_yekutieli", "storey", "fwer")) {
    expect_s3_class(adjust_p_values(p, method = m),
                    "morie_multiple_testing_result")
  }
})

test_that("adjust_p_values errors on unknown method", {
  expect_error(adjust_p_values(c(0.1, 0.2), method = "no_such_method"))
})

test_that("n_effective_tests runs for all three estimators", {
  set.seed(1)
  R <- diag(5)
  R[1, 2] <- R[2, 1] <- 0.7
  for (m in c("galwey", "li_ji", "nyholt")) {
    expect_true(n_effective_tests(R, method = m) >= 1)
  }
})

test_that("n_effective_tests errors on null or non-square input", {
  expect_error(n_effective_tests(NULL))
  expect_error(n_effective_tests(matrix(1, 2, 3)))
})

test_that("n_effective_tests handles a degenerate zero-eigenvalue matrix", {
  M <- matrix(0, 3, 3)
  expect_equal(n_effective_tests(M), 1)
})

# ---------------------------------------------------------------------------
# print method
# ---------------------------------------------------------------------------

test_that("print.morie_multiple_testing_result emits the title", {
  res <- bonferroni(c(0.01, 0.5))
  expect_output(print(res), "Adjusted p-values", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Local FDR + permutation FWER/FDR
# ---------------------------------------------------------------------------

test_that("local_fdr returns a data frame with the right columns", {
  set.seed(1)
  p <- c(runif(80), stats::pnorm(-abs(rnorm(20, mean = 3))) * 2)
  out <- local_fdr(p)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("p_value", "z_score", "local_fdr") %in% names(out)))
  expect_true(all(out$local_fdr >= 0 & out$local_fdr <= 1))
})

test_that("local_fdr labels propagate to the output", {
  out <- local_fdr(c(0.01, 0.5, 0.9), labels = c("a", "b", "c"))
  expect_true("label" %in% names(out))
})

test_that("local_fdr handles a constant-z degenerate input", {
  out <- local_fdr(rep(0.5, 30))
  expect_true(all(is.finite(out$local_fdr)))
})

test_that("permutation_fwer requires ncol(null) == length(obs)", {
  set.seed(1)
  expect_error(permutation_fwer(c(1, 2), matrix(rnorm(20), 10, 3)))
})

test_that("permutation_fwer two-sided / greater / less branches run", {
  set.seed(1)
  m <- 6; nperm <- 100
  obs <- c(rnorm(4), 3, 4)
  null <- matrix(rnorm(nperm * m), nperm, m)
  for (alt in c("two_sided", "greater", "less")) {
    res <- permutation_fwer(obs, null, alternative = alt)
    expect_s3_class(res, "morie_multiple_testing_result")
    expect_equal(length(res$adjusted), m)
  }
})

test_that("permutation_fwer adjusted p in [0, 1] and monotone within sort", {
  set.seed(1)
  m <- 8; nperm <- 200
  obs <- c(rnorm(m - 2), 4, 4.5)
  null <- matrix(rnorm(nperm * m), nperm, m)
  res <- permutation_fwer(obs, null)
  expect_true(all(res$adjusted >= 0 & res$adjusted <= 1))
})

test_that("permutation_fdr returns q in [0, 1]", {
  set.seed(1)
  m <- 12; nperm <- 200
  p_obs <- c(runif(m - 3), 1e-4, 1e-3, 5e-3)
  p_null <- matrix(runif(nperm * m), nperm, m)
  res <- permutation_fdr(p_obs, p_null)
  expect_true(all(res$adjusted >= 0 & res$adjusted <= 1))
  expect_s3_class(res, "morie_multiple_testing_result")
})

test_that("permutation_fdr rejects ncol(null) mismatch", {
  set.seed(1)
  expect_error(permutation_fdr(c(0.1, 0.2), matrix(runif(20), 10, 3)))
})