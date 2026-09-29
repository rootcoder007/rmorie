# Coverage for the edgeR building blocks (Robinson, McCarthy & Smyth
# 2010; Robinson & Oshlack 2010; Lund et al. 2012): the NB variance
# mu (1 + mu phi), the TMM factor (trimmed, precision-weighted mean of
# M values, recomputed by rank trimming), effective library sizes,
# log-linear dispersion shrinkage, the conditional exact test (summed
# over dnbinom) and the QL F-test (against stats::pf / stats::pchisq).

test_that("NB variance splits into Poisson and biological parts", {
  v <- edgrn_nb_variance(20, 0.1)
  expect_equal(c(v$variance, v$poisson, v$biological, v$bcv), c(20 * 3, 20, 40, sqrt(0.1)), tolerance = 1e-12)
  expect_identical(edgrn_nb_variance(5, 0)$variance, 5)
  expect_error(edgrn_nb_variance(-1, 0.1), "non-negative")
})

test_that("TMM is the weighted mean of M over genes kept by both trims", {
  set.seed(4)
  r <- stats::rpois(40, 50) + 1
  y <- stats::rpois(40, 80) + 1
  y[1:3] <- 0
  t <- edgrn_tmm_factor(y, r, trim_m = 0.3, trim_a = 0.05)
  Nk <- sum(y)
  Nr <- sum(r)
  ok <- y > 0 & r > 0
  M <- log2(y[ok] / Nk) - log2(r[ok] / Nr)
  A <- 0.5 * (log2(y[ok] / Nk) + log2(r[ok] / Nr))
  w <- 1 / ((Nk - y[ok]) / (Nk * y[ok]) + (Nr - r[ok]) / (Nr * r[ok]))
  n <- sum(ok)
  rm <- rank(M, ties.method = "first")
  ra <- rank(A, ties.method = "first")
  cm <- floor(0.3 * n)
  ca <- floor(0.05 * n)
  keep <- rm > cm & rm <= n - cm & ra > ca & ra <= n - ca
  expect_equal(t$log2_factor, sum(w[keep] * M[keep]) / sum(w[keep]), tolerance = 1e-12)
  expect_equal(t$factor, 2^t$log2_factor, tolerance = 1e-12)
  expect_identical(t$n_used, sum(keep))
  expect_identical(t$n_genes, n)
  expect_identical(morie_edgrn, edgrn_tmm_factor)
  # identical libraries up to depth give factor 1
  expect_equal(edgrn_tmm_factor(2 * r, r)$factor, 1, tolerance = 1e-12)
  expect_error(edgrn_tmm_factor(y, r[-1]), "40 genes in the sample but 39")
  expect_error(edgrn_tmm_factor(c(0, 1), c(1, 0)), "no gene is positive")
  expect_error(edgrn_tmm_factor(y, r, trim_m = 0.5), "\\[0, 0.5\\)")
  expect_error(edgrn_tmm_factor(y, r, lib_sample = 0), "library size is zero")
})

test_that("effective library size and log-linear dispersion shrinkage", {
  e <- edgrn_effective_library_size(1e6, 0.8)
  expect_equal(c(e$effective, e$offset), c(8e5, log(8e5)), tolerance = 1e-12)
  expect_error(edgrn_effective_library_size(0, 1), "must be positive")
  p <- c(0.05, 0.2, 0.4, 0)
  m <- edgrn_moderate_dispersion(p, prior_df = 6, df_residual = 2)
  cv <- exp(mean(log(p[1:3])))
  w <- 6 / 8
  expect_equal(m$common, cv, tolerance = 1e-12)
  expect_equal(m$dispersion, c(exp(0.25 * log(p[1:3]) + w * log(cv)), w * cv), tolerance = 1e-12)
  expect_equal(edgrn_moderate_dispersion(p, common = 0.1, prior_df = 0)$dispersion, p)
  expect_error(edgrn_moderate_dispersion(numeric(0)), "no dispersions")
  expect_error(edgrn_moderate_dispersion(-1), "cannot be negative")
  expect_error(edgrn_moderate_dispersion(1, df_residual = 0), "must be positive")
})

test_that("the exact test sums NB probabilities no larger than the observed", {
  ex <- function(ya, yb, Na, Nb, phi) {
    tot <- ya + yb
    pc <- tot / (Na + Nb)
    lp <- function(s) stats::dnbinom(s, size = 1 / phi, mu = Na * pc, log = TRUE) +
      stats::dnbinom(tot - s, size = 1 / phi, mu = Nb * pc, log = TRUE)
    all <- vapply(0:tot, lp, 1)
    sum(exp(all[all <= lp(ya) + 1e-12])) / sum(exp(all))
  }
  r <- edgrn_exact_test(30, 8, 1e6, 1.2e6, 0.1)
  expect_equal(r$p_value, ex(30, 8, 1e6, 1.2e6, 0.1), tolerance = 1e-10)
  expect_equal(r$logFC, log2((30 / 1e6 + 1e-12) / (8 / 1.2e6 + 1e-12)), tolerance = 1e-12)
  # phi = 0 is the Poisson case: conditionally binomial
  p0 <- edgrn_exact_test(12, 3, 1, 1, 0)$p_value
  pr <- stats::dbinom(0:15, 15, 0.5)
  expect_equal(p0, sum(pr[pr <= stats::dbinom(12, 15, 0.5) * (1 + 1e-9)]), tolerance = 1e-10)
  expect_equal(p0, stats::binom.test(12, 15)$p.value, tolerance = 1e-10)
  expect_error(edgrn_exact_test(-1, 2, 1, 1, 0.1), "non-negative")
})

test_that("the QL F-test uses F = LRT / (q Phi) on (q, df_res + df_prior)", {
  q <- edgrn_ql_f_test(9.3, 2, 1.4, 6, df_prior = 4)
  Fv <- 9.3 / (2 * 1.4)
  expect_equal(q$F, Fv, tolerance = 1e-12)
  expect_identical(q$df2, 10)
  expect_equal(q$p_value, stats::pf(Fv, 2, 10, lower.tail = FALSE), tolerance = 1e-10)
  expect_equal(q$lrt_p_value, stats::pchisq(9.3, 2, lower.tail = FALSE), tolerance = 1e-10)
  big <- edgrn_ql_f_test(40, 3, 0.9, 20)
  expect_equal(big$p_value, stats::pf(40 / 2.7, 3, 20, lower.tail = FALSE), tolerance = 1e-10)
  expect_equal(big$lrt_p_value, stats::pchisq(40, 3, lower.tail = FALSE), tolerance = 1e-10)
  expect_identical(edgrn_ql_f_test(0, 1, 1, 5)$p_value, 1)
  expect_identical(edger, edgrn_ql_f_test)
  expect_identical(edger_diff, edgrn_ql_f_test)
  expect_identical(edgerdiff, edgrn_ql_f_test)
  expect_error(edgrn_ql_f_test(1, 0, 1, 5), "q >= 1")
})
