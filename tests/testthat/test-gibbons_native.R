# Cross-language anchors from the morie.fn gb* modules at full
# precision (testthat tolerance is relative).

test_that("runs pmfs match Python and sum to one", {
  out <- morie_runs_pmf(4, 5, r1 = 3, r2 = 3)
  expect_equal(out$joint, 0.285714285714286, tolerance = 1e-12)
  expect_equal(sum(out$marginal_r1), 1, tolerance = 1e-12)
  expect_equal(sum(out$total), 1, tolerance = 1e-12)
  expect_equal(out$mean, 1 + 2 * 20 / 9)
  # infeasible cell is exactly zero
  expect_equal(morie_runs_pmf(4, 5, r1 = 1, r2 = 3)$joint, 0)
  expect_error(morie_runs_pmf(0, 5))
})

test_that("runs up/down enumeration matches the Ch. 3.4 moments", {
  for (n in c(4L, 5L, 6L)) {
    out <- morie_runs_updown(n = n)
    expect_equal(sum(out$pmf), 1, tolerance = 1e-12)
    expect_equal(out$mean, out$mean_formula, tolerance = 1e-12)
    expect_equal(out$var, out$var_formula, tolerance = 1e-12)
  }
  obs <- morie_runs_updown(x = c(1, 2, 3, 4, 5))
  expect_equal(obs$observed, 1L)
  expect_error(morie_runs_updown(x = c(1, 1, 2)))
  expect_error(morie_runs_updown(n = 15))
})

test_that("tolerance coefficient matches Python and extends tolim", {
  out <- morie_tolerance_beta(n = 50, r = 1, s = 50, p = 0.8)
  expect_equal(out$gamma, 0.999807321561485, tolerance = 1e-12)
  expect_equal(out$coverage_dist, c(49, 2))
  req <- morie_tolerance_beta(gamma = 0.95, p = 0.9)$n_required
  expect_gte(morie_tolerance_beta(n = req, p = 0.9)$gamma, 0.95)
  expect_lt(morie_tolerance_beta(n = req - 1L, p = 0.9)$gamma, 0.95)
  # agrees with the Wilks confidence in R/tolim.R at (1, n)
  set.seed(1)
  x <- stats::rnorm(40)
  wilks <- morie_tolerance_limits(x, coverage = 0.9)$confidence_achieved
  expect_equal(morie_tolerance_beta(n = 40, p = 0.9)$gamma, wilks,
    tolerance = 1e-10
  )
  expect_error(morie_tolerance_beta(n = 10, r = 5, s = 3))
})

test_that("K-S limits match Python and internal identities hold", {
  out <- morie_ks_limit(1.36, n = 20, exact_c = 0.25)
  expect_equal(out$L, 0.950514123244622, tolerance = 1e-12)
  expect_equal(out$bt_p_exceed, 0.0688267813646036, tolerance = 1e-12)
  expect_equal(out$p_one_sided, exp(-2 * 1.36^2), tolerance = 1e-12)
  # chi2(2) clothing gives the same tail as exp(-2 n c^2)
  big <- morie_ks_limit(1, n = 400, exact_c = 0.05)
  expect_equal(big$chi2_p, exp(-2 * 400 * 0.05^2), tolerance = 1e-10)
  expect_error(morie_ks_limit(0))
})

test_that("tie-corrected variances shrink and KW matches kruskal.test", {
  set.seed(2)
  x <- round(stats::rnorm(25))
  y <- round(stats::rnorm(28) + 0.4)
  out <- morie_rank_tie_variance(x, y)
  expect_lt(out$var_corrected, out$var_uncorrected)
  expect_equal(out$U, out$W - 25 * 26 / 2)
  g <- list(round(stats::rnorm(15)), round(stats::rnorm(15) + 0.5),
            round(stats::rnorm(15) + 1))
  kw <- morie_rank_tie_variance(groups = g)
  ref <- stats::kruskal.test(g)
  expect_equal(kw$H, unname(ref$statistic), tolerance = 1e-10)
  expect_gte(kw$H, kw$H_uncorrected)
  expect_error(morie_rank_tie_variance(rep(1, 5), rep(1, 5)))
})

test_that("exact rank-correlation nulls reproduce the closed-form variances", {
  for (n in c(4L, 5L, 6L)) {
    kt <- morie_rank_exact_null(n, "kendall")
    expect_equal(kt$mean, 0, tolerance = 1e-12)
    expect_equal(kt$var, kt$var_formula, tolerance = 1e-12)
    sp <- morie_rank_exact_null(n, "spearman")
    expect_equal(sp$var, 1 / (n - 1), tolerance = 1e-12)
  }
  expect_error(morie_rank_exact_null(12))
})

test_that("ARE table matches its own efficacy re-derivation", {
  for (d in c("uniform", "normal", "logistic", "double_exponential")) {
    out <- morie_are_nonparametric(d)
    expect_equal(out$derived$wilcoxon_vs_t, out$wilcoxon_vs_t, tolerance = 1e-6)
    expect_equal(out$derived$sign_vs_t, out$sign_vs_t, tolerance = 1e-6)
    expect_equal(out$derived$sign_vs_wilcoxon, out$sign_vs_wilcoxon,
      tolerance = 1e-6
    )
    expect_gte(out$wilcoxon_vs_t, out$hl_bound_wilcoxon - 1e-9)
  }
  nrm <- morie_are_nonparametric("normal")
  expect_equal(nrm$wilcoxon_vs_t, 3 / pi)
  # the PDF-verified Mood value, not the fabricated 3/pi
  expect_equal(nrm$are_mood_f, 15 / (2 * pi^2))
  expect_gt(abs(nrm$are_mood_f - 3 / pi), 0.01)
})

test_that("order-statistic laws: median CDF, coverage, identity", {
  out <- morie_order_statistic_laws(11L, F_x = 0.5, t = 0.4, r = 3L)
  expect_equal(out$median_cdf, 0.5, tolerance = 1e-12) # symmetric at the median
  expect_equal(out$coverage_mean, 1 / 12)
  expect_equal(out$identity_lhs, out$identity_rhs, tolerance = 1e-12)
  expect_equal(out$edf_var, 0.25 / 11)
  expect_null(morie_order_statistic_laws(10L, F_x = 0.5)$median_cdf) # even n
  expect_error(morie_order_statistic_laws(0L))
})
