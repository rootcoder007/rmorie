# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-validation of the native multiple-testing closed forms against
# the reference packages (qvalue, poolr). These packages are NOT used
# at run time; they appear here only as references.

mt_p <- function(seed, m0 = 800, m1 = 200) {
  set.seed(seed)
  c(stats::runif(m0), stats::rbeta(m1, 0.3, 8))
}

test_that("storey_q reproduces qvalue::qvalue defaults", {
  skip_if_not_installed("qvalue")
  # Same arithmetic (identical stats::smooth.spline call, same step-up
  # recursion), so agreement is to floating-point rounding: 1e-10.
  for (seed in 1:5) {
    p <- mt_p(seed)
    ref <- qvalue::qvalue(p)
    nat <- storey_q(p)
    expect_equal(nat$pi0, ref$pi0, tolerance = 1e-10)
    expect_equal(nat$adjusted, as.numeric(ref$qvalues), tolerance = 1e-10)
  }
  # single-lambda cutoff estimator
  p <- mt_p(11)
  for (lam in c(0.3, 0.5, 0.8)) {
    ref <- qvalue::qvalue(p, lambda = lam)
    nat <- storey_q(p, lambda_param = lam)
    expect_equal(nat$pi0, ref$pi0, tolerance = 1e-10)
    expect_equal(nat$adjusted, as.numeric(ref$qvalues), tolerance = 1e-10)
  }
})

test_that("estimate_pi0 reproduces qvalue::pi0est", {
  skip_if_not_installed("qvalue")
  for (seed in 1:5) {
    p <- mt_p(seed)
    expect_equal(estimate_pi0(p, "storey"), qvalue::pi0est(p)$pi0,
                 tolerance = 1e-10)
    expect_equal(estimate_pi0(p, "bootstrap"),
                 qvalue::pi0est(p, pi0.method = "bootstrap")$pi0,
                 tolerance = 1e-10)
  }
})

test_that("storey_q / estimate_pi0 work without qvalue", {
  # no qvalue:: or requireNamespace() call remains in the code path
  expect_false(any(grepl("qvalue::", deparse(storey_q))))
  expect_false(any(grepl("requireNamespace", deparse(estimate_pi0))))
  p <- mt_p(3)
  expect_true(storey_q(p)$pi0 <= 1)
})

test_that("fisher / stouffer / tippett reproduce poolr", {
  skip_if_not_installed("poolr")
  # identical closed forms -> agreement to rounding (1e-12)
  for (seed in 1:5) {
    set.seed(seed)
    p <- c(stats::runif(8), stats::runif(3, 0, 0.02))
    f <- poolr::fisher(p)
    s <- poolr::stouffer(p)
    t <- poolr::tippett(p)
    expect_equal(fisher_combined(p)$statistic, as.numeric(f$statistic), tolerance = 1e-12)
    expect_equal(fisher_combined(p)$p_value, f$p, tolerance = 1e-12)
    expect_equal(stouffer_combined(p)$statistic, as.numeric(s$statistic), tolerance = 1e-12)
    expect_equal(stouffer_combined(p)$p_value, s$p, tolerance = 1e-12)
    expect_equal(tippett_combined(p)$statistic, as.numeric(t$statistic), tolerance = 1e-12)
    expect_equal(tippett_combined(p)$p_value, t$p, tolerance = 1e-12)
  }
})

test_that("n_effective_tests reproduces poolr::meff", {
  skip_if_not_installed("poolr")
  map <- c(galwey = "galwey", li_ji = "liji", nyholt = "nyholt")
  for (seed in 1:5) {
    set.seed(seed)
    Z <- matrix(stats::rnorm(400 * 12), 400, 12)
    X <- Z %*% (diag(12) + matrix(stats::runif(144, 0, 0.4), 12, 12))
    R <- stats::cor(X)
    for (m in names(map)) {
      # both floor() the same eigenvalue formula: exact equality
      expect_equal(n_effective_tests(R, method = m),
                   max(1, poolr::meff(R = R, method = map[[m]])))
    }
  }
})
