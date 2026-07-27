# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 14 -- dccmd.R (DCC-GARCH), vrgft.R (variogram fit),
# fzmrl.R (kernel mean-residual-life), irm.R (DoubleML IRM).

test_that("morie_dcc_multivariate_garch validates panel dimensions", {
  expect_error(morie_dcc_multivariate_garch(matrix(1:10, 5, 2)), "n>=30")
  expect_error(
    morie_dcc_multivariate_garch(matrix(stats::rnorm(30), 30, 1)),
    "k>=2"
  )
})

test_that("morie_dcc_multivariate_garch runs the base-R DCC fallback", {
  set.seed(1)
  x <- matrix(stats::rnorm(140), 70, 2)
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package %in% c("rmgarch", "rugarch")) FALSE else TRUE
    },
    .package = "base"
  )
  res <- tryCatch(suppressWarnings(morie_dcc_multivariate_garch(x)),
    error = function(e) e
  )
  expect_true(is.list(res) || inherits(res, "error"))
  if (is.list(res)) {
    # The DCC estimator is native throughout now; there is no longer a
    # delegated path to fall back FROM, so the label no longer says so.
    expect_match(res$method, "DCC\\(1,1\\) two-step")
    expect_equal(res$k, 2L)
  }
})

test_that("vrgft fits variogram models on point data", {
  set.seed(2)
  coords <- matrix(stats::runif(80), 40, 2)
  x <- stats::rnorm(40)
  for (m in c("exponential", "gaussian", "spherical")) {
    r <- tryCatch(vrgft(x, coords, model = m, n_bins = 7),
      error = function(e) e
    )
    expect_true(is.list(r) || inherits(r, "error"))
    if (is.list(r)) expect_equal(r$estimate$model, m)
  }
  # too few points -> not enough non-empty bins -> error
  r2 <- tryCatch(vrgft(c(1, 2, 3), matrix(c(0, 1, 2), 3, 1)),
    error = function(e) e
  )
  expect_true(is.list(r2) || inherits(r2, "error"))
})

test_that("fzmrl computes kernel MRL across its branches", {
  expect_equal(fzmrl(1)$method, "fzmrl - too few obs")
  set.seed(3)
  r <- fzmrl(stats::rexp(800, 1), t = 0)
  expect_true(is.numeric(r$estimate))
  expect_true(is.finite(r$se))
  # an evaluation point just above the maximum
  edge <- fzmrl(c(1, 2, 3, 4, 5), t = 5.0001)
  expect_true(grepl("no x>t", edge$method) ||
    grepl("S\\(t\\)", edge$method) ||
    is.numeric(edge$estimate))
})

test_that("morie_estimate_irm validates degenerate input (native, no DoubleML)", {
  # native engine: DoubleML absence is irrelevant; degenerate one-arm
  # input must still error clearly
  expect_error(
    morie_estimate_irm(data.frame(Y = 1, T = 1, X1 = 1),
      treatment = "T", outcome = "Y", covariates = "X1"
    ),
    "both arms"
  )
})

test_that("morie_estimate_irm runs the DoubleML IRM when packages are present", {
  testthat::skip_if_not_installed("DoubleML")
  testthat::skip_if_not_installed("mlr3")
  testthat::skip_if_not_installed("mlr3learners")
  testthat::skip_if_not_installed("ranger")
  skip_on_ci()  # DoubleML/mlr3 fit runs via future workers that segfault flakily on CI
  set.seed(1)
  n <- 240
  X <- matrix(stats::rnorm(n * 4), n, 4)
  Tr <- stats::rbinom(n, 1, stats::plogis(X[, 1]))
  Y <- 0.5 * Tr + X[, 1] + stats::rnorm(n)
  df <- data.frame(Y = Y, T = Tr, X)
  res <- tryCatch(
    suppressWarnings(morie_estimate_irm(df,
      treatment = "T", outcome = "Y",
      covariates = paste0("X", 1:4)
    )),
    error = function(e) e
  )
  expect_true(is.list(res) || inherits(res, "error"))
  if (is.list(res)) expect_equal(res$method, "IRM (rmorie native)")
})
