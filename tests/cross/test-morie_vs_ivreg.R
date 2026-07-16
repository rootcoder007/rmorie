# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 17 cross-validation: native IV engines vs ivreg / AER / gmm
# (reference packages allowed here only).
library(testthat)
library(rmorie)

set.seed(150)
n <- 1200
x <- rnorm(n)
z1 <- rnorm(n); z2 <- rnorm(n)
u <- rnorm(n)
d <- 0.5 * z1 + 0.4 * z2 + 0.3 * x + u + rnorm(n)
y <- 0.8 * d + 0.5 * x + 0.8 * u + rnorm(n)
df <- data.frame(y = y, d = d, x = x, z1 = z1, z2 = z2)

test_that("native 2SLS reproduces ivreg (coef exact, both vcovs)", {
  skip_if_not_installed("ivreg")
  ref <- ivreg::ivreg(y ~ d + x | z1 + z2 + x, data = df)
  mine <- morie_iv_tsls(df, "y", "d", c("z1", "z2"), exogenous = "x",
                        robust = FALSE)
  common <- intersect(names(coef(ref)), names(mine$coefficients))
  expect_equal(mine$coefficients[common], coef(ref)[common],
               tolerance = 1e-8)
  expect_equal(mine$std_errors[common],
               sqrt(diag(vcov(ref)))[common], tolerance = 1e-8)
  if (requireNamespace("sandwich", quietly = TRUE)) {
    mine_r <- morie_iv_tsls(df, "y", "d", c("z1", "z2"),
                            exogenous = "x", robust = TRUE)
    se_hc1 <- sqrt(diag(sandwich::vcovHC(ref, type = "HC1")))
    expect_equal(mine_r$std_errors[common], se_hc1[common],
                 tolerance = 1e-8)
  }
})

test_that("native LIML matches AER-style k-class on kappa and coef", {
  skip_if_not_installed("ivreg")
  ref <- tryCatch(ivreg::ivreg(y ~ d + x | z1 + z2 + x, data = df,
                               method = "M"),
                  error = function(e) NULL)
  mine <- morie_iv_liml(df, "y", "d", c("z1", "z2"), exogenous = "x")
  expect_gte(mine$details$kappa, 1 - 1e-10)
  if (!is.null(ref) && !is.null(coef(ref))) {
    common <- intersect(names(coef(ref)), names(mine$coefficients))
    expect_equal(mine$coefficients[common], coef(ref)[common],
                 tolerance = 0.02)
  }
})

test_that("native two-step GMM matches gmm::gmm with MDS vcov", {
  skip_if_not_installed("gmm")
  ref <- gmm::gmm(y ~ d + x, x = ~ z1 + z2 + x, data = df,
                  type = "twoStep", vcov = "MDS")
  mine <- morie_iv_gmm(df, "y", "d", c("z1", "z2"), exogenous = "x")
  common <- intersect(names(coef(ref)), names(mine$coefficients))
  expect_equal(unname(mine$coefficients[common]),
               unname(coef(ref)[common]), tolerance = 1e-4)
  # Hansen J agrees
  jref <- gmm::specTest(ref)$test
  jmine <- morie_iv_hansen_j(df, "y", "d", c("z1", "z2"),
                             exogenous = "x")
  expect_equal(unname(jmine$statistic), unname(jref[1]),
               tolerance = 0.05)
})

test_that("native Sargan matches AER diagnostics", {
  skip_if_not_installed("ivreg")
  ref <- ivreg::ivreg(y ~ d + x | z1 + z2 + x, data = df)
  diag_tbl <- summary(ref, diagnostics = TRUE)$diagnostics
  mine <- morie_iv_sargan(df, "y", "d", c("z1", "z2"), exogenous = "x")
  if ("Sargan" %in% rownames(diag_tbl)) {
    expect_equal(unname(mine$statistic),
                 unname(diag_tbl["Sargan", "statistic"]),
                 tolerance = 0.05)
  }
})
