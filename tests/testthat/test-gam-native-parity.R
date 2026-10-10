# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity of morie_gam with mgcv::gam (the reference; used only here).
#
# Two kinds of comparison:
# * fixed smoothing parameters (sp = mgcv's estimates): the basis,
#   constraints, penalties, P-IRLS fit, covariance, EDF, criterion and the
#   Wood (2013) p-values are then the same computation and agree to
#   rounding (1e-8 here; observed 1e-12 to 1e-15).  The exception is the
#   Gaussian REML scale, which mgcv takes from its Newton stopping point
#   (observed 1e-9 to 2e-8), so Vp is compared at 1e-6 there.
# * estimated smoothing parameters: morie converges the Newton iteration
#   to |gradient| < 1e-10 * score, mgcv stops at |gradient| < 1e-6 *
#   score.scale (gam.control()$newton$conv.tol; magic's 1e-7 for Gaussian
#   GCV).  The criterion agrees to 1e-7 relative or better, and the
#   smoothing parameters to what mgcv's last gradient implies (up to ~1e-4
#   relative where the criterion is flat); fitted values and EDF agree to
#   1e-4 relative.  A smoothing parameter heading to infinity (a smooth
#   that is really linear) is not compared, and the other tolerances are
#   ten times looser in that case (see the test for the numbers).

gam_parity_data <- function(n = 400, seed = 2) {
  set.seed(seed)
  d <- data.frame(x = stats::runif(n), z = stats::runif(n), w = stats::rnorm(n))
  d$y <- sin(2 * pi * d$x) + 0.5 * d$z^2 + 0.3 * d$w + stats::rnorm(n, 0, 0.3)
  d$yb <- stats::rbinom(n, 1, stats::plogis(2 * sin(2 * pi * d$x) + d$z))
  d$yp <- stats::rpois(n, exp(sin(2 * pi * d$x) + d$z))
  d
}

gam_rel <- function(a, b) max(abs(a - b)) / max(abs(b))

gam_parity_cases <- list(
  list(f = y ~ s(x), fam = "gaussian", method = "GCV.Cp"),
  list(f = y ~ s(x) + s(z) + w, fam = "gaussian", method = "GCV.Cp"),
  list(f = y ~ s(x, bs = "cr") + s(z, bs = "cr") + w, fam = "gaussian", method = "GCV.Cp"),
  list(f = y ~ s(x) + s(z) + w, fam = "gaussian", method = "REML"),
  list(f = yb ~ s(x) + s(z), fam = "binomial", method = "GCV.Cp"),
  list(f = yp ~ s(x) + z, fam = "poisson", method = "GCV.Cp"),
  list(f = yp ~ s(x) + s(z), fam = "poisson", method = "REML"),
  list(f = y ~ s(x, z), fam = "gaussian", method = "GCV.Cp"),
  list(f = y ~ s(x, k = 20) + w, fam = "gaussian", method = "REML")
)

test_that("fixed smoothing parameters reproduce mgcv::gam to rounding", {
  testthat::skip_if_not_installed("mgcv")
  d <- gam_parity_data()
  nd <- data.frame(x = c(0.1, 0.5, 0.93), z = c(0.2, 0.7, 0.5), w = c(0, 1, -1))
  for (cs in gam_parity_cases) {
    b <- mgcv::gam(cs$f, data = d, family = cs$fam, method = cs$method)
    m <- rmorie::morie_gam(cs$f, d, family = cs$fam, method = cs$method, sp = b$sp)
    lab <- paste(format(cs$f), cs$fam, cs$method)
    reml_gauss <- cs$method == "REML" && cs$fam == "gaussian"
    expect_lt(gam_rel(m$coefficients, stats::coef(b)), 1e-8, label = paste("coef", lab))
    expect_equal(names(m$coefficients), names(stats::coef(b)), label = paste("names", lab))
    expect_lt(gam_rel(m$Vp, b$Vp), if (reml_gauss) 1e-6 else 1e-8, label = paste("Vp", lab))
    expect_lt(gam_rel(m$edf, b$edf), 1e-8, label = paste("edf", lab))
    expect_lt(gam_rel(m$fitted.values, b$fitted.values), 1e-8, label = paste("fitted", lab))
    expect_lt(abs(m$score - b$gcv.ubre) / abs(b$gcv.ubre), 1e-8, label = paste("score", lab))
    expect_lt(abs(m$scale - b$sig2) / b$sig2, if (reml_gauss) 1e-6 else 1e-10,
              label = paste("scale", lab))
    sb <- summary(b); sm <- summary(m)
    expect_lt(max(abs(sm$s.table[, 4] - sb$s.table[, 4])), 1e-8, label = paste("smooth p", lab))
    expect_lt(gam_rel(sm$s.table[, 1:3], sb$s.table[, 1:3]), 1e-6, label = paste("s.table", lab))
    expect_lt(abs(sm$r.sq - sb$r.sq), 1e-8, label = paste("r.sq", lab))
    expect_lt(abs(sm$dev.expl - sb$dev.expl), 1e-8, label = paste("dev.expl", lab))
    pm <- stats::predict(m, nd, type = "response", se.fit = TRUE)
    pb <- stats::predict(b, nd, type = "response", se.fit = TRUE)
    expect_lt(gam_rel(pm$fit, as.numeric(pb$fit)), 1e-8, label = paste("predict", lab))
    expect_lt(gam_rel(pm$se.fit, as.numeric(pb$se.fit)), 1e-6, label = paste("predict se", lab))
  }
})

test_that("estimated smoothing parameters agree with mgcv::gam within its tolerance", {
  testthat::skip_if_not_installed("mgcv")
  d <- gam_parity_data()
  for (cs in gam_parity_cases) {
    b <- mgcv::gam(cs$f, data = d, family = cs$fam, method = cs$method)
    m <- rmorie::morie_gam(cs$f, d, family = cs$fam, method = cs$method)
    lab <- paste(format(cs$f), cs$fam, cs$method)
    finite <- b$sp < 1e3
    # with a smoothing parameter on its way to infinity the criterion is
    # flat and mgcv stops early (yp ~ s(x) + s(z), Poisson REML: mgcv's
    # final gradient is 1.5e-4 against its tolerance 1e-6 * 654 = 6.5e-4,
    # sp(z) = 4.8e4 where morie continues to 1.2e8), so the tolerances are
    # ten times looser there
    loose <- if (all(finite)) 1 else 10
    expect_lt(abs(m$score - b$gcv.ubre) / abs(b$gcv.ubre), 1e-7 * loose, label = paste("score", lab))
    # morie's optimum is never worse than mgcv's stopping point
    expect_lte(unname(m$score), unname(b$gcv.ubre) + 1e-12 * abs(b$gcv.ubre))
    expect_lt(gam_rel(sum(m$edf), sum(b$edf)), 1e-4 * loose, label = paste("edf", lab))
    expect_lt(gam_rel(m$fitted.values, b$fitted.values), 1e-4 * loose, label = paste("fitted", lab))
    expect_lt(gam_rel(log(m$sp[finite]), log(b$sp[finite])), 1e-3, label = paste("log sp", lab))
  }
})

test_that("thin plate basis matches mgcv's, eigenvector signs included", {
  testthat::skip_if_not_installed("mgcv")
  d <- gam_parity_data()
  for (f in list(y ~ s(x), y ~ s(x, k = 20), y ~ s(x, z))) {
    b <- mgcv::gam(f, data = d)
    m <- rmorie::morie_gam(f, d, sp = b$sp)
    Xb <- stats::predict(b, d, type = "lpmatrix")
    Xm <- stats::predict(m, d, type = "lpmatrix")
    expect_lt(max(abs(Xm - Xb)), 1e-8)
  }
})

test_that("large data: knot subsampling matches mgcv (n > 2000)", {
  testthat::skip_if_not_installed("mgcv")
  set.seed(3)
  n <- 2600
  d <- data.frame(x = stats::runif(n), z = stats::runif(n))
  d$y <- sin(2 * pi * d$x) + d$z^2 + stats::rnorm(n, 0, 0.3)
  b <- mgcv::gam(y ~ s(x) + s(z), data = d)
  m <- rmorie::morie_gam(y ~ s(x) + s(z), d, sp = b$sp)
  expect_lt(gam_rel(m$coefficients, stats::coef(b)), 1e-8)
  expect_lt(gam_rel(m$fitted.values, b$fitted.values), 1e-8)
})

test_that("native GAM behaves without the reference package", {
  d <- gam_parity_data(200, 7)
  r1 <- {
    set.seed(11); stats::runif(1)
  }
  set.seed(11)
  m <- rmorie::morie_gam(y ~ s(x) + w, d)
  expect_equal(stats::runif(1), r1)                 # caller's RNG stream untouched
  expect_s3_class(m, "morie_gam")
  expect_equal(unname(stats::predict(m, d)), unname(m$linear.predictors), tolerance = 1e-8)
  expect_equal(sum(stats::residuals(m, "response")), sum(d$y - stats::fitted(m)))
  expect_true(m$edf_total > 2 && m$edf_total < 11)
  expect_equal(dim(stats::vcov(m)), c(11L, 11L))
  s <- summary(m)
  expect_true(all(c("edf", "Ref.df", "F", "p-value") %in% colnames(s$s.table)))
  expect_output(print(m), "GCV score")
  expect_output(print(s), "Approximate significance")
  expect_s3_class(stats::logLik(m), "logLik")
  expect_error(rmorie::morie_gam(y ~ s(x, bs = "ps"), d), "not supported")
  expect_error(rmorie::morie_gam(y ~ s(x, by = w), d), "not supported")
  expect_error(rmorie::morie_gam(y ~ s(x):w, d), "interactions")
  expect_error(rmorie::morie_gam(y ~ s(x), d, sp = c(1, 2)), "sp must")
})
