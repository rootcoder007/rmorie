# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation of the native marginal-effects engine (R/effects_native.R)
# against emmeans 2.x and marginaleffects 1.x.
#
# Tolerance: every quantity is a closed-form linear / delta-method
# expression evaluated with the same coefficients, vcov and (for slopes)
# the same finite-difference step as the reference packages, so agreement
# is at rounding level (observed <= 1e-12 relative). 1e-6 leaves room for
# BLAS / platform differences without hiding a convention mismatch (a
# wrong grid, step or scale shows up at 1e-3 or worse).

tol <- 1e-6

mk_eff_data <- function(n = 200L) {
  set.seed(1)
  d <- data.frame(
    x = rnorm(n), z = runif(n),
    g = factor(sample(c("a", "b", "c"), n, TRUE)),
    h = factor(sample(c("u", "v"), n, TRUE)),
    k = rbinom(n, 1, 0.5), l = runif(n) > 0.5
  )
  d$y <- 1 + d$x + 0.5 * d$z + (d$g == "b") - 0.5 * d$x * (d$g == "c") + rnorm(n)
  d$yb <- rbinom(n, 1, plogis(0.3 * d$x - 0.4 * (d$g == "b") + d$z))
  d$cnt <- rpois(n, exp(0.2 + 0.3 * d$x))
  d
}

mk_eff_models <- function() {
  d <- mk_eff_data()
  list(
    lm = stats::lm(y ~ x * g + z + h + k, data = d),
    logit = stats::glm(yb ~ x * g + z + h + l, family = stats::binomial(), data = d),
    pois = stats::glm(cnt ~ g * h + x + I(z^2), family = stats::poisson(), data = d)
  )
}

test_that("native emmeans matches emmeans::emmeans (lm, logit, poisson)", {
  skip_if_not_installed("emmeans")
  ms <- mk_eff_models()
  for (nm in names(ms)) {
    m <- ms[[nm]]
    for (sp in list(~g, ~ g | h, ~ g * h, "h")) {
      for (ty in c("link", "response")) {
        a <- morie_effects_emmeans(m, sp, type = ty)
        b <- as.data.frame(suppressMessages(summary(emmeans::emmeans(m, sp, type = ty))))
        expect_identical(names(a), names(b))
        for (col in names(b)[sapply(b, is.numeric)]) {
          expect_equal(a[[col]], b[[col]], tolerance = tol, ignore_attr = TRUE,
                       info = paste(nm, deparse(sp), ty, col))
        }
        for (col in names(b)[sapply(b, is.factor)]) {
          expect_equal(as.character(a[[col]]), as.character(b[[col]]))
        }
      }
    }
    a <- morie_effects_emmeans(m, ~g, at = list(x = 0.5))
    b <- as.data.frame(suppressMessages(summary(emmeans::emmeans(m, ~g, at = list(x = 0.5)))))
    expect_equal(a$SE, b$SE, tolerance = tol)
    expect_equal(a[[2]], b[[2]], tolerance = tol)
  }
})

test_that("native predictions match marginaleffects::predictions", {
  skip_if_not_installed("marginaleffects")
  ms <- mk_eff_models()
  for (nm in names(ms)) {
    m <- ms[[nm]]
    types <- if (nm == "lm") list(NULL, "response") else list(NULL, "response", "link")
    for (ty in types) {
      a <- do.call(morie_effects_predictions, c(list(m), list(type = ty)))
      b <- do.call(marginaleffects::predictions, c(list(m), list(type = ty)))
      expect_equal(a$estimate, b$estimate, tolerance = tol)
      expect_equal(a$conf.low, b$conf.low, tolerance = tol)
      expect_equal(a$conf.high, b$conf.high, tolerance = tol)
      expect_equal(a$p.value, b$p.value, tolerance = tol)
      if (!is.null(b$std.error)) expect_equal(a$std.error, b$std.error, tolerance = tol)
    }
    a <- morie_effects_predictions(m, by = "g")
    b <- marginaleffects::predictions(m, by = "g")
    expect_equal(a$estimate, b$estimate, tolerance = tol)
    expect_equal(a$std.error, b$std.error, tolerance = tol)
    expect_equal(nrow(morie_effects_predictions(m, newdata = mk_eff_data()[1:5, ])), 5L)
  }
})

test_that("native comparisons / slopes match marginaleffects", {
  skip_if_not_installed("marginaleffects")
  ms <- mk_eff_models()
  d <- mk_eff_data()
  chk <- function(a, b) {
    expect_identical(a$term, b$term)
    expect_identical(a$contrast, b$contrast)
    expect_equal(a$estimate, b$estimate, tolerance = tol)
    expect_equal(a$std.error, b$std.error, tolerance = tol)
    expect_equal(a$conf.low, b$conf.low, tolerance = tol)
  }
  for (nm in names(ms)) {
    m <- ms[[nm]]
    chk(morie_effects_comparisons(m), marginaleffects::comparisons(m))
    chk(morie_effects_comparisons(m, by = TRUE), marginaleffects::avg_comparisons(m))
    a <- morie_effects_comparisons(m, by = "h")
    b <- marginaleffects::avg_comparisons(m, by = "h")
    chk(a, b)
    expect_identical(as.character(a$h), as.character(b$h))
    chk(morie_effects_comparisons(m, variables = list(x = 2)),
        marginaleffects::comparisons(m, variables = list(x = 2)))
    chk(morie_effects_slopes(m), marginaleffects::slopes(m))
    chk(morie_effects_slopes(m, by = TRUE), marginaleffects::avg_slopes(m))
    chk(morie_effects_slopes(m, variables = "x", newdata = d[1:5, ]),
        marginaleffects::slopes(m, variables = "x", newdata = d[1:5, ]))
  }
})

test_that("native marginal effects run without the reference packages", {
  d <- mk_eff_data(80L)
  fit <- stats::lm(y ~ x + g, data = d)
  em <- morie_effects_emmeans(fit, ~g)
  expect_s3_class(em, "data.frame")
  expect_identical(names(em), c("g", "emmean", "SE", "df", "lower.CL", "upper.CL"))
  # the emmean of an additive lm at the mean covariate equals the cell prediction
  nd <- data.frame(x = mean(d$x), g = factor(levels(d$g), levels = levels(d$g)))
  expect_equal(em$emmean, unname(stats::predict(fit, nd)))
  p <- morie_effects_predictions(fit)
  expect_equal(p$estimate, unname(stats::fitted(fit)))
  cm <- morie_effects_comparisons(fit, variables = "x", by = TRUE)
  expect_equal(cm$estimate, unname(stats::coef(fit)["x"]))
  expect_equal(cm$std.error, unname(sqrt(stats::vcov(fit)["x", "x"])))
  sl <- morie_effects_slopes(fit, variables = "x")
  expect_equal(sl$estimate, rep(unname(stats::coef(fit)["x"]), nrow(d)), tolerance = 1e-8)
})

test_that("unsupported arguments and model classes error clearly", {
  d <- mk_eff_data(80L)
  fit <- stats::lm(y ~ x + g, data = d)
  expect_error(morie_effects_emmeans(fit, ~g, weights = "proportional"), "not supported")
  expect_error(morie_effects_emmeans(fit, pairwise ~ g), "two-sided")
  expect_error(morie_effects_predictions(fit, hypothesis = "pairwise"), "not supported")
  expect_error(morie_effects_comparisons(fit, comparison = "ratio"), "difference")
  expect_error(morie_effects_slopes(fit, slope = "eyex"), "dydx")
  expect_error(morie_effects_predictions(stats::loess(y ~ x, data = d)), "lm")
})

test_that("a numeric variable the formula wraps in factor() is categorical, as in emmeans and marginaleffects", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")
  rel <- function(a, b) max(abs(a - b)) / max(abs(b))
  m1 <- stats::lm(mpg ~ factor(cyl) + wt, mtcars)
  m2 <- stats::glm(am ~ factor(cyl) + wt, stats::binomial, mtcars)
  m3 <- stats::lm(mpg ~ factor(cyl) * hp + as.factor(gear), mtcars)
  for (m in list(m1, m3)) {
    a <- morie_effects_emmeans(m, specs = ~ cyl)
    b <- suppressMessages(summary(emmeans::emmeans(m, ~ cyl)))
    expect_lt(rel(a$emmean, b$emmean), 1e-10)
    expect_lt(rel(a$SE, b$SE), 1e-10)
  }
  a <- morie_effects_emmeans(m2, specs = ~ cyl, type = "response")
  b <- summary(emmeans::emmeans(m2, ~ cyl, type = "response"))
  expect_lt(rel(a$prob, b$prob), 1e-10)
  for (m in list(m1, m2, m3)) {
    a <- morie_effects_comparisons(m, variables = "cyl")
    b <- suppressWarnings(as.data.frame(marginaleffects::comparisons(m, variables = "cyl")))
    expect_equal(nrow(a), nrow(b))
    expect_lt(rel(a$estimate, b$estimate), 1e-10)
    expect_lt(rel(a$std.error, b$std.error), 1e-10)
  }
})
