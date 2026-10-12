# SPDX-License-Identifier: AGPL-3.0-or-later
# The branches of R/effects_native.R the parity tests leave untouched: offsets,
# data recovery, the vcov / by / type options, step contrasts, misnamed variables,
# emmeans specs, and the response-scale labels of a Gamma family.

mk_cov_eff <- function(n = 60L) {
  set.seed(11)
  d <- data.frame(
    x = stats::rnorm(n), g = factor(sample(c("a", "b", "c"), n, TRUE)),
    flag = sample(c(TRUE, FALSE), n, TRUE), bin = sample(c(0, 1), n, TRUE),
    expo = stats::runif(n, 1, 3)
  )
  d$y <- 1 + 0.5 * d$x + (d$g == "b") - 0.5 * d$flag + 0.3 * d$bin + stats::rnorm(n, sd = 0.3)
  d$cnt <- stats::rpois(n, exp(0.2 + 0.3 * d$x + log(d$expo)))
  d$pos <- exp(d$y)
  d
}

test_that("an offset= argument is refused while offset() in the formula is honoured", {
  d <- mk_cov_eff()
  fit_arg <- stats::glm(cnt ~ x, family = stats::poisson(), data = d, offset = log(expo))
  expect_error(morie_effects_predictions(fit_arg), "offset =")
  fit_fml <- stats::glm(cnt ~ x + offset(log(expo)), family = stats::poisson(), data = d)
  p <- morie_effects_predictions(fit_fml, type = "response")
  expect_equal(p$estimate, unname(stats::fitted(fit_fml)), tolerance = 1e-10)
  p_link <- morie_effects_predictions(fit_fml, type = "link")
  expect_equal(p_link$estimate, unname(stats::predict(fit_fml, type = "link")), tolerance = 1e-10)
  expect_identical(attr(p_link, "type"), "link")
})

test_that("the model's data is recovered from the model frame when the call's data is gone", {
  d <- mk_cov_eff()
  fit <- local({
    inner <- d
    stats::lm(y ~ x + g, data = inner)
  })
  # `inner` no longer exists where the formula lives, so the model frame is used
  p <- morie_effects_predictions(fit)
  expect_equal(p$estimate, unname(stats::fitted(fit)))
  expect_true(all(c("x", "g") %in% names(p)))
  # the call's data is an expression, not a name: the model frame itself is used
  fit_expr <- stats::lm(y ~ x + g, data = d[seq_len(nrow(d)), ])
  p_expr <- morie_effects_predictions(fit_expr)
  expect_equal(p_expr$estimate, unname(stats::fitted(fit_expr)))
  # a model whose frame lacks a used variable cannot be recovered
  fit2 <- stats::lm(y ~ x, data = d)
  fit2$model <- fit2$model[, "y", drop = FALSE]
  fit2$call$data <- as.name("no_such_frame_anywhere")
  fit2$data <- NULL
  expect_error(morie_effects_predictions(fit2), "could not recover")
})

test_that("factor levels come from the model, logical and binary predictors contrast on their two values", {
  d <- mk_cov_eff()
  fit <- stats::lm(y ~ x + g + flag + bin, data = d)
  cm <- morie_effects_comparisons(fit, variables = c("g", "flag", "bin"), by = TRUE)
  expect_setequal(cm$contrast, c("b - a", "c - a", "TRUE - FALSE", "1 - 0"))
  cf <- stats::coef(fit)
  expect_equal(cm$estimate[cm$contrast == "b - a"], unname(cf["gb"]))
  expect_equal(cm$estimate[cm$contrast == "TRUE - FALSE"], unname(cf["flagTRUE"]))
  expect_equal(cm$estimate[cm$contrast == "1 - 0"], unname(cf["bin"]))
  # a character predictor and a factor held as character in newdata take the model's levels
  d2 <- d
  d2$g <- as.character(d2$g)
  fit2 <- stats::lm(y ~ x + g, data = d2)
  cm2 <- morie_effects_comparisons(fit2, variables = "g", by = TRUE)
  expect_identical(cm2$contrast, c("b - a", "c - a"))
  # a logical wrapped in factor() contrasts on FALSE/TRUE through the model's own levels
  fit_lf <- stats::lm(y ~ x + factor(flag), data = d)
  cm_lf <- morie_effects_comparisons(fit_lf, variables = "flag", by = TRUE)
  expect_identical(cm_lf$contrast, "TRUE - FALSE")
  expect_equal(cm_lf$estimate, unname(stats::coef(fit_lf)[["factor(flag)TRUE"]]))
  # a fit stripped of its xlevels (as some model wrappers do) falls back to the data's levels
  fit_nx <- stats::lm(y ~ x + g, data = d)
  fit_nx$xlevels <- NULL
  cm_nx <- morie_effects_comparisons(fit_nx, variables = "g", by = TRUE)
  expect_identical(cm_nx$contrast, c("b - a", "c - a"))
  # emmeans on a logical predictor puts both values on the grid
  em <- morie_effects_emmeans(fit, ~flag)
  expect_identical(em$flag, c(FALSE, TRUE))
  expect_equal(diff(em$emmean), unname(cf["flagTRUE"]))
})

test_that("vcov = FALSE drops the standard errors, a matrix vcov is used, anything else errors", {
  d <- mk_cov_eff()
  fit <- stats::lm(y ~ x + g, data = d)
  p0 <- morie_effects_predictions(fit, vcov = FALSE)
  expect_true(all(is.na(p0$std.error)))
  expect_equal(p0$estimate, unname(stats::fitted(fit)))
  V <- 4 * stats::vcov(fit)
  p4 <- morie_effects_predictions(fit, vcov = V)
  p1 <- morie_effects_predictions(fit)
  expect_equal(p4$std.error, 2 * p1$std.error)
  expect_error(morie_effects_predictions(fit, vcov = "HC3"), "covariance matrix")
  # a finite df switches to t-based inference
  pt <- morie_effects_predictions(fit, df = fit$df.residual)
  q_t <- stats::qt(0.975, fit$df.residual)
  expect_equal(pt$conf.high - pt$estimate, q_t * pt$std.error)
  expect_equal(pt$p.value, 2 * stats::pt(-abs(pt$statistic), fit$df.residual))
  expect_error(morie_effects_predictions(fit, by = 3), "`by` must be")
  expect_error(morie_effects_predictions(fit, NULL, 5), "must be named")
  expect_error(morie_effects_predictions(fit, newdata = as.matrix(d[, "x", drop = FALSE])), "data frame")
  expect_error(morie_effects_predictions(fit, by = "nope"), "not found")
})

test_that("type = link on an lm falls back to the response scale", {
  d <- mk_cov_eff()
  fit <- stats::lm(y ~ x, data = d)
  p <- morie_effects_predictions(fit, type = "link")
  expect_identical(attr(p, "type"), "response")
  expect_equal(p$estimate, unname(stats::fitted(fit)))
})

test_that("step contrasts and slopes with an explicit eps or a constant predictor", {
  d <- mk_cov_eff()
  fit <- stats::lm(y ~ x + g, data = d)
  cm <- morie_effects_comparisons(fit, variables = list(x = 0.5), by = TRUE)
  expect_identical(cm$contrast, "+0.5")
  expect_equal(cm$estimate, 0.5 * unname(stats::coef(fit)["x"]))
  expect_error(morie_effects_comparisons(fit, variables = list(x = "big")), "single number")
  expect_error(morie_effects_comparisons(fit, variables = "zzz"), "not in the model")
  sl <- morie_effects_slopes(fit, variables = "x", eps = 1e-3, by = TRUE)
  expect_equal(sl$estimate, unname(stats::coef(fit)["x"]), tolerance = 1e-8)
  # a predictor with no spread uses the fallback step
  d$k <- 1
  fitk <- stats::lm(y ~ x + k, data = d)
  slk <- morie_effects_slopes(fitk, variables = "x", by = TRUE)
  expect_equal(slk$estimate, unname(stats::coef(fitk)["x"]), tolerance = 1e-8)
  d$z <- d$x + 2
  d$z[1:30] <- d$z[1]
  fitz <- stats::lm(y ~ x + z, data = d)
  d$z <- 2
  fit_const <- stats::lm(y ~ x + z, data = d)
  sl_const <- morie_effects_slopes(fit_const, variables = "x", by = TRUE)
  expect_equal(sl_const$estimate, unname(stats::coef(fit_const)["x"]), tolerance = 1e-8)
  # the slope of the constant predictor itself: zero range, fallback step, zero effect
  sl_z <- morie_effects_slopes(fit_const, variables = "z", by = TRUE)
  expect_equal(sl_z$estimate, 0)
})

test_that("emmeans specs: character vector, conditioning bar, at=, bad specs, and a Gamma response scale", {
  d <- mk_cov_eff()
  fit <- stats::lm(y ~ x + g + flag, data = d)
  em_chr <- morie_effects_emmeans(fit, specs = "g")
  em_fml <- morie_effects_emmeans(fit, specs = ~g)
  expect_equal(em_chr$emmean, em_fml$emmean)
  em_by <- morie_effects_emmeans(fit, specs = ~ g | flag)
  expect_equal(nrow(em_by), 6L)
  expect_identical(attr(em_by, "by.vars"), "flag")
  em_at <- morie_effects_emmeans(fit, specs = ~g, at = list(x = 2))
  nd <- data.frame(x = 2, g = factor(c("a", "b", "c"), levels = levels(d$g)), flag = NA)
  pred_at <- sapply(c("a", "b", "c"), function(l) {
    mean(stats::predict(fit, data.frame(x = 2, g = factor(l, levels = levels(d$g)), flag = c(FALSE, TRUE))))
  })
  expect_equal(em_at$emmean, unname(pred_at))
  expect_error(morie_effects_emmeans(fit, specs = 42), "one-sided formula or a character")
  expect_error(morie_effects_emmeans(fit, specs = ~nope), "not in the model")
  expect_error(morie_effects_emmeans(fit, specs = ~g, weights = "cells"), "not supported")
  # a transformed response cannot be back-transformed by the native engine
  fit_log <- stats::lm(log(pos) ~ x + g, data = d)
  expect_error(morie_effects_emmeans(fit_log, specs = ~g, type = "response"), "transformed response")
  # Gamma family: response-scale estimates are labelled "response" and keep t-based df
  fit_gam <- stats::glm(pos ~ x + g, family = stats::Gamma(link = "log"), data = d)
  em_g <- morie_effects_emmeans(fit_gam, specs = ~g, type = "response")
  expect_identical(attr(em_g, "estName"), "response")
  expect_true("response" %in% names(em_g))
  expect_equal(em_g$df, rep(fit_gam$df.residual, 3L))
  expect_true(all(em_g$lower.CL <= em_g$response & em_g$response <= em_g$upper.CL))
  # the emmean of the log-link model at the mean covariate is the cell prediction
  nd2 <- data.frame(x = mean(d$x), g = factor(levels(d$g), levels = levels(d$g)))
  expect_equal(em_g$response, unname(stats::predict(fit_gam, nd2, type = "response")), tolerance = 1e-10)
})
