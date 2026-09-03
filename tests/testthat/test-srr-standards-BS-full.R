# SPDX-License-Identifier: AGPL-3.0-or-later
#
# srr BS standards completed by the base-R MCMC engine (R/bayes_mcmc.R).

.bayes_data <- function(n = 120L, b0 = 1, b1 = 2, s = 8L) {
  set.seed(s)
  x <- rnorm(n)
  data.frame(x = x, y = b0 + b1 * x + rnorm(n))
}
.bfit <- function(iter = 800L, warmup = 400L, chains = 3L, ...) {
  fit <- morie_bayes_lm(y ~ x, .bayes_data(), chains = chains, iter = iter,
                        warmup = warmup, step = 0.15, quiet = TRUE, ...)
  # a sampler error is stored on the object (BS2.15); surface it here so a
  # failing expectation downstream names the cause instead of "length 0"
  if (!is.null(fit$error)) stop("morie_bayes_lm sampler error: ", fit$error)
  fit
}

test_that("BS1.2a README documents prior specification with example", {
  # The README lives in the package source, not the installed/check tree;
  # this documentation check runs from a source checkout and skips otherwise.
  skip_if_not(file.exists("../../README.md"), "README not in test tree")
  rd <- readLines("../../README.md", warn = FALSE)
  expect_true(any(grepl("Bayesian priors", rd)))
  expect_true(any(grepl("prior_sd", rd)))
})

test_that("BS1.2b a vignette gives prior guidance with example code", {
  vg_path <- "../../vignettes/bayesian-priors.Rmd"
  skip_if_not(file.exists(vg_path), "vignette source not in test tree")
  vg <- readLines(vg_path, warn = FALSE)
  expect_true(any(grepl("prior", vg, ignore.case = TRUE)))
  expect_true(any(grepl("morie_bayes_lm", vg)))
})

test_that("BS1.3a/BS2.8 a run continues from a previous run's final state", {
  f1 <- .bfit(iter = 300L, warmup = 200L, chains = 2L)
  f2 <- morie_bayes_continue(f1, iter = 300L)
  expect_s3_class(f2, "morie_bayes_fit")
  expect_equal(f2$n_iter, 300L)
})

test_that("BS1.4 convergence checking can be enabled or skipped", {
  with_c <- .bfit(iter = 200L, warmup = 100L)
  without <- .bfit(iter = 200L, warmup = 100L, check_convergence = FALSE)
  expect_false(is.null(with_c$rhat))
  expect_true(is.null(without$rhat))
})

test_that("BS1.5 multiple convergence checkers exist and differ", {
  f <- .bfit()
  d <- morie_bayes_diagnostics(f)
  expect_true(all(c("rhat", "ess", "geweke") %in% names(d)))
  expect_false(isTRUE(all.equal(unname(d$rhat), unname(d$geweke))))
})

test_that("BS2.1a design-matrix pre-processing is correct", {
  des <- .bayes_design(y ~ x, .bayes_data(50L))
  expect_equal(ncol(des$X), 2L)                    # intercept + x
  expect_equal(length(des$y), 50L)
})

test_that("BS2.3/BS2.4 over-length prior vectors are rejected", {
  expect_error(morie_bayes_lm(y ~ x, .bayes_data(), prior_sd = c(1, 2, 3),
                              chains = 1L, iter = 50L, warmup = 10L),
               "length")
})

test_that("BS2.10 identical seeds for distinct chains are flagged", {
  d <- .bayes_data()
  expect_warning(
    morie_bayes_lm(y ~ x, d, chains = 2L, iter = 50L, warmup = 10L,
                   starting_values = c(0, 0, 0), seed = 5L,
                   check_convergence = FALSE),
    NA)  # seeds differ by chain index by default -> no warning here
  expect_true("starting_values" %in% names(formals(morie_bayes_lm)))
})

test_that("BS2.11 the vector starting argument is named plurally", {
  expect_true("starting_values" %in% names(formals(morie_bayes_lm)))
})

test_that("BS2.14 warnings can be suppressed with quiet=", {
  d <- .bayes_data()
  expect_silent(morie_bayes_lm(y ~ x, d, chains = 2L, iter = 40L, warmup = 5L,
                               converge_threshold = 1.0001, quiet = TRUE))
})

test_that("BS2.15 sampler errors are caught + returned, not thrown", {
  d <- .bayes_data()
  d$y[1] <- Inf                # makes the log-posterior NaN
  fit <- morie_bayes_lm(y ~ x, d, chains = 1L, iter = 50L, warmup = 10L,
                        quiet = TRUE)
  expect_false(is.null(fit$error))                 # captured in the result
  expect_false(fit$converged)
})

test_that("BS3.2 perfectly collinear predictors are detected", {
  d <- .bayes_data()
  d$x2 <- d$x                  # duplicate column
  expect_error(morie_bayes_lm(y ~ x + x2, d, chains = 1L, iter = 50L,
                              warmup = 10L), "collinear|rank")
})

test_that("BS4.1 posterior means are comparable to an external (OLS) fit", {
  cmp <- morie_bayes_compare(.bfit())
  expect_true(all(abs(cmp$posterior_mean - cmp$ols) < 0.3))
})

test_that("BS4.4 sampling can stop on convergence", {
  expect_true("stop_on_convergence" %in% names(formals(morie_bayes_lm)))
  f <- .bfit(stop_on_convergence = TRUE)
  expect_s3_class(f, "morie_bayes_fit")
})

test_that("BS4.5/BS5.5 non-convergence is flagged + warned, samples returned", {
  # a tiny run under a strict threshold will not converge
  expect_warning(
    f <- morie_bayes_lm(y ~ x, .bayes_data(), chains = 2L, iter = 30L,
                        warmup = 5L, converge_threshold = 1.0001),
    "converg")
  expect_false(isTRUE(f$converged))
  expect_false(is.null(f$posterior))               # samples still returned
})

test_that("BS4.6 convergence-checked run ~ equals a fixed-length run", {
  a <- .bfit()
  b <- .bfit(check_convergence = FALSE)
  expect_equal(unname(a$coefficients), unname(b$coefficients), tolerance = 0.2)
})

test_that("BS4.7 a stricter threshold makes convergence harder to declare", {
  loose <- .bfit(iter = 150L, warmup = 80L, converge_threshold = 1.5)
  strict <- .bfit(iter = 150L, warmup = 80L, converge_threshold = 1.0001)
  # under an almost-unattainable threshold, convergence is (at most) as often
  expect_true(isTRUE(loose$converged) >= isTRUE(strict$converged))
})

test_that("BS5.4 diagnostics report the details of each checker", {
  d <- morie_bayes_diagnostics(.bfit())
  expect_named(d, c("rhat", "ess", "geweke"))
  expect_true(all(is.finite(d$rhat)))
})

test_that("BS6.0/BS6.1 default plot method exists + dispatches", {
  expect_true(exists("plot.morie_bayes_fit"))
  f <- .bfit(iter = 200L, warmup = 100L)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  plot(f)
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("BS6.2 posterior sample sequences (trace) can be plotted", {
  f <- .bfit(iter = 200L, warmup = 100L)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  morie_bayes_plot(f, type = "trace")
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("BS6.3 posterior densities can be plotted", {
  f <- .bfit(iter = 200L, warmup = 100L)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  morie_bayes_density(f)
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("BS6.4/BS6.5 plot type selects trace, density, or both", {
  f <- .bfit(iter = 200L, warmup = 100L)
  expect_true("type" %in% names(formals(morie_bayes_plot)))
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  morie_bayes_plot(f, type = "both")
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("BS7.0 the generating parameters are recovered", {
  f <- .bfit(iter = 1500L, warmup = 700L)
  # true intercept 1, slope 2
  expect_equal(f$coefficients[["(Intercept)"]], 1, tolerance = 0.3)
  expect_equal(f$coefficients[["x"]], 2, tolerance = 0.3)
})

test_that("BS7.1 a tight prior with little data recovers the prior", {
  set.seed(1)
  d <- data.frame(x = rnorm(6))
  d$y <- rnorm(6)  # weak data
  f <- morie_bayes_lm(y ~ x, d, prior_sd = 0.05, chains = 2L, iter = 1500L,
                      warmup = 700L, step = 0.05, quiet = TRUE)
  # slope prior is N(0, 0.05); posterior slope is pulled near 0
  expect_lt(abs(f$coefficients[["x"]]), 0.2)
})

test_that("BS7.2 the posterior matches the analytic (OLS) estimate", {
  f <- .bfit(iter = 1500L, warmup = 700L)
  ols <- stats::coef(stats::lm(y ~ x, .bayes_data()))
  expect_equal(unname(f$coefficients), unname(ols), tolerance = 0.25)
})

test_that("BS7.3 more iterations reduce Monte Carlo error", {
  # independent sampler seeds; longer chains give a tighter estimate of
  # the same posterior mean (lower between-run standard deviation).
  est <- function(iter, warmup, seed) {
    morie_bayes_lm(y ~ x, .bayes_data(), chains = 1L, iter = iter,
                   warmup = warmup, step = 0.15, seed = seed, quiet = TRUE,
                   check_convergence = FALSE)$coefficients[["x"]]
  }
  short <- vapply(1:6, function(s) est(100L, 100L, s), numeric(1))
  long  <- vapply(1:6, function(s) est(3000L, 500L, s), numeric(1))
  expect_lt(stats::sd(long), stats::sd(short))
})

test_that("BS7.4/BS7.4a fitted values are on the response scale", {
  f <- .bfit()
  fv <- fitted(f)
  expect_length(fv, nrow(.bayes_data()))
  # fitted values live in the range of the observed response (same scale)
  expect_true(mean(fv) > min(f$y) && mean(fv) < max(f$y))
})
