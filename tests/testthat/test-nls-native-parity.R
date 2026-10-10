# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity of morie_nls (native formula nonlinear least squares) with
# stats::nls, the reference.
#
# Tolerances.  morie_nls runs nls's own Gauss-Newton iteration (QR
# increment, step factor halved / doubled, relative-offset test before
# each step) but with an analytic Jacobian from deriv(), whereas default
# nls uses forward differences (relative error ~1e-8).  Hence:
#  * against nls given the SAME analytic gradient (a deriv() function in
#    the formula) the iterates are identical: 1e-10;
#  * against default nls, estimates agree to ~1e-8 (observed 6e-10 ..
#    4e-8): the forward-difference gradient moves nls's increments, and
#    both stop as soon as the relative offset is below tol = 1e-5, a few
#    1e-6 (relative) short of the exact minimum; 1e-7 is used.  Standard
#    errors use the gradient at the end point, so they differ by the
#    forward-difference error itself (observed <= 3e-8); 1e-6 is used.

.nlsn_rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-300))

.nlsn_cases <- function() {
  set.seed(1)
  n <- 60
  d1 <- data.frame(x = seq(0, 6, length.out = n))
  d1$y <- 4 * (1 - exp(-0.7 * d1$x)) + stats::rnorm(n, sd = 0.1)
  d2 <- data.frame(t = seq(0, 20, length.out = n))
  d2$y <- 10 / (1 + exp((8 - d2$t) / 2.5)) + stats::rnorm(n, sd = 0.3)
  d3 <- datasets::Puromycin[datasets::Puromycin$state == "treated", ]
  list(
    exprise = list(fo = y ~ a * (1 - exp(-b * x)), d = d1, st = list(a = 3, b = 1),
                   g = stats::deriv(~ a * (1 - exp(-b * x)), c("a", "b"),
                                    function(a, b, x) NULL),
                   fg = y ~ g(a, b, x)),
    logistic = list(fo = y ~ Asym / (1 + exp((xmid - t) / scal)), d = d2,
                    st = list(Asym = 8, xmid = 7, scal = 2),
                    g = stats::deriv(~ Asym / (1 + exp((xmid - t) / scal)),
                                     c("Asym", "xmid", "scal"),
                                     function(Asym, xmid, scal, t) NULL),
                    fg = y ~ g(Asym, xmid, scal, t)),
    michaelis = list(fo = rate ~ Vm * conc / (K + conc), d = d3, st = c(Vm = 200, K = 0.05),
                     g = stats::deriv(~ Vm * conc / (K + conc), c("Vm", "K"),
                                      function(Vm, K, conc) NULL),
                     fg = rate ~ g(Vm, K, conc))
  )
}

test_that("Gauss-Newton matches stats::nls on three standard models", {
  for (cs in .nlsn_cases()) {
    q <- stats::nls(cs$fo, cs$d, cs$st)
    sq <- summary(q)
    f <- rmorie::morie_nls(cs$fo, cs$d, cs$st)
    expect_identical(f$jacobian, "analytic")
    expect_lt(.nlsn_rel(coef(f), coef(q)), 1e-7)
    expect_lt(.nlsn_rel(f$coef_table[, 2], sq$coefficients[, 2]), 1e-6)
    expect_lt(.nlsn_rel(f$sigma, sq$sigma), 1e-10)
    expect_identical(f$df, sq$df)
    expect_identical(f$convInfo$finIter, q$convInfo$finIter)
    expect_true(f$convInfo$isConv)
    # with the same analytic gradient, nls runs the identical iteration
    g <- cs$g
    environment(cs$fg) <- environment()
    qa <- stats::nls(cs$fg, cs$d, cs$st)
    expect_lt(.nlsn_rel(coef(f), coef(qa)), 1e-10)
    expect_lt(.nlsn_rel(f$coef_table[, 2], summary(qa)$coefficients[, 2]), 1e-10)
  }
})

test_that("central-difference Jacobian gives the same fit", {
  for (cs in .nlsn_cases()) {
    q <- stats::nls(cs$fo, cs$d, cs$st)
    f <- rmorie::morie_nls(cs$fo, cs$d, cs$st, control = list(jacobian = "central"))
    expect_identical(f$jacobian, "central")
    expect_lt(.nlsn_rel(coef(f), coef(q)), 1e-7)
    expect_lt(.nlsn_rel(f$coef_table[, 2], summary(q)$coefficients[, 2]), 1e-6)
  }
})

test_that("Levenberg-Marquardt reaches the same minimum", {
  for (cs in .nlsn_cases()) {
    q <- stats::nls(cs$fo, cs$d, cs$st)
    f <- rmorie::morie_nls(cs$fo, cs$d, cs$st, algorithm = "levenberg-marquardt")
    # a different path, stopped by the same tol = 1e-5 relative-offset test
    expect_lt(.nlsn_rel(coef(f), coef(q)), 1e-6)
    expect_lt(.nlsn_rel(f$coef_table[, 2], summary(q)$coefficients[, 2]), 1e-6)
    ft <- rmorie::morie_nls(cs$fo, cs$d, cs$st, algorithm = "levenberg-marquardt",
                           control = list(tol = 1e-8))
    g <- cs$g
    environment(cs$fg) <- environment()
    qt <- stats::nls(cs$fg, cs$d, cs$st, control = stats::nls.control(tol = 1e-8))
    expect_lt(.nlsn_rel(coef(ft), coef(qt)), 1e-9)
  }
})

test_that("methods: predict, vcov, logLik, residuals, print", {
  cs <- .nlsn_cases()$michaelis
  q <- stats::nls(cs$fo, cs$d, cs$st)
  f <- rmorie::morie_nls(cs$fo, cs$d, cs$st)
  nd <- data.frame(conc = c(0.1, 0.5, 1))
  expect_lt(.nlsn_rel(predict(f, nd), predict(q, nd)), 1e-7)
  expect_lt(.nlsn_rel(vcov(f), vcov(q)), 1e-6)
  expect_equal(as.numeric(logLik(f)), as.numeric(logLik(q)), tolerance = 1e-10)
  # residuals near zero make elementwise relative errors meaningless: scale by sigma
  expect_lt(max(abs(residuals(f) - as.numeric(residuals(q)))) / f$sigma, 1e-7)
  expect_equal(deviance(f), deviance(q), tolerance = 1e-10)
  expect_output(print(f), "Nonlinear regression model")
  expect_output(print(summary(f)), "Residual standard error")
})

test_that("indexed parameters, I(), one-sided formulas and errors", {
  cs <- .nlsn_cases()$michaelis
  q <- stats::nls(cs$fo, cs$d, cs$st)
  fi <- rmorie::morie_nls(rate ~ b[1] * conc / (b[2] + conc), cs$d, list(b = c(200, 0.05)))
  expect_identical(names(coef(fi)), c("b1", "b2"))
  expect_lt(.nlsn_rel(unname(coef(fi)), unname(coef(q))), 1e-7)
  f2 <- rmorie::morie_nls(rate ~ I(Vm * conc) / (K + conc), cs$d, cs$st)
  expect_lt(.nlsn_rel(coef(f2), coef(q)), 1e-7)
  f3 <- rmorie::morie_nls(~ rate - Vm * conc / (K + conc), cs$d, cs$st)
  expect_lt(.nlsn_rel(coef(f3), coef(q)), 1e-7)
  expect_error(rmorie::morie_nls(rate ~ Vm * conc / (K + conc2), cs$d, cs$st), "conc2")
  expect_error(rmorie::morie_nls(rate ~ Vm * conc / (K + conc), cs$d, c(Vm = 1, K = 10)),
               "minFactor")
  expect_warning(f5 <- rmorie::morie_nls(cs$fo, cs$d, cs$st,
                                        control = list(maxiter = 1, warnOnly = TRUE)),
                 "exceeded maximum")
  expect_false(f5$convInfo$isConv)
})

test_that("n = 50,000 with 3 parameters fits in under 2 s", {
  testthat::skip_on_cran()
  set.seed(2)
  n <- 50000
  d <- data.frame(t = stats::runif(n, 0, 20))
  d$y <- 10 / (1 + exp((8 - d$t) / 2.5)) + stats::rnorm(n, sd = 0.5)
  fo <- y ~ Asym / (1 + exp((xmid - t) / scal))
  st <- list(Asym = 8, xmid = 7, scal = 2)
  tm <- system.time(f <- rmorie::morie_nls(fo, d, st))[["elapsed"]]
  expect_lt(tm, 2)
  q <- stats::nls(fo, d, st)
  expect_lt(.nlsn_rel(coef(f), coef(q)), 1e-7)
  expect_lt(.nlsn_rel(f$coef_table[, 2], summary(q)$coefficients[, 2]), 1e-6)
})
