# R/nls_native.R, the lines PR #77 added that no other test reached: the model reader's refusals,
# a constant model's gradient, the analytic-Jacobian refusal, both algorithms' stops (a singular
# gradient, no acceptable step), the not-converged print lines and the small methods, against nls().

.nd <- function(n = 30, seed = 1) {
  set.seed(seed)
  x <- seq(0, 4, length.out = n)
  data.frame(x = x, y = 2 * exp(-0.7 * x) + stats::rnorm(n, sd = 0.02))
}

test_that("the model reader refuses a bad formula, start or length, in words", {
  d <- .nd()
  expect_error(morie_nls("y ~ a * x", d, c(a = 1)), "formula must be a formula.", fixed = TRUE)
  expect_error(morie_nls(y ~ a * x, d, c(1)), "start must be a named list or vector of starting values.", fixed = TRUE)
  expect_error(morie_nls(y ~ a * x, d, list(a = numeric(0))), "every starting value must be non-empty.", fixed = TRUE)
  expect_error(morie_nls(y ~ a * x, d, c(a = NA)), "starting values must be finite numbers.", fixed = TRUE)
  expect_error(morie_nls(y ~ a * x[1:3], d, c(a = 1)), "the left- and right-hand sides have different lengths.", fixed = TRUE)
  expect_error(morie_nls(y ~ a * exp(-b * x), d, c(a = 1, b = 1), control = list(foo = 1)),
               "unknown control settings: foo", fixed = TRUE)
  expect_error(morie_nls(y ~ a * exp(-b * x), d[1:2, ], c(a = 1, b = 1)), "need more observations than parameters.",
               fixed = TRUE)
  expect_error(morie_nls(y ~ a * log(x - 1), d, c(a = 1)),
               "missing or infinite values in the model at the starting values.", fixed = TRUE)
  expect_error(morie_nls(y ~ (a * b) * x, d, c(a = 1, b = 1)),
               "singular gradient matrix at initial parameter estimates.", fixed = TRUE)
  # a function deriv() does not know: analytic is refused, central is used under auto
  ff <- function(x, a) a * x
  expect_error(morie_nls(y ~ ff(x, a), d, c(a = 1), control = list(jacobian = "analytic")),
               "stats::deriv() cannot differentiate this model; use jacobian = 'central'.", fixed = TRUE)
  expect_equal(unname(coef(morie_nls(y ~ ff(x, a), d, c(a = 1)))), unname(coef(stats::nls(y ~ ff(x, a), d, c(a = 1)))),
               tolerance = 1e-6)
  # a constant defined beside the formula, as nls() finds it, and predict() with new data
  k0 <- 0.7
  fk <- morie_nls(y ~ a * exp(-k0 * x), d, c(a = 1))
  expect_equal(unname(coef(fk)), unname(coef(stats::nls(y ~ a * exp(-k0 * x), d, c(a = 1)))), tolerance = 1e-6)
  expect_equal(as.numeric(predict(fk, data.frame(x = c(0, 1)))), unname(coef(fk)) * exp(-k0 * c(0, 1)))
})

test_that("a constant model's gradient is spread over the rows; its fit is the mean, as nls() has it", {
  d <- .nd()
  f <- morie_nls(y ~ a + 0 * x, d, c(a = 1))
  g <- morie_nls(y ~ a, d, c(a = 1))
  expect_equal(unname(coef(g)), mean(d$y), tolerance = 1e-10)
  expect_equal(unname(coef(f)), mean(d$y), tolerance = 1e-10)
})

test_that("both algorithms report their stops: a singular gradient, no acceptable step, too many iterations", {
  d <- .nd()
  # the second parameter stops mattering once the first reaches 0: the gradient turns singular
  d0 <- data.frame(x = 1:20, y = rep(0, 20) + c(1e-9, rep(0, 19)))
  for (alg in c("gauss-newton", "levenberg-marquardt")) {
    w <- character()
    fit <- withCallingHandlers(morie_nls(y ~ a * exp(b * x), d0, c(a = 1, b = -0.1), algorithm = alg,
                                         control = list(warnOnly = TRUE)),
                               warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
    expect_true(length(w) == 1L)
    expect_false(fit$convInfo$isConv)
  }
  # an exact fit: the criterion is 0/0, never converged, and the stop is nls()'s
  de <- data.frame(x = 1:10)
  de$y <- 3 * de$x
  ref <- tryCatch(stats::nls(y ~ a * x + b * x^2, de, c(a = 3, b = 0)), error = conditionMessage)
  expect_error(morie_nls(y ~ a * x + b * x^2, de, c(a = 3, b = 0)), ref, fixed = TRUE)
  # a damping parameter already past its ceiling: no step is tried
  expect_warning(morie_nls(y ~ a * exp(-b * x), d, c(a = 1, b = 1), algorithm = "levenberg-marquardt",
                           control = list(warnOnly = TRUE, lambda = 1, maxlambda = 0.1)),
                 "damping parameter increased beyond 'maxlambda' of 0.1", fixed = TRUE)
  ft <- suppressWarnings(morie_nls(y ~ a * exp(-b * x), d, c(a = 1, b = 3), control = list(maxiter = 1L, warnOnly = TRUE)))
  expect_false(ft$convInfo$isConv)
  expect_true(any(grepl("Not converged:", utils::capture.output(print(ft)), fixed = TRUE)))
  expect_true(any(grepl("Not converged:", utils::capture.output(print(summary(ft))), fixed = TRUE)))
})

test_that("fitted, df.residual and predict without newdata are nls()'s", {
  d <- .nd()
  f <- morie_nls(y ~ a * exp(-b * x), d, c(a = 1, b = 1))
  n <- stats::nls(y ~ a * exp(-b * x), d, c(a = 1, b = 1))
  expect_equal(as.numeric(fitted(f)), as.numeric(fitted(n)), tolerance = 1e-6)
  expect_identical(stats::df.residual(f), stats::df.residual(n))
  expect_equal(as.numeric(predict(f)), as.numeric(predict(n)), tolerance = 1e-6)
})

test_that("both algorithms stop on a gradient that turns singular after accepted steps", {
  # the amplitude goes to zero and takes b's gradient with it (into the subnormals, where qr() is NaN)
  for (dz in list(data.frame(x = 1:20, y = c(1e-9, rep(0, 19))), data.frame(x = 1:20, y = 0))) {
    for (alg in c("gauss-newton", "levenberg-marquardt")) {
      w <- character()
      fit <- withCallingHandlers(morie_nls(y ~ a * exp(b * x), dz, c(a = 1, b = -0.1), algorithm = alg,
                                           control = list(warnOnly = TRUE, maxiter = 500L)),
                                 warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
      expect_length(w, 1L)
      expect_false(fit$convInfo$isConv)
    }
  }
  # a fit kept after a singular gradient still comes back, its covariance unavailable
  fz <- suppressWarnings(morie_nls(y ~ a * exp(b * x), data.frame(x = 1:20, y = 0), c(a = 1, b = -0.1),
                                   control = list(warnOnly = TRUE, maxiter = 500L)))
  expect_s3_class(fz, "morie_nls")
})
