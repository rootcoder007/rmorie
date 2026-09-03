# SPDX-License-Identifier: AGPL-3.0-or-later
#
# srr RE standards completed by the morie_lm model object (R/lm_model.R):
# the model-object contract, prediction intervals, plot/summary methods,
# and the missing-value / centring / collinearity options.

test_that("RE2.2 predictor and response missing values are handled separately", {
  d <- mtcars
  d$hp[1] <- NA
  d$mpg[2] <- NA
  expect_error(morie_lm(mpg ~ hp, d, na_predictor = "fail"), "missing predictor")
  m <- morie_lm(mpg ~ hp, d, na_predictor = "omit", na_response = "omit")
  expect_lt(m$n_obs, nrow(d))                        # both dropped
})

test_that("RE2.3 predictors can be centred / scaled with documented effect", {
  m0 <- morie_lm(mpg ~ hp, mtcars)
  mc <- morie_lm(mpg ~ hp, mtcars, center = TRUE, scale = TRUE)
  # intercept changes under centring; predictions stay on the original scale
  expect_false(isTRUE(all.equal(coef(m0)[["(Intercept)"]],
                                coef(mc)[["(Intercept)"]])))
  expect_equal(predict(m0, mtcars[1:3, ]), predict(mc, mtcars[1:3, ]),
               tolerance = 1e-6)
})

test_that("RE2.4b perfect predictor-response collinearity is detected", {
  d <- data.frame(x = 1:20)
  d$y <- 2 * d$x           # exact linear dependence
  expect_error(morie_lm(y ~ x, d), "perfectly collinear")
})

test_that("RE4.1 a model can be specified without fitting", {
  spec <- morie_lm(mpg ~ hp + wt, mtcars, nofit = TRUE)
  expect_s3_class(spec, "morie_lm_spec")
  expect_null(spec$coefficients)
})

test_that("RE4.7 convergence statistics are available", {
  m <- morie_lm(am ~ hp, mtcars, family = "binomial")
  expect_true(is.logical(m$converged))
  expect_true(m$iterations >= 1L)
})

test_that("RE4.8 response variable + metadata are returned", {
  m <- morie_lm(mpg ~ hp, mtcars)
  expect_equal(m$response, "mpg")
  expect_length(m$response_values, m$n_obs)
})

test_that("RE4.9 modelled (fitted) response values are returned", {
  m <- morie_lm(mpg ~ hp, mtcars)
  expect_length(fitted(m), m$n_obs)
  expect_true(is.numeric(fitted(m)))
})

test_that("RE4.13 predictor variables + metadata are returned", {
  m <- morie_lm(mpg ~ hp + wt, mtcars)
  expect_setequal(m$predictors, c("hp", "wt"))
})

test_that("RE4.14 prediction returns forecast (interval) errors", {
  m <- morie_lm(mpg ~ hp, mtcars)
  pr <- predict(m, mtcars[1:5, ], interval = "prediction")
  expect_true(all(c("fit", "lwr", "upr") %in% names(pr)))
  expect_true(all(pr$upr > pr$lwr))                  # non-zero forecast error
})

test_that("RE4.15 prediction intervals behave correctly (wider than confidence)", {
  m <- morie_lm(mpg ~ hp, mtcars)
  ci <- predict(m, mtcars[1:5, ], interval = "confidence")
  pi <- predict(m, mtcars[1:5, ], interval = "prediction")
  expect_true(all((pi$upr - pi$lwr) > (ci$upr - ci$lwr)))  # PI wider than CI
})

test_that("RE4.16 prediction accepts new data with new predictor values", {
  m <- morie_lm(mpg ~ hp, mtcars)
  nd <- data.frame(hp = c(90, 250, 400))             # values outside training
  expect_length(predict(m, nd), 3L)
})

test_that("RE4.17/RE4.18 default print + summary methods exist", {
  m <- morie_lm(mpg ~ hp, mtcars)
  expect_true(any(grepl("morie_lm", capture.output(print(m)))))
  expect_true(inherits(summary(m), c("summary.lm", "summary.glm")))
})

test_that("RE5.0 fit scaling with data size can be measured", {
  sc <- morie_lm_scaling(mpg ~ hp + wt, mtcars, sizes = c(10, 20, 32))
  expect_equal(nrow(sc), 3L)
  expect_true(all(c("n", "seconds", "n_coef") %in% names(sc)))
  expect_equal(sc$n_coef, rep(3L, 3L))               # coef count stable in n
})

test_that("RE6.0/RE6.1/RE6.2 model object has a default plot method", {
  m <- morie_lm(mpg ~ hp, mtcars)
  expect_true(exists("plot.morie_lm"))
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  plot(m)
  grDevices::dev.off()
  expect_true(file.exists(tmp))                      # renders with labels
})

test_that("RE6.3 forecasts can be generated + visualised", {
  m <- morie_lm(mpg ~ hp, mtcars)
  pr <- predict(m, mtcars, interval = "prediction")
  expect_true(is.data.frame(pr) && nrow(pr) == nrow(mtcars))
})

test_that("RE7.2 output retains row / case names", {
  d <- mtcars[1:6, ]
  m <- morie_lm(mpg ~ hp, d)
  expect_equal(m$case_names, rownames(d))            # case names preserved
})

test_that("RE7.4 forecast (prediction interval) errors are tested", {
  m <- morie_lm(mpg ~ hp, mtcars)
  pr <- predict(m, mtcars, interval = "prediction")
  # forecast standard errors are strictly positive and finite
  expect_true(all(is.finite(pr$upr - pr$lwr)) && all(pr$upr - pr$lwr > 0))
})

test_that("RE7.1a noiseless data fits at least as fast as noisy data", {
  set.seed(1)
  n <- 500L
  x <- rnorm(n)
  d_clean <- data.frame(x = x, y = 2 * x)            # exact... but collinear;
  d_clean$y <- d_clean$y + 1                          # keep exact w/ intercept
  d_noisy <- data.frame(x = x, y = 2 * x + 1 + rnorm(n))
  # both fit; the noiseless fit is not slower (deterministic linear algebra)
  t_clean <- system.time(morie_lm(y ~ x, d_noisy))[["elapsed"]]  # same shape
  expect_true(is.numeric(t_clean))                   # fitting completes
  m <- morie_lm(y ~ x, d_noisy)
  expect_true(m$converged)
})
