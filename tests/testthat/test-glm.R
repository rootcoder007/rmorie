# Generalised linear models, anchored on R's stats::glm.
# morie_glm is a self-contained IRLS reimplementation; these compare it
# with the reference on the same 40-observation fixture.

n <- 40
x1 <- sapply(0:(n - 1), function(i) ((i * 7) %% 11) / 5 - 1)
x2 <- sapply(0:(n - 1), function(i) ((i * 5) %% 7) / 3 - 1)
X <- cbind(x1, x2)
yb <- c(0,1,0,1,1,0,1,1,0,1, 1,0,0,1,1,1,0,1,0,1,
        1,1,0,0,1,0,1,1,1,0, 1,0,1,1,0,1,1,0,1,1)
yc <- c(2,0,3,1,4,2,1,0,5,3, 2,1,6,2,0,3,4,1,2,5,
        1,3,0,2,4,2,1,3,2,6, 0,2,3,1,4,2,5,1,3,2)
yg <- c(1.2,3.4,0.8,2.1,5.6,1.9,2.7,0.5,4.3,3.1,
        1.4,2.2,6.1,1.7,0.9,3.3,4.8,1.1,2.5,5.2,
        1.6,3.7,0.7,2.4,4.1,1.8,2.9,3.6,2.0,6.4,
        0.6,2.3,3.9,1.3,4.5,2.6,5.1,1.5,3.2,2.8)

test_that("logistic regression matches glm(family = binomial)", {
  f <- morie_glm(yb, X, "binomial")
  expect_equal(f$coef[1], 0.575508023961936, tolerance = 1e-10)
  expect_equal(f$coef[2], 0.674299692288504, tolerance = 1e-10)
  expect_equal(f$coef[3], -0.892689060687477, tolerance = 1e-10)
  expect_equal(f$se[1], 0.352865639103213, tolerance = 1e-10)
  expect_equal(f$se[2], 0.554161354747213, tolerance = 1e-10)
  expect_equal(f$se[3], 0.531199272470709, tolerance = 1e-10)
  expect_true(f$converged)
})

test_that("Wald statistics match summary.glm", {
  f <- morie_glm(yb, X, "binomial")
  expect_identical(f$statistic_name, "z")
  expect_equal(f$statistic[2], 1.21679306308917, tolerance = 1e-10)
  expect_equal(f$statistic[3], -1.68051634659703, tolerance = 1e-10)
  expect_equal(f$p_value[2], 0.223682960012271, tolerance = 1e-9)
  expect_equal(f$p_value[3], 0.0928568965556806, tolerance = 1e-9)
  expect_identical(f$dispersion, 1)
})

test_that("deviance and AIC match glm", {
  f <- morie_glm(yb, X, "binomial")
  expect_equal(f$deviance, 48.1888471033342, tolerance = 1e-11)
  expect_equal(f$null_deviance, 52.9250590526386, tolerance = 1e-11)
  expect_equal(f$aic, 54.1888471033342, tolerance = 1e-11)
  expect_identical(f$df_residual, 37L)
  expect_identical(f$df_null, 39L)
})

test_that("standard errors use the final-solve weights", {
  # summary.glm inverts the QR stored by the last IRLS step, whose
  # weights sit at the PREVIOUS eta.  Recomputing them at the converged
  # eta shifts every standard error in its eighth digit -- invisible in
  # beta, visible here, which is why this is pinned.
  f <- morie_glm(yb, X, "binomial")
  expect_equal(f$se[1], 0.352865639103213, tolerance = 1e-12)
  expect_gt(abs(f$se[1] - 0.352865645205766), 1e-10)
})

test_that("fitted values and deviance residuals match glm", {
  f <- morie_glm(yb, X, "binomial")
  expect_equal(f$fitted[1], 0.68866756307354, tolerance = 1e-11)
  expect_equal(f$fitted[10], 0.727125645237499, tolerance = 1e-11)
  dr <- morie_deviance_residuals(f, yb)
  expect_equal(dr[1], -1.52767405468362, tolerance = 1e-11)
  expect_equal(dr[2], 1.07321463302608, tolerance = 1e-11)
  # their sum of squares IS the deviance
  expect_equal(sum(dr^2), f$deviance, tolerance = 1e-10)
})

test_that("Poisson regression matches glm(family = poisson)", {
  f <- morie_glm(yc, X, "poisson")
  expect_equal(f$coef[1], 0.840912082217813, tolerance = 1e-10)
  expect_equal(f$coef[2], -0.0653858891755061, tolerance = 1e-9)
  expect_equal(f$coef[3], 0.236522669977984, tolerance = 1e-10)
  expect_equal(f$se[1], 0.104553588511299, tolerance = 1e-10)
  expect_equal(f$se[2], 0.160148592152359, tolerance = 1e-10)
  expect_equal(f$deviance, 49.0196717101663, tolerance = 1e-11)
  expect_equal(f$aic, 151.532951940163, tolerance = 1e-11)
})

test_that("a constant log-offset shifts only the intercept", {
  # the property that makes rate models work: exposure enters as an
  # offset and must leave every slope untouched
  f <- morie_glm(yc, X, "poisson")
  o <- morie_glm(yc, X, "poisson", offset = rep(log(2), n))
  expect_equal(o$coef[1], 0.147764901657868, tolerance = 1e-10)
  expect_equal(o$coef[1], f$coef[1] - log(2), tolerance = 1e-8)
  expect_equal(o$coef[3], f$coef[3], tolerance = 1e-8)
})

test_that("gaussian GLM is least squares with a t statistic", {
  f <- morie_glm(yg, X, "gaussian")
  expect_equal(f$coef[1], 2.79318065495427, tolerance = 1e-10)
  expect_equal(f$coef[2], 0.181934504572747, tolerance = 1e-10)
  expect_equal(f$coef[3], 0.825089714755335, tolerance = 1e-10)
  expect_identical(f$statistic_name, "t")
  expect_equal(f$dispersion, 2.27985899217362, tolerance = 1e-11)
  expect_equal(f$statistic[2], 0.489116569745558, tolerance = 1e-10)
  expect_equal(f$p_value[2], 0.627646213372388, tolerance = 1e-9)
  # deviance IS the residual sum of squares for this family
  expect_equal(f$deviance, sum(f$residuals^2), tolerance = 1e-10)
})

test_that("Gamma log-link matches glm(family = Gamma(link = 'log'))", {
  f <- morie_glm(yg, X, "gamma")
  expect_equal(f$coef[1], 1.0064775735195, tolerance = 1e-9)
  expect_equal(f$coef[2], 0.037055477172802, tolerance = 1e-7)
  expect_equal(f$coef[3], 0.306389256569374, tolerance = 1e-8)
  expect_equal(f$se[2], 0.133798105402901, tolerance = 1e-8)
  expect_equal(f$dispersion, 0.294986758013349, tolerance = 1e-8)
  expect_equal(f$deviance, 12.9790508743701, tolerance = 1e-8)
})

test_that("predict returns the two scales", {
  f <- morie_glm(yb, X, "binomial")
  eta <- morie_glm_predict(f, X, type = "link")
  mu <- morie_glm_predict(f, X, type = "response")
  expect_equal(eta, f$linear_predictor, tolerance = 1e-12)
  expect_equal(mu, f$fitted, tolerance = 1e-12)
  # the link scale is unbounded, the response scale never leaves (0, 1)
  expect_true(all(mu > 0 & mu < 1))
  expect_equal(mu, 1 / (1 + exp(-eta)), tolerance = 1e-12)
})

test_that("prior weights replicate duplicated observations", {
  a <- morie_glm(yb, X, "binomial", weights = rep(2, n))
  b <- morie_glm(c(yb, yb), rbind(X, X), "binomial")
  expect_equal(a$coef, b$coef, tolerance = 1e-8)
  expect_equal(a$deviance, b$deviance, tolerance = 1e-7)
})

test_that("the intercept can be dropped", {
  f <- morie_glm(yc, X, "poisson", add_intercept = FALSE)
  expect_identical(f$k, 2L)
  expect_identical(f$df_residual, 38L)
  expect_identical(f$df_null, 40L)
})

test_that("bad input is refused", {
  expect_error(morie_glm(yb, X, "binomial2"), "family must be")
  expect_error(morie_glm(yb[1:5], X, "binomial"), "rows but y has")
  expect_error(morie_glm(rep(2, n), X, "binomial"), "must lie in")
  expect_error(morie_glm(rep(-1, n), X, "poisson"), "non-negative")
  expect_error(morie_glm(yb[1:2], X[1:2, ], "binomial"),
               "more observations")
})

test_that("collinear predictors are refused, not silently fudged", {
  Xc <- cbind(x1, 2 * x1)
  expect_error(morie_glm(yb, Xc, "binomial"), "collinear")
})

test_that("predict rejects a mismatched design", {
  f <- morie_glm(yb, X, "binomial")
  expect_error(morie_glm_predict(f, matrix(1, 3, 1)), "columns but the fit")
})
