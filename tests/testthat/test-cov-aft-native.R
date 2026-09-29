# Coverage for the parametric AFT fitters against survival::survreg
# (Weibull, log-logistic and log-normal), the numerical-Hessian standard
# errors, and the Cox-Snell, martingale and deviance residuals recomputed
# from the fitted standardised log-times.

.sdat <- function() {
  set.seed(12)
  n <- 50
  x <- stats::rnorm(n)
  t <- exp(1 + 0.5 * x + 0.7 * log(stats::rexp(n)))
  c <- stats::runif(n, 1, 8)
  list(time = pmin(t, c), event = as.numeric(t <= c), x = x)
}

test_that("the three AFT families reproduce survreg", {
  skip_if_not_installed("survival")
  d <- .sdat()
  for (f in list(list(morie_aft_weibull, "weibull"), list(morie_aft_log_logistic, "loglogistic"),
                 list(morie_aft_generalized_gamma, "lognormal"))) {
    r <- f[[1]](d$time, d$event, matrix(d$x))
    s <- survival::survreg(survival::Surv(d$time, d$event) ~ d$x, dist = f[[2]])
    expect_equal(r$beta, unname(stats::coef(s)), tolerance = 1e-6, info = f[[2]])
    expect_equal(r$sigma, s$scale, tolerance = 1e-6, info = f[[2]])
    # the fitters work on the log-time scale; survreg adds the Jacobian
    # -sum(event * log t) of the time scale
    expect_equal(r$loglik - sum(d$event * log(d$time)), s$loglik[2], tolerance = 1e-8, info = f[[2]])
    # numerical Hessian by central differences: ~1e-5 relative
    expect_equal(r$se, unname(sqrt(diag(stats::vcov(s)))[1:2]), tolerance = 1e-4, info = f[[2]])
    expect_equal(r$aic, 2 * 3 - 2 * r$loglik)
    expect_equal(r$time_ratio, exp(r$beta))
  }
  expect_error(morie_aft_weibull(c(0, d$time[-1]), d$event, matrix(d$x)), "strictly positive")
})

test_that("residuals follow from the standardised log-times", {
  skip_if_not_installed("survival")
  d <- .sdat()
  r <- morie_aft_weibull(d$time, d$event, matrix(d$x))
  res <- morie_aft_residuals(r)
  z <- (log(d$time) - r$beta[1] - r$beta[2] * d$x) / r$sigma
  cs <- exp(z)
  mart <- d$event - cs
  dev <- sign(mart) * sqrt(-2 * (mart + ifelse(d$event > 0, log(d$event - mart), 0)))
  expect_equal(res$standardized, z, tolerance = 1e-12)
  expect_equal(res$cox_snell, cs, tolerance = 1e-12)
  expect_equal(res$martingale, mart, tolerance = 1e-12)
  expect_equal(res$deviance, dev, tolerance = 1e-12)
  ll <- morie_aft_residuals(morie_aft_log_logistic(d$time, d$event, matrix(d$x)))
  expect_equal(ll$cox_snell, log1p(exp(ll$standardized)), tolerance = 1e-12)
  ln <- morie_aft_residuals(morie_aft_generalized_gamma(d$time, d$event, matrix(d$x)))
  expect_equal(ln$cox_snell, -stats::pnorm(ln$standardized, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
  expect_error(morie_aft_residuals(list(time = 1)), "fit is missing 'event'")
  bad <- r
  bad$family <- "gompertz"
  expect_error(morie_aft_residuals(bad), "family must be")
})
