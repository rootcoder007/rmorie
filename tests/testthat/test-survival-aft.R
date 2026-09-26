## Reference: survreg(Surv(t, e) ~ x1 + x2, dist = ...,
## control = survreg.control(rel.tolerance = 1e-14)) on the deterministic
## data below (the same data as morie tests/test_survival_reference.py).
.aft_data <- function() {
  i <- 0:149
  x1 <- sin(1.3 * i)
  x2 <- as.numeric(cos(0.9 * i) > 0)
  g <- i %% 2
  rate <- exp(0.4 * x1 - 0.5 * x2 + 0.3 * g)
  tt <- round(-log(0.02 + 0.96 * abs(sin(2.3 * i + 0.4))) / rate * 5, 1)
  cc <- round(2 + 8 * abs(cos(1.9 * i)), 1)
  data.frame(t = pmax(pmin(tt, cc), 0.1), e = as.numeric(tt <= cc), x1 = x1, x2 = x2)
}

test_that("morie_aft matches survreg (coefficients, scale, SEs, loglik)", {
  d <- .aft_data()
  X <- cbind(x1 = d$x1, x2 = d$x2)
  ref <- list(
    weibull = list(c(0.95464566434250653, -0.42397359150975616, 0.3596205839009648), 1.2082781490838239,
                   c(0.14915168219246222, 0.15600025833897435, 0.21785133436179238, 0.073715052052702901),
                   -262.92038894138722),
    lognormal = list(c(0.2930708670275064, -0.44501849394716181, 0.40973903203989143), 1.5231352509549119,
                     c(0.176947723904718, 0.18112984231016344, 0.25486483101727631, 0.06506216323184047),
                     -263.31995645449791),
    loglogistic = list(c(0.35322548245078, -0.40628730573015726, 0.39980424072509529), 0.90923066767639271,
                       c(0.18417134590609704, 0.18851426245724751, 0.26365034787765024, 0.073649829110218265),
                       -266.62828968729383))
  for (di in names(ref)) {
    m <- morie_aft(d$t, d$e, X, di)
    expect_equal(unname(m$coefficients), ref[[di]][[1]], tolerance = 1e-11)
    expect_equal(m$scale, ref[[di]][[2]], tolerance = 1e-11)
    expect_equal(unname(sqrt(diag(m$vcov))), ref[[di]][[3]], tolerance = 1e-10)
    expect_equal(m$loglik, ref[[di]][[4]], tolerance = 1e-13)
  }
})

test_that("exponential AFT fixes the scale at 1", {
  d <- .aft_data()
  m <- morie_aft(d$t, d$e, cbind(x1 = d$x1), "exponential")
  expect_identical(m$scale, 1)
  expect_equal(dim(m$vcov), c(2L, 2L))
})

test_that("intercept-only morie_aft matches survreg(Surv ~ 1)", {
  skip_if_not_installed("survival")
  d <- .aft_data()
  for (di in c("weibull", "lognormal", "loglogistic", "exponential")) {
    m <- morie_aft(d$t, d$e, NULL, di)
    f <- survival::survreg(survival::Surv(t, e) ~ 1, data = d, dist = di,
                           control = survival::survreg.control(rel.tolerance = 1e-14))
    expect_equal(unname(m$coefficients), unname(stats::coef(f)), tolerance = 1e-10)
    expect_equal(m$loglik, f$loglik[2], tolerance = 1e-12)
  }
})
