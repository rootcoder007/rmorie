# Cross test: Weibull shared gamma-frailty model against parfm.

test_that("WeibullFrailtyFit equals parfm::parfm (Weibull baseline, gamma frailty)", {
  skip_if_not_installed("parfm")
  i <- 0:59
  x1 <- sin(i * 0.7)
  cl <- i %/% 6
  df <- data.frame(time = exp(1 + 0.5 * x1 + 0.4 * cos(cl * 1.7) + 0.6 * sin(i * 2.3)), event = as.numeric(i %% 5 != 0),
                   x1 = x1, cl = cl)
  ref <- suppressWarnings(parfm::parfm(survival::Surv(time, event) ~ x1, cluster = "cl", data = df, dist = "weibull",
                                       frailty = "gamma"))
  got <- WeibullFrailtyFit(df$time, df$event, matrix(x1), cl)
  # parfm maximises with nlminb (default tolerances)
  expect_equal(c(got$theta, got$rho, got$lambda_, got$beta), unname(ref[, "ESTIMATE"]), tolerance = 1e-4)
  expect_equal(got$loglik, as.numeric(attributes(ref)$loglik), tolerance = 1e-7)
})
