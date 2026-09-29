# Cross tests: CrpsCdf against scoringRules closed forms; BvLogisticSimulate against evd.

test_that("CrpsCdf equals scoringRules::crps_norm, crps_logis and crps_gamma", {
  skip_if_not_installed("scoringRules")
  y <- c(-2.1, -0.4, 0.3, 1.7, 3.2)
  for (v in y) {
    expect_equal(CrpsCdf(function(z) pnorm(z, 0.5, 1.4), v), scoringRules::crps_norm(v, 0.5, 1.4), tolerance = 1e-11)
    expect_equal(CrpsCdf(function(z) plogis(z, -0.2, 0.8), v), scoringRules::crps_logis(v, -0.2, 0.8), tolerance = 1e-11)
    if (v > 0) {
      expect_equal(CrpsCdf(function(z) pgamma(z, 2.5, 1.3), v, breaks = 0), scoringRules::crps_gamma(v, 2.5, 1.3),
                   tolerance = 1e-10)
    }
  }
})

test_that("BvLogisticSimulate reproduces the evd bivariate logistic distribution", {
  skip_if_not_installed("evd")
  n <- 20000
  r <- BvLogisticSimulate(n, 0.55, loc = c(1, -2), scale = c(0.5, 2), shape = c(0.2, -0.1), seed = 21)
  pts <- rbind(c(1, -2), c(1.5, 0), c(0.7, -3), c(2, 1))
  for (k in seq_len(nrow(pts))) {
    emp <- mean(r$x <= pts[k, 1] & r$y <= pts[k, 2])
    th <- evd::pbvevd(pts[k, ], dep = 0.55, model = "log", mar1 = c(1, 0.5, 0.2), mar2 = c(-2, 2, -0.1))
    expect_lt(abs(emp - th), 4 * sqrt(th * (1 - th) / n))
  }
})
