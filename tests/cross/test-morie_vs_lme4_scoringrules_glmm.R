test_that("SpatialGlmmFit equals glmer (tight PIRLS) and CRPS equal scoringRules", {
  skip_if_not_installed("lme4")
  u <- .morie_random_uniform(200, seed = 7, stream = 0)
  y <- c(5, 6, 0, 1, 6, 6, 0, 1, 1, 0, 2, 3, 1, 1, 6, 1, 4, 6, 0, 2, 2, 0, 4, 2, 14, 7, 2, 8, 3, 6, 12, 8, 2, 7, 8, 5,
         2, 4, 6, 7, 7, 17, 2, 1, 2, 5, 0, 1, 0, 3, 1, 4, 0, 7, 5, 8, 5, 2, 6, 1)
  x <- u[1:60]
  g <- rep(0:9, each = 6)
  f <- lme4::glmer(y ~ x + (1 | g), data = data.frame(y = y, x = x, g = factor(g)), family = poisson,
                   control = lme4::glmerControl(optimizer = "bobyqa", tolPwrss = 1e-13, optCtrl = list(rhoend = 1e-12)))
  r <- SpatialGlmmFit(y, cbind(1, x), groups = g)
  expect_equal(r$beta, unname(lme4::fixef(f)), tolerance = 1e-6)
  expect_equal(r$sigma2, as.numeric(lme4::VarCorr(f)), tolerance = 1e-6)
  expect_equal(r$loglik, as.numeric(stats::logLik(f)), tolerance = 1e-9)
  m <- c(3, 7, 2, 9, 5)
  yb <- c(1, 4, 0, 6, 2)
  gb <- c(0, 0, 1, 1, 2)
  fb <- lme4::glmer(cbind(yb, m - yb) ~ 1 + (1 | gb), data = data.frame(yb = yb, m = m, gb = factor(gb)),
                    family = binomial,
                    control = lme4::glmerControl(optimizer = "bobyqa", tolPwrss = 1e-13, optCtrl = list(rhoend = 1e-12)))
  rb <- SpatialGlmmFit(yb, matrix(1, 5, 1), family = "binomial", groups = gb, trials = m)
  expect_equal(rb$loglik, as.numeric(stats::logLik(fb)), tolerance = 1e-7)
  skip_if_not_installed("scoringRules")
  yy <- c(0, 2, 5, 11)
  lam <- c(0.5, 2.5, 4, 9.3)
  expect_equal(CrpsPoisson(yy, lam), scoringRules::crps_pois(yy, lam), tolerance = 1e-12)
  expect_equal(CrpsGaussian(yy, lam, 1.3), scoringRules::crps_norm(yy, lam, 1.3), tolerance = 1e-12)
  S <- lapply(1:4, function(i) .morie_random_normal(15, seed = 5, stream = i))
  expect_equal(CrpsSample(yy / 5, S), scoringRules::crps_sample(yy / 5, do.call(rbind, S)), tolerance = 1e-12)
})
