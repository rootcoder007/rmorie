# SPDX-License-Identifier: AGPL-3.0-or-later
# Native design-based mean and GLM (morie_survey_design / morie_survey_mean / morie_survey_glm)
# against survey::svydesign + svymean + svyglm: weights only, strata, clusters, fpc, missing
# covariates, and a survey design object passed straight in.

test_that("native survey designs reproduce survey::svymean and svyglm", {
  skip_if_not_installed("survey")
  set.seed(3)
  n <- 120
  df <- data.frame(y = rbinom(n, 1, .4), yc = rnorm(n, 10, 2), x = rnorm(n), w = runif(n, .5, 3),
                   s = rep(c("a", "b", "c"), each = 40), psu = rep(1:30, each = 4))
  df$pop <- c(a = 200, b = 150, c = 90)[df$s]
  df$x[c(3, 17, 55, 101)] <- NA
  cfgs <- list(list(), list(s = "s"), list(s = "s", c = "psu"), list(s = "s", c = "psu", f = "pop"),
               list(c = "psu"))
  for (cfg in cfgs) {
    d <- morie_survey_design(df, "w", strata_col = cfg$s, cluster_col = cfg$c, fpc_col = cfg$f, nest = TRUE)
    ref <- survey::svydesign(ids = if (is.null(cfg$c)) ~1 else ~psu, strata = if (is.null(cfg$s)) NULL else ~s,
                             fpc = if (is.null(cfg$f)) NULL else ~pop, weights = ~w, data = df, nest = TRUE)
    m <- morie_survey_mean(d, "yc")
    r <- survey::svymean(~yc, ref)
    expect_equal(c(m$mean, m$se), unname(c(coef(r), survey::SE(r))), tolerance = 1e-12)
    g <- morie_survey_glm(d, yc ~ x)
    rg <- summary(survey::svyglm(yc ~ x, ref))$coefficients
    expect_equal(unname(g$coefficients), unname(rg), tolerance = 1e-12)
    gb <- morie_survey_glm(d, y ~ x, family = "binomial")
    rb <- summary(survey::svyglm(y ~ x, ref, family = quasibinomial()))$coefficients
    expect_equal(unname(gb$coefficients), unname(rb), tolerance = 1e-6)
    g2 <- morie_survey_glm(ref, yc ~ x)
    expect_equal(unname(g2$coefficients), unname(rg), tolerance = 1e-12)
  }
})
