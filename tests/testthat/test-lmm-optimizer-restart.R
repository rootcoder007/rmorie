# SPDX-License-Identifier: AGPL-3.0-or-later
# nlminb's "singular convergence" stopped morie_lmm() on the theta boundary, short of the REML
# maximum, and a restart from that point only confirmed it: Orthodont's random slope with y
# perturbed by 1e-10 gave logLik -222.54 against nlme's and lme4's -221.32 (31 of 60 such
# perturbations missed; macOS CI hit it on the unperturbed data).

test_that("a singular nlminb stop is not taken as the REML maximum", {
  skip_if_not_installed("nlme")
  data(Orthodont, package = "nlme")
  d <- as.data.frame(Orthodont)
  set.seed(1)
  d$distance <- d$distance * (1 + 1e-10 * stats::rnorm(nrow(d)))
  f <- morie_lmm(distance ~ age, d, random = ~ age | Subject)
  n <- nlme::lme(distance ~ age, d, random = ~ age | Subject, method = "REML")
  expect_equal(as.numeric(f$logLik), as.numeric(stats::logLik(n)), tolerance = 1e-6)
  expect_false(isTRUE(f$convergence$singular))
  expect_gt(f$theta[[1L]], 1)
})

test_that("perturbations of the data never cost the maximum", {
  skip_if_not_installed("lme4")
  data(Orthodont, package = "nlme")
  o <- as.data.frame(Orthodont)
  for (i in 1:12) {
    d <- o
    set.seed(i)
    d$distance <- d$distance * (1 + 10^-(8 + i %% 5) * stats::rnorm(nrow(d)))
    a <- as.numeric(morie_lmm(distance ~ age + (age | Subject), d)$logLik)
    b <- as.numeric(stats::logLik(lme4::lmer(distance ~ age + (age | Subject), d)))
    expect_lt(b - a, 1e-5, label = sprintf("perturbation %d", i))
  }
})
