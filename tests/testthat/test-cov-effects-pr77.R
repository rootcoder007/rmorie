# R/effects.R, the lines PR #77 added that no other test reached in morie_effects_tidy(): a model
# whose coefficient table has no p-value column (lme4) and a confint() that returns a bare vector
# (a one-coefficient glm), against lme4 and broom.

test_that("a three-column coefficient table gets NA p-values and lme4's estimates", {
  skip_if_not_installed("lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  td <- morie_effects_tidy(m)
  expect_identical(td$term, c("(Intercept)", "Days"))
  expect_equal(td$estimate, unname(lme4::fixef(m)), tolerance = 1e-12)
  expect_equal(td$std.error, unname(sqrt(diag(as.matrix(stats::vcov(m))))), tolerance = 1e-12)
  expect_equal(td$statistic, td$estimate / td$std.error, tolerance = 1e-12)
  expect_true(all(is.na(td$p.value)))
})

test_that("a one-coefficient glm's vector confint becomes one interval row, as broom has it", {
  skip_if_not_installed("broom")
  set.seed(1)
  d <- data.frame(y = stats::rbinom(30, 1, 0.4))
  g <- stats::glm(y ~ 1, family = stats::binomial(), data = d)
  expect_null(dim(suppressMessages(stats::confint(g))))
  td <- morie_effects_tidy(g, conf.int = TRUE)
  ref <- suppressMessages(broom::tidy(g, conf.int = TRUE))
  expect_equal(td$conf.low, ref$conf.low, tolerance = 1e-10)
  expect_equal(td$conf.high, ref$conf.high, tolerance = 1e-10)
  expect_equal(td$estimate, qlogis(mean(d$y)), tolerance = 1e-8)
})
