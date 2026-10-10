# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native Type-II / Type-III tests in anova_table() cross-validated
# against car::Anova (its defaults: F tests for lm, likelihood-ratio
# chi-square for glm). The lm sums of squares are computed by QR model
# comparison where car uses linear hypotheses on the full fit -- the same
# quantities, so they agree to rounding; we use 1e-8 relative. The glm
# refits use glm.fit with the same inputs and control as car, so the
# LR statistics agree to rounding too.

.anova_data <- function() {
  set.seed(11)
  n <- 90
  d <- data.frame(
    a = factor(sample(c("p", "q", "r"), n, TRUE, prob = c(0.5, 0.3, 0.2))),
    b = factor(sample(c("u", "v"), n, TRUE, prob = c(0.65, 0.35))),
    x = stats::rnorm(n),
    z = stats::runif(n),
    w = stats::runif(n, 0.5, 2)
  )
  d$y <- 1 + as.numeric(d$a) * 0.5 - (d$b == "v") + 0.7 * d$x +
    0.4 * (d$a == "q") * (d$b == "v") + stats::rnorm(n)
  d$yb <- stats::rbinom(n, 1, stats::plogis(-0.3 + 0.8 * d$x +
    0.5 * (d$a == "r") - 0.4 * (d$b == "v")))
  d$yc <- stats::rpois(n, exp(0.2 + 0.4 * d$x + 0.3 * (d$a == "q")))
  d
}

.expect_anova_match <- function(m, typ) {
  ref <- as.data.frame(suppressMessages(car::Anova(m, type = typ)))
  nat <- rmorie:::.tbl_anova_typed(m, typ)
  expect_identical(rownames(nat), rownames(ref))
  expect_identical(names(nat), names(ref))
  expect_identical(is.na(as.matrix(nat)), is.na(as.matrix(ref)))
  expect_equal(as.matrix(nat), as.matrix(ref), tolerance = 1e-8)
}

test_that("Type II / III match car::Anova on lm fits", {
  skip_if_not_installed("car")
  d <- .anova_data()
  models <- list(
    unbalanced_two_way = stats::lm(y ~ a * b, d),
    factor_covariate = stats::lm(y ~ a + x, d),
    interaction_covariate = stats::lm(y ~ a * x + b, d),
    three_way = stats::lm(y ~ a * b * x, d),
    weighted = stats::lm(y ~ a * b, d, weights = w),
    sum_contrasts = stats::lm(y ~ a * b, d,
                              contrasts = list(a = "contr.sum", b = "contr.sum")),
    offset = stats::lm(y ~ a * b + offset(z), d),
    no_intercept = stats::lm(y ~ 0 + a + x, d),
    aov = stats::aov(y ~ a * b, d)
  )
  for (m in models) {
    .expect_anova_match(m, 2)
    .expect_anova_match(m, 3)
  }
  # aliased coefficients: Type II by model comparison, Type III refused
  d$x2 <- 2 * d$x
  m <- stats::lm(y ~ x + x2 + a, d)
  .expect_anova_match(m, 2)
  expect_error(rmorie:::.tbl_anova_typed(m, 3), "aliased")
})

test_that("Type II / III LR tests match car::Anova on glm fits", {
  skip_if_not_installed("car")
  d <- .anova_data()
  models <- list(
    binomial_interaction = stats::glm(yb ~ a * b, stats::binomial(), d),
    binomial_covariate = stats::glm(yb ~ a + x + b, stats::binomial(), d),
    poisson_interaction = stats::glm(yc ~ a * x, stats::poisson(), d),
    poisson_sum_contrasts = stats::glm(yc ~ a + b + x, stats::poisson(), d,
                                       contrasts = list(a = "contr.sum")),
    gaussian = stats::glm(y ~ a * b, stats::gaussian(), d),
    quasibinomial = stats::glm(yb ~ x + a, stats::quasibinomial(), d)
  )
  for (m in models) {
    .expect_anova_match(m, 2)
    .expect_anova_match(m, 3)
  }
})

test_that("anova_table formats native Type II / III tables", {
  d <- .anova_data()
  m <- stats::lm(y ~ a * b, d)
  out2 <- anova_table(m)
  expect_s3_class(out2, "data.frame")
  expect_identical(rownames(out2), c("a", "b", "a:b", "Residuals"))
  expect_true(all(c("Sum Sq", "Df", "F value", "p-value") %in% names(out2)))
  out3 <- anova_table(m, typ = 3L)
  expect_identical(rownames(out3), c("(Intercept)", "a", "b", "a:b", "Residuals"))
  g <- stats::glm(yb ~ a + x, stats::binomial(), d)
  outg <- anova_table(g)
  expect_true("LR Chisq" %in% names(outg))
  # Type II on a main-effects-only lm equals Type I for the last term
  m2 <- stats::lm(y ~ a + x, d)
  t1 <- stats::anova(m2)
  t2 <- rmorie:::.tbl_anova_typed(m2, 2)
  expect_equal(t2["x", "Sum Sq"], t1["x", "Sum Sq"], tolerance = 1e-10)
  expect_error(anova_table(m, typ = 4L), "typ must be")
  expect_warning(rmorie:::.tbl_anova_typed(stats::lm(y ~ 1, d), 2), "Type III")
})
