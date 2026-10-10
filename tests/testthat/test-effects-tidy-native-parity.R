# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie_effects_tidy() is native-only; broom::tidy() is the reference.

tidy_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

tidy_data <- function(n = 120, seed = 1) {
  set.seed(seed)
  d <- data.frame(x = rnorm(n), z = runif(n),
                  g = factor(sample(c("a", "b", "c"), n, TRUE)))
  d$y <- 1 + 0.5 * d$x - d$z + (d$g == "b") + rnorm(n)
  d$yb <- rbinom(n, 1, stats::plogis(-0.3 + d$x))
  d$yc <- rpois(n, exp(0.2 + 0.3 * d$x))
  d$x2 <- 2 * d$x # aliased with x
  d
}

test_that("morie_effects_tidy matches broom::tidy for lm and glm", {
  skip_if_not_installed("broom")
  d <- tidy_data()
  fits <- list(
    stats::lm(y ~ x + z + g, data = d),
    stats::lm(y ~ x + x2 + g, data = d), # aliased coefficient -> NA row
    stats::glm(yb ~ x + g, family = stats::binomial(), data = d),
    stats::glm(yc ~ x + z, family = stats::poisson(), data = d),
    stats::glm(y ~ x + g, family = stats::gaussian(), data = d)
  )
  for (f in fits) {
    nat <- morie_effects_tidy(f)
    ref <- tidy_df(broom::tidy(f))
    expect_s3_class(nat, "data.frame")
    expect_identical(names(nat), names(ref))
    expect_identical(nat$term, ref$term)
    # Both read summary()$coefficients, so values are identical.
    expect_identical(nat[-1], ref[-1])
  }
})

test_that("morie_effects_tidy conf.int / exponentiate match broom::tidy", {
  skip_if_not_installed("broom")
  d <- tidy_data(seed = 2)
  f_lm <- stats::lm(y ~ x + g, data = d)
  nat <- morie_effects_tidy(f_lm, conf.int = TRUE, conf.level = 0.9)
  ref <- tidy_df(broom::tidy(f_lm, conf.int = TRUE, conf.level = 0.9))
  expect_identical(names(nat), names(ref))
  expect_equal(nat[-1], ref[-1], tolerance = 1e-12)

  skip_if_not_installed("MASS") # profile-likelihood confint.glm (R < 4.4)
  f_glm <- stats::glm(yb ~ x, family = stats::binomial(), data = d)
  nat <- morie_effects_tidy(f_glm, conf.int = TRUE, exponentiate = TRUE)
  ref <- tidy_df(broom::tidy(f_glm, conf.int = TRUE, exponentiate = TRUE))
  expect_identical(names(nat), names(ref))
  # same stats::confint() call; tolerance only covers platform rounding
  expect_equal(nat[-1], ref[-1], tolerance = 1e-10)
})

test_that("morie_effects_tidy needs no broom and refuses untidyable input", {
  d <- tidy_data(seed = 3)
  f <- stats::lm(y ~ x, data = d)
  out <- morie_effects_tidy(f)
  expect_named(out, c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(out$estimate, unname(stats::coef(f)))
  expect_error(morie_effects_tidy(list(a = 1)), "cannot tidy")
})
