# R/tables_pub.R, the native Type-II/III ANOVA lines PR #77 added that no other test reached: the
# refusals car::Anova makes too (a non-lm fit, no residual df, an exact fit), a glm stripped of its
# response and prior weights, and a glm term whose columns are aliased -- against car and stats::glm.

.tp <- function(name) get(name, envir = environment(anova_table))

test_that("fits the typed tests cannot take are refused with car's messages", {
  ty <- .tp(".tbl_anova_typed")
  expect_error(ty(structure(list(), class = "nls"), 2L), "Type-II/III tests are implemented for lm, aov and glm fits.",
               fixed = TRUE)
  d <- data.frame(x = 1:6, y = c(2, 1, 4, 3, 6, 5), y2 = c(1, 3, 2, 5, 4, 6))
  expect_error(ty(stats::lm(cbind(y, y2) ~ x, d), 3L), "implemented for lm, aov and glm fits", fixed = TRUE)
  expect_error(ty(stats::lm(y ~ x, d[1:2, ]), 2L), "residual df = 0", fixed = TRUE)
  expect_error(ty(stats::lm(I(2 * x + 1) ~ x, d), 3L), "residual sum of squares is 0 (within rounding error)",
               fixed = TRUE)
  skip_if_not_installed("car")
  expect_error(car::Anova(stats::lm(I(2 * x + 1) ~ x, d), type = 3), "residual sum of squares is 0", fixed = TRUE)
})

test_that("a glm without its stored response and prior weights gives car's likelihood-ratio tests", {
  skip_if_not_installed("car")
  ty <- .tp(".tbl_anova_typed")
  set.seed(5)
  d <- data.frame(x = stats::rnorm(40), g = gl(2, 20))
  d$y <- stats::rpois(40, exp(0.3 + 0.5 * d$x + 0.4 * (d$g == "2")))
  full <- stats::glm(y ~ x + g, family = stats::poisson, data = d)
  bare <- stats::glm(y ~ x + g, family = stats::poisson, data = d, y = FALSE)
  expect_null(bare$y)
  bare$prior.weights <- NULL    # a fit trimmed for storage keeps the model frame, not the weights
  for (typ in 2:3) {
    ref <- as.data.frame(car::Anova(full, type = typ, test.statistic = "LR"))
    got <- ty(bare, typ)
    expect_equal(got[["LR Chisq"]], ref[["LR Chisq"]], tolerance = 1e-8, info = typ)
    expect_equal(got[["Df"]], ref[["Df"]], info = typ)
    expect_equal(got[["Pr(>Chisq)"]], ref[["Pr(>Chisq)"]], tolerance = 1e-8, info = typ)
  }
})

test_that("a Type-II glm term whose columns are aliased has no test, and the others are deviance differences", {
  ty <- .tp(".tbl_anova_typed")
  set.seed(8)
  d <- data.frame(x1 = stats::rnorm(50), z = stats::rnorm(50))
  d$x2 <- 2 * d$x1                       # spans the same column space as x1
  d$y <- stats::rpois(50, exp(0.2 + 0.4 * d$x1 + 0.3 * d$z))
  m <- stats::glm(y ~ x1 + x2 + z, family = stats::poisson, data = d)
  expect_true(is.na(stats::coef(m)[["x2"]]))
  got <- ty(m, 2L)
  # dropping x1 or x2 alone leaves the other, so neither changes the fit
  expect_identical(got[["Df"]], c(0, 0, 1))
  expect_true(all(is.na(got[["LR Chisq"]][1:2])) && all(is.na(got[["Pr(>Chisq)"]][1:2])))
  ref <- stats::deviance(stats::glm(y ~ x1, family = stats::poisson, data = d)) - stats::deviance(m)
  expect_equal(got[["LR Chisq"]][3], ref, tolerance = 1e-8)
  expect_equal(got[["Pr(>Chisq)"]][3], stats::pchisq(ref, 1, lower.tail = FALSE), tolerance = 1e-8)
})
