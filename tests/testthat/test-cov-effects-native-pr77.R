# R/effects_native.R, the line PR #77 added that no other test reached: newdata whose types cannot
# rebuild the fitted design (a numeric predictor given as a factor) is refused, as predict.lm
# refuses it.

test_that("newdata that cannot rebuild the coefficient columns is refused", {
  fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
  nd <- data.frame(wt = factor(c("a", "b")), hp = c(100, 150))
  expect_error(morie_effects_predictions(fit, newdata = nd),
               "could not rebuild the design matrix for columns: wt", fixed = TRUE)
  # the same refusal from the design builder itself
  expect_error(.mfx_design(.mfx_setup(fit, "test"), nd), "could not rebuild the design matrix for columns: wt",
               fixed = TRUE)
  expect_error(stats::predict(fit, newdata = nd), "was fitted with type")
  # the same columns with the fitted types rebuild and predict as predict.lm does
  ok <- data.frame(wt = c(2.5, 3), hp = c(100, 150))
  expect_equal(morie_effects_predictions(fit, newdata = ok)$estimate, unname(stats::predict(fit, ok)),
               tolerance = 1e-12)
})
