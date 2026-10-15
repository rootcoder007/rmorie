# R/weightit_native.R, the line PR #77 added that no other test reached: a factor treatment with a
# single level is refused (WeightIt refuses it too).

test_that("a one-level factor treatment is refused", {
  d <- data.frame(a = factor(rep("t", 20)), x = stats::rnorm(20))
  expect_error(morie_weightit(d, "a", "x"), "treatment has one level", fixed = TRUE)
  expect_error(morie_weightit(transform(d, a = rep(TRUE, 20)), "a", "x"), "treatment has one level", fixed = TRUE)
})
