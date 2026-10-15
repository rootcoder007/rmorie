# R/matching.R, the lines PR #77 added that no other test reached: gbm propensities on a sample
# too small for a second split (every child would hold fewer than 2 * n.minobsinnode rows, so a
# tree stops early), against gbm::gbm, and the refusal of a treatment that is not 0/1.

test_that("a tree that cannot split again stops early and still matches gbm", {
  skip_if_not_installed("gbm")
  set.seed(4)
  d <- data.frame(x1 = stats::rnorm(30), x2 = stats::runif(30))
  d$d <- stats::rbinom(30, 1, stats::plogis(d$x1))
  a <- morie_matching_estimate_propensity(d, "d", c("x1", "x2"), model = "gbm")
  fit <- gbm::gbm(d ~ x1 + x2, data = d, distribution = "bernoulli", n.trees = 100,
                  interaction.depth = 3, shrinkage = 0.1, bag.fraction = 1, verbose = FALSE)
  b <- gbm::predict.gbm(fit, newdata = d, n.trees = 100, type = "response")
  expect_equal(unname(a), as.numeric(b), tolerance = 1e-8)
})

test_that("gbm refuses a treatment that is not 0/1 with both values", {
  d <- data.frame(t = rep(c(0, 2), 15), x = stats::rnorm(30))
  msg <- "model = \"gbm\" needs a 0/1 treatment with both values present."
  expect_error(morie_matching_estimate_propensity(d, "t", "x", model = "gbm"), msg, fixed = TRUE)
  d$t <- 1
  expect_error(morie_matching_estimate_propensity(d, "t", "x", model = "gbm"), msg, fixed = TRUE)
})
