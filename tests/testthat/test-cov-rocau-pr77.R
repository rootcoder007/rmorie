# R/rocau.R, the lines PR #77 added that no other test reached: a sample with no cases, and a
# threshold midpoint that rounds onto an observed score (adjacent doubles) and is moved up to the
# next score, as pROC does.

test_that("a sample without cases is refused", {
  expect_error(morie_roc_auc_score(c(0, 0, 1, 1), c(0.1, 0.2, NA, NA)),
               "morie_roc_auc_score: no control or no case observations", fixed = TRUE)
})

test_that("a midpoint between adjacent doubles is nudged as pROC nudges it", {
  skip_if_not_installed("pROC")
  co <- c(1, 3)
  ca <- c(1 + 2^-52, 4)
  expect_identical((1 + (1 + 2^-52)) / 2, 1)  # the midpoint rounds onto the lower score
  r <- morie_roc_auc_score(c(0, 0, 1, 1), c(co, ca))
  ref <- pROC::roc(controls = co, cases = ca, direction = "<", quiet = TRUE)
  expect_identical(sort(r$thresholds), sort(ref$thresholds))
  expect_true((1 + 2^-52) %in% r$thresholds)
  expect_equal(r$auc, as.numeric(ref$auc))
  expect_equal(r$auc, mean(outer(ca, co, ">") + 0.5 * outer(ca, co, "==")))
})
