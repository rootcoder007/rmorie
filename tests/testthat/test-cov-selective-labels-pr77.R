# R/selective_labels.R, the lines PR #77 added that no other test reached: 0/1 release decisions
# are read as logical, and a missing decision is refused.

test_that("0/1 release columns give the logical answer and NA is refused", {
  set.seed(5)
  n <- 200
  risk <- stats::runif(n)
  y <- stats::rbinom(n, 1, risk)
  lg <- cbind(strict = risk < 0.5, lenient = risk < 0.8)
  num <- lg + 0
  a <- morie_selective_labels_judges(y, num, rule_released = risk < 0.7)
  b <- morie_selective_labels_judges(y, lg, rule_released = risk < 0.7)
  expect_identical(a$judges, b$judges)
  expect_true(a$nested)
  expect_equal(a$judges[[2]], unname(colMeans(lg)))
  num[3, 1] <- NA
  expect_error(morie_selective_labels_judges(y, num, rule_released = risk < 0.7),
               "released must not contain NA", fixed = TRUE)
})
