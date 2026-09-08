# Properties come from the definition in Matthews (1975); the
# cross-check is against morie.fn.mcc, which itself matches
# sklearn.metrics.matthews_corrcoef to 1e-12.

test_that("perfect agreement gives +1 and perfect disagreement -1", {
  y <- c(0, 0, 1, 1)
  expect_equal(morie_matthews_corrcoef(y, y), 1, tolerance = 1e-12)
  expect_equal(morie_matthews_corrcoef(y, 1 - y), -1, tolerance = 1e-12)
})

test_that("random labels sit near zero", {
  set.seed(42)
  yt <- rbinom(1000, 1, 0.5)
  yp <- rbinom(1000, 1, 0.5)
  v <- morie_matthews_corrcoef(yt, yp)
  expect_gt(v, -0.15)
  expect_lt(v, 0.15)
})

test_that("MCC is symmetric in its two arguments", {
  # The formula swaps FP and FN when the arguments swap, and the
  # numerator and denominator are both invariant under that swap.
  set.seed(3)
  yt <- rbinom(200, 1, 0.3)
  yp <- rbinom(200, 1, 0.6)
  expect_equal(morie_matthews_corrcoef(yt, yp),
               morie_matthews_corrcoef(yp, yt), tolerance = 1e-12)
})

test_that("labels need not be 0/1", {
  expect_equal(morie_matthews_corrcoef(c(-1, -1, 1, 1), c(-1, -1, 1, 1)),
               1, tolerance = 1e-12)
  expect_equal(morie_matthews_corrcoef(c("n", "n", "p", "p"),
                                       c("n", "n", "p", "p")),
               1, tolerance = 1e-12)
})

test_that("the two entry points agree", {
  set.seed(9)
  yt <- rbinom(300, 1, 0.4)
  yp <- rbinom(300, 1, 0.4)
  counts <- morie_mcc_counts(
    tp = sum(yt == 1 & yp == 1), tn = sum(yt == 0 & yp == 0),
    fp = sum(yt == 0 & yp == 1), fn = sum(yt == 1 & yp == 0)
  )
  expect_equal(morie_matthews_corrcoef(yt, yp), counts$value,
               tolerance = 1e-12)
})

test_that("a zero marginal warns and reports 0", {
  # Everything predicted negative: TP = FP = 0, so two marginals vanish.
  expect_warning(v <- morie_matthews_corrcoef(c(0, 0, 1, 1), c(0, 0, 0, 0)),
                 "undefined")
  expect_equal(v, 0)
})

test_that("morie_matthews_corrcoef validates its inputs", {
  expect_error(morie_matthews_corrcoef(c(0, 1), c(0, 1, 1)), "same length")
  expect_error(morie_matthews_corrcoef(numeric(0), numeric(0)), "not be empty")
  expect_error(morie_matthews_corrcoef(c(0, 1, 2), c(0, 1, 2)), "binary labels only")
  expect_error(morie_mcc_counts(-1, 1, 1, 1), "non-negative")
})
