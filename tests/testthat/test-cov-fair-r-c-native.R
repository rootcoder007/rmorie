# Coverage for ranked-output fairness (Yang & Stoyanovich 2017): rND,
# rKL and rRD as 1/log2(i)-discounted sums over the top-10, top-20, ...
# prefixes, normalised by the value of the ranking with every protected
# item at the bottom, and the dispatcher.

.raw <- function(p, f) {
  N <- length(p)
  P <- mean(p)
  sum(vapply(seq(10, N, 10), function(i) f(p[1:i], p, P) / log2(i), 1))
}

test_that("rND, rKL and rRD recompute from their prefix definitions", {
  p <- c(rep(0, 12), 1, 0, 1, rep(0, 5), 1, 1, 0, 1, 1, 0, 1, 0, 0, 1)
  worst <- c(rep(0, 30 - sum(p)), rep(1, sum(p)))
  nd <- function(q, p, P) abs(mean(q) - P)
  kl <- function(q, p, P) {
    a <- min(max(mean(q), 1e-12), 1 - 1e-12)
    a * log(a / P) + (1 - a) * log((1 - a) / (1 - P))
  }
  rd <- function(q, p, P) {
    r1 <- if (sum(q) == 0 || sum(q) == length(q)) 0 else sum(q) / sum(1 - q)
    abs(r1 - sum(p) / sum(1 - p))
  }
  r <- rND(p)
  expect_equal(r$raw, .raw(p, nd), tolerance = 1e-12)
  expect_equal(r$value, .raw(p, nd) / .raw(worst, nd), tolerance = 1e-12)
  expect_equal(r$cutoffs, c(10, 20, 30))
  expect_equal(rKL(p)$value, .raw(p, kl) / .raw(worst, kl), tolerance = 1e-12)
  expect_equal(rRD(p)$value, .raw(p, rd) / .raw(worst, rd), tolerance = 1e-12)
  expect_equal(rND(p, normalize = FALSE)$value, .raw(p, nd), tolerance = 1e-12)
  expect_equal(rND(worst)$value, 1, tolerance = 1e-12)
  expect_equal(normalizer(p), .raw(worst, nd), tolerance = 1e-12)
  expect_equal(cutoffs(25, 5), seq(5, 25, 5))
  expect_error(cutoffs(5), "shorter than the first cut-off 10")
})

test_that("guards, the majority caveat and the dispatcher", {
  p <- rep(c(1, 0), 10)
  expect_match(rRD(c(rep(0, 6), rep(1, 14)))$caveat, "NOT APPLICABLE")
  expect_null(rRD(p)$caveat)
  expect_identical(morie_fairRC(p, "rKL"), rKL(p))
  expect_identical(morie_fairRC(p, "rRD", step = 5), rRD(p, step = 5))
  expect_identical(fairranking, rND)
  expect_identical(fairness_rec, rND)
  expect_identical(fairnessrec, rND)
  expect_error(morie_fairRC(p, "rAUC"), "measure must be one of")
  expect_error(rND(rep(1, 20)), "every item is in one group")
  expect_error(rND(integer(0)), "empty")
})
