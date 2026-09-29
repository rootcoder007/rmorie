# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tlhaltm_native.R (HAL-TMLE efficiency conditions, van
# der Laan & Rose 2018 Chap. 7): the rate condition a + b > 1/2, the
# second-order remainder err_Q err_g / delta, the efficiency verdict
# and the cross-validation split.

test_that("rate_condition adds the two exponents against the 1/2 boundary", {
  r <- rate_condition(1 / 3, 1 / 3, 100)
  expect_equal(r$sum, 2 / 3, tolerance = 1e-12)
  expect_equal(r$required, 0.5)
  expect_true(r$satisfied)
  expect_equal(r$product_order, 100^(-2 / 3), tolerance = 1e-12)
  expect_equal(r$root_n_order, 0.1, tolerance = 1e-12)
  # two estimators at exactly n^{-1/4} sit ON the boundary
  b <- rate_condition(0.25, 0.25, 100)
  expect_equal(b$sum, 0.5)
  expect_false(b$satisfied)
  expect_equal(b$product_order, b$root_n_order, tolerance = 1e-12)
  expect_error(rate_condition(0, 0.5, 10), "positive exponents")
})

test_that("remainder_bound is the product of the errors over delta", {
  r <- remainder_bound(0.02, 0.03, 0.05)
  expect_equal(r$bound, 0.02 * 0.03 / 0.05, tolerance = 1e-12)
  expect_equal(r$delta, 0.05)
  # the bound diverges as delta shrinks with the errors fixed
  expect_gt(remainder_bound(0.02, 0.03, 1e-4)$bound, r$bound)
  expect_equal(remainder_bound(0.02, 0.03, 1)$bound, 6e-4, tolerance = 1e-12)
  expect_error(remainder_bound(0.1, 0.1, 0), "must lie in")
  expect_error(remainder_bound(0.1, 0.1, 1.5), "must lie in")
})

test_that("efficiency_check needs a negligible remainder AND a Donsker class", {
  e <- efficiency_check(0.02, 0.03, 0.5, 400)
  expect_equal(e$remainder_bound, 0.02 * 0.03 / 0.5, tolerance = 1e-12)
  expect_equal(e$root_n, 0.05)
  expect_true(e$remainder_negligible)
  expect_true(e$efficient)
  expect_false(efficiency_check(0.02, 0.03, 0.5, 400, donsker = FALSE)$efficient)
  big <- efficiency_check(0.3, 0.3, 0.01, 400)
  expect_false(big$remainder_negligible)
  expect_false(big$efficient)
  expect_equal(e$estimate, e$remainder_bound)
})

test_that("cv_tmle_split makes V disjoint folds covering every index", {
  s <- cv_tmle_split(20, V = 4, seed = 3)
  expect_length(s$folds, 4L)
  expect_setequal(unlist(s$folds), 1:20)
  expect_equal(sum(lengths(s$folds)), 20L)
  for (v in 1:4) expect_setequal(s$training[[v]], setdiff(1:20, s$folds[[v]]))
  # the same shuffle, reproduced from the package stream
  e <- .ghc_rng(3)
  idx <- 1:20
  for (i in 20:2) {
    j <- as.integer(.ghc_unif(e, 1L) * (i + 1)) %% (i + 1)
    if (j == 0L) j <- 1L
    if (j == i) j <- i - 1L
    idx[c(i, j)] <- idx[c(j, i)]
  }
  expect_identical(s$folds[[1]], sort(idx[seq(1, 20, by = 4)]))
  expect_length(cv_tmle_split(10, V = 10)$folds, 10L)
  expect_error(cv_tmle_split(10, V = 1), "V must lie in 2..10")
  expect_error(cv_tmle_split(10, V = 11), "V must lie in 2..10")
})

test_that("morie_tlhaltm dispatches on mode", {
  expect_true(morie_tlhaltm(1 / 3, 1 / 3, 100)$satisfied)
  expect_equal(morie_tlhaltm(err_Q = 0.02, err_g = 0.03, delta = 0.5, mode = "remainder")$bound,
               0.0012, tolerance = 1e-12)
  expect_true(morie_tlhaltm(err_Q = 0.02, err_g = 0.03, delta = 0.5, n = 400,
                            mode = "efficiency")$efficient)
  expect_length(morie_tlhaltm(n = 20, mode = "split")$folds, 10L)
  expect_error(morie_tlhaltm(mode = "donsker"))
})
