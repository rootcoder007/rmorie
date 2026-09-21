# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P9: the recording-map functions must agree with research/lean/P9Recording.lean.

test_that("pure re-classification keeps the total and moves the ratio (total_invariant_of_colStochastic, reclassification_moves_ratio)", {
  set.seed(1)
  for (k in 1:100) {
    m <- sample(2:5, 1)
    M <- matrix(runif(m * m), m, m); M <- sweep(M, 2, colSums(M), "/")   # column-stochastic
    cnt <- runif(m, 10, 1000)
    r <- morie_recording_map(M, cnt)
    expect_equal(r$recorded_total, r$true_total, tolerance = 1e-10)
    expect_equal(r$regime, "reclassification")
    expect_equal(r$dropped$total, 0, tolerance = 1e-10)
  }
  q <- 0.3; ci <- 100; cj <- 400
  M <- rbind(c(1, q), c(0, 1 - q))          # share q of category 2 moved into category 1
  r <- morie_recording_map(M, c(ci, cj))
  expect_equal(r$recorded_total, ci + cj)
  expect_equal(unname(r$recorded[1] / r$recorded[2]), ci / cj + q / (1 - q) * (1 + ci / cj), tolerance = 1e-12)
  expect_gte(unname(r$recorded[1] / r$recorded[2]), ci / cj)
})

test_that("cuffing lowers the total by exactly the dropped mass (total_le_of_colSubstochastic)", {
  set.seed(2)
  for (k in 1:100) {
    m <- sample(2:5, 1)
    M <- matrix(runif(m * m), m, m); M <- sweep(M, 2, colSums(M) / runif(m, 0.5, 1), "/")
    cnt <- runif(m, 10, 1000)
    r <- morie_recording_map(M, cnt)
    expect_equal(r$regime, "cuffing")
    expect_lte(r$recorded_total, r$true_total + 1e-10)
    expect_equal(r$recorded_total, r$true_total - r$dropped$total, tolerance = 1e-10)
  }
  expect_error(morie_recording_map(rbind(c(0.8, 0.5), c(0.5, 0.5)), c(1, 1)), "column sums")
})

test_that("downgrade-and-caution raises the detection rate by q n (1 - d) / N (detection_rate_rises)", {
  s <- morie_detection_rate_shift(detected = 2000, total = 10000, n = 1500, d = 0.2, q = 0.5)
  expect_equal(s$rise, 0.5 * 1500 * 0.8 / 10000)
  expect_equal(s$rate_after - s$rate_before, s$rise, tolerance = 1e-12)
  expect_gt(s$rise, 0)
  expect_error(morie_detection_rate_shift(10, 100, 50, 1, 0.5), "d must lie")
})
