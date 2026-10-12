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

test_that("one series, two regimes; a second source separates them (regimes_not_identified, second_source_separates)", {
  set.seed(3)
  for (k in 1:50) {
    m <- sample(2:6, 1)
    r <- runif(m, 0, 500)
    eps <- runif(1, 0.05, 0.9)
    two <- morie_recording_regimes(r, cuffed_share = eps)
    # both constructions reproduce the series exactly
    expect_equal(unname(as.numeric(two$reclassification$M %*% two$reclassification$counts)), r, tolerance = 1e-12)
    expect_equal(unname(as.numeric(two$cuffing$M %*% two$cuffing$counts)), r, tolerance = 1e-12)
    expect_equal(colSums(two$reclassification$M), rep(1, m))
    expect_true(all(colSums(two$cuffing$M) < 1))
    expect_equal(two$reclassification$true_total, sum(r))
    expect_equal(two$cuffing$true_total, sum(r) / (1 - eps), tolerance = 1e-12)
    expect_gt(two$cuffing$true_total, two$reclassification$true_total)
    expect_false(two$identified_from_series)
    # the second source: re-classification keeps the total, cuffing records strictly less
    expect_equal(morie_recording_regimes(r, eps, true_total = sum(r))$second_source$regime, "reclassification")
    sec <- morie_recording_regimes(r, eps, true_total = two$cuffing$true_total)$second_source
    expect_equal(sec$regime, "cuffing")
    expect_equal(sec$dropped_mass, two$cuffing$true_total - sum(r), tolerance = 1e-10)
    expect_equal(morie_recording_regimes(r, eps, true_total = sum(r) / 2)$second_source$regime, "inconsistent")
  }
  expect_error(morie_recording_regimes(c(1, -1)), "non-negative")
  expect_error(morie_recording_regimes(c(1, 2), cuffed_share = 1), "cuffed_share")
  expect_error(morie_recording_regimes(c(1, 2), true_total = -1), "true_total")
})
