# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P2, P6, P8 R functions against research/lean/{P2Selection,P6AgeCrime,P8Deterrence}.lean.

set.seed(1)

test_that("exposure bounds contain the true rate and the ends are attained (rate_bounds)", {
  set.seed(1)
  y <- 300; m <- 1000; gamma <- 1.5
  b <- morie_disparity_exposure_bounds(c(A = y, B = 100), c(A = m, B = 1000), gamma, "B")
  a <- b[b$group == "A", ]
  for (e in c(m / gamma, m, gamma * m, stats::runif(20, m / gamma, gamma * m))) {
    expect_lte(a$rate_lower, y / e + 1e-12)
    expect_gte(a$rate_upper, y / e - 1e-12)
  }
  expect_equal(a$rate_lower, y / (gamma * m))
  expect_equal(a$rate_upper, y / (m / gamma))
})

test_that("disparity ratio bounds are gamma-squared wide and the sign rule holds", {
  set.seed(1)
  b <- morie_disparity_exposure_bounds(c(A = 300, B = 100), c(A = 1000, B = 1000), 1.5, "B")
  a <- b[b$group == "A", ]
  expect_equal(a$ratio_proxy, 3)
  expect_equal(a$ratio_lower, 3 / 2.25)
  expect_equal(a$ratio_upper, 3 * 2.25)
  expect_true(a$direction_identified)          # 3 > 1.5^2
  b2 <- morie_disparity_exposure_bounds(c(A = 200, B = 100), c(A = 1000, B = 1000), 1.5, "B")
  expect_false(b2[b2$group == "A", "direction_identified"])   # 2 < 2.25
  # any admissible exposure pair keeps the true ratio inside
  for (i in 1:100) {
    eA <- stats::runif(1, 1000 / 1.5, 1500); eB <- stats::runif(1, 1000 / 1.5, 1500)
    true_ratio <- (300 / eA) / (100 / eB)
    expect_lte(a$ratio_lower, true_ratio + 1e-12)
    expect_gte(a$ratio_upper, true_ratio - 1e-12)
  }
})

test_that("a two-type mixture and its one-type average share an aggregate (aggregate_not_identifying)", {
  set.seed(1)
  ages <- 10:60
  early <- stats::dgamma(ages - 9, shape = 3, rate = 0.5) * 40
  late <- stats::dgamma(ages - 9, shape = 8, rate = 0.35) * 40
  two <- morie_age_crime_aggregate(c(0.5, 0.5), cbind(early = early, late = late), ages)
  one <- morie_age_crime_aggregate(1, cbind(avg = attr(two, "equivalent_single_type")), ages)
  expect_equal(one$aggregate, two$aggregate, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(two$early, two$late)))
  expect_error(morie_age_crime_aggregate(c(0.6, 0.6), cbind(early, late)), "sum to 1")
})

test_that("a design that holds a dimension fixed cannot identify it (constant_dimension_not_identified)", {
  set.seed(1)
  d <- morie_deterrence_design_check(p = c(0.1, 0.3, 0.5), s = c(2, 2, 2), c = c(30, 30, 30))
  expect_equal(d$rank, 2L)
  expect_true(d$identified[["certainty"]])
  expect_false(d$identified[["severity"]])
  expect_false(d$identified[["celerity"]])
  full <- morie_deterrence_design_check(p = c(0, 1, 0, 0), s = c(0, 0, 1, 0), c = c(0, 0, 0, 1))
  expect_equal(full$rank, 4L)
  expect_true(all(full$identified))
  # collinear severity and celerity: neither identified even though both vary
  col <- morie_deterrence_design_check(p = c(0, 1, 0, 1), s = c(1, 2, 3, 4), c = c(2, 4, 6, 8))
  expect_false(col$identified[["severity"]])
  expect_false(col$identified[["celerity"]])
})
