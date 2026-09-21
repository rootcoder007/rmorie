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

test_that("benchmark disparities multiply, never add, and offsets shift by -log kappa (P2Benchmark)", {
  set.seed(1)
  b <- morie_disparity_benchmark(pop = c(A = 100, B = 100), contact = c(A = 30, B = 10),
                                 force = c(A = 12, B = 2), reference = "B")
  a <- b[b$group == "A", ]
  expect_equal(a$contact_disparity, 3); expect_equal(a$force_given_contact_disparity, 2)
  expect_equal(a$resident_disparity, 6); expect_equal(a$additive_claim, 5)
  expect_equal(max(abs(b$product_check)), 0, tolerance = 1e-12)
  # random tables: the product identity is exact
  for (k in 1:100) {
    pop <- stats::setNames(runif(4, 50, 500), LETTERS[1:4]); con <- pop * runif(4, 0.05, 0.5); frc <- con * runif(4, 0.05, 0.5)
    r <- morie_disparity_benchmark(pop, con, frc, reference = "C")
    expect_equal(r$resident_disparity, r$contact_disparity * r$force_given_contact_disparity, tolerance = 1e-12)
    expect_equal(r$resident_disparity[r$group == "C"], 1)
  }
  # offset shift: a rate model refit with a scaled offset moves the coefficient by exactly -log(kappa)
  d <- data.frame(y = c(12, 2), grp = factor(c("A", "B"), levels = c("B", "A")), E = c(100, 100))
  f1 <- stats::glm(y ~ grp + offset(log(E)), family = stats::poisson, data = d)
  d$E2 <- d$E * c(A = 4, B = 1)[as.character(d$grp)]
  f2 <- stats::glm(y ~ grp + offset(log(E2)), family = stats::poisson, data = d)
  expect_equal(unname(stats::coef(f2)["grpA"] - stats::coef(f1)["grpA"]), -log(4), tolerance = 1e-8)
  bc <- morie_disparity_benchmark(pop = c(A = 100, B = 100), contact = c(A = 30, B = 10), force = c(A = 12, B = 2),
                                  reference = "B", exposure_error_factor = c(A = 4, B = 1))
  expect_equal(bc$log_shift[bc$group == "A"], -log(4))
  expect_equal(bc$resident_disparity_corrected[bc$group == "A"], 6 / 4)
  expect_error(morie_disparity_benchmark(c(A = 1), c(B = 1), c(A = 1), "A"), "same group names")
})
