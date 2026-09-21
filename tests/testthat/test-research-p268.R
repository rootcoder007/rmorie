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

test_that("relative risk sits between 1 and the odds ratio and OR overstates it (rr_between, or_overstates)", {
  set.seed(5)
  for (k in 1:300) {
    a <- runif(1); b <- runif(1)
    OR <- (a / (1 - a)) / (b / (1 - b)); RR <- a / b
    expect_gte(RR, min(1, OR) - 1e-12); expect_lte(RR, max(1, OR) + 1e-12)
    expect_lte(abs(log(RR)), abs(log(OR)) + 1e-12)
    expect_equal(OR, RR * (1 - b) / (1 - a), tolerance = 1e-12)
    r <- morie_relative_risk_from_or(OR, base_rate = 0.3 * a + 0.7 * b, exposed_share = 0.3)
    expect_equal(unname(r$risks["relative_risk"]), RR, tolerance = 1e-6)
    expect_gte((r$overstatement_factor - 1) * log(OR), -1e-12)          # OR/RR = (1-b)/(1-a) exceeds 1 exactly when OR > 1
  }
  expect_equal(morie_relative_risk_from_or(3)$rr_bounds, c(lower = 1, upper = 3))
  expect_equal(morie_relative_risk_from_or(0.25)$rr_bounds, c(lower = 0.25, upper = 1))
  expect_error(morie_relative_risk_from_or(-1), "positive")
})

test_that("optimal offending is non-increasing in certainty for arbitrary benefit shapes (certainty_monotone, aggregate_monotone)", {
  set.seed(9)
  for (k in 1:200) {
    x <- 0:15; benefit <- cumsum(rnorm(16, 0.5)) + rnorm(16, 0, 2)    # non-concave, non-monotone benefit
    sanction <- cumsum(runif(16, 0.1, 2))                               # strictly increasing, any shape
    p <- sort(runif(6, 0, 3))
    r <- morie_deterrence_response(x, benefit, sanction, p)
    expect_true(all(diff(r$x_opt) <= 0))
  }
  # aggregate over heterogeneous offenders is non-increasing too
  agg <- sapply(c(0.2, 0.6, 1.5), function(pp) sum(sapply(1:30, function(i) { set.seed(i); b <- cumsum(rnorm(11, 0.4)); s <- cumsum(runif(11, 0.1, 1.5))
    morie_deterrence_response(0:10, b, s, pp)$x_opt })))
  expect_true(all(diff(agg) <= 0))
  expect_error(morie_deterrence_response(0:2, 1:3, c(1, 1, 2), 0.5), "strictly increasing")
})

test_that("random mixing makes per-offender-group rates load on victim share; pair exposure removes it (rate_per_offender_group, dyad_ratio)", {
  set.seed(10)
  for (i in 1:100) {
    pop <- c(A = runif(1, 1e4, 1e5), B = runif(1, 1e4, 1e5)); N <- sum(pop); p <- pop / N; k <- runif(1, 0.001, 0.01)
    off <- c(A_on_B = k * p["A"] * p["B"] * N, B_on_A = k * p["B"] * p["A"] * N,
             A_on_A = k * p["A"]^2 * N, B_on_B = k * p["B"]^2 * N)
    names(off) <- c("A_on_B", "B_on_A", "A_on_A", "B_on_B")
    r <- morie_interracial_rates(off, pop)
    expect_equal(r$rate_per_offender_group[r$offender == "A" & r$victim == "B"], unname(k * p["B"]), tolerance = 1e-12)
    expect_equal(r$rate_per_offender_group[1] / r$rate_per_offender_group[2], unname(p["B"] / p["A"]), tolerance = 1e-12)
    expect_true(all(abs(r$ratio_to_null - 1) < 1e-10))          # pair exposure: constant k, ratio 1 everywhere
  }
  # a real departure shows up only in the pair-exposure ratio
  r <- morie_interracial_rates(c(A_on_B = 2 * 0.8 * 0.2 * 1e5 * 0.005, B_on_A = 0.2 * 0.8 * 1e5 * 0.005), c(A = 8e4, B = 2e4))
  expect_gt(r$ratio_to_null[1], r$ratio_to_null[2])
  expect_error(morie_interracial_rates(c(AB = 1), c(A = 1, B = 1)), "offender_on_victim")
})

test_that("probability of necessity: Frechet bounds contain every joint, ends attained (necessity_bounds)", {
  set.seed(12)
  for (k in 1:200) {
    n <- 500; a <- runif(1); b <- runif(1)
    pn <- morie_probability_of_necessity(a, b)
    # random joint with marginals near (a, b): copula by a shared uniform
    u <- runif(n); rho <- runif(1, -1, 1)
    A <- u < a; B <- if (rho >= 0) (u < b) else ((1 - u) < b)
    share <- mean(A & !B)
    expect_gte(share, max(0, mean(A) - mean(B)) - 1e-12); expect_lte(share, min(mean(A), 1 - mean(B)) + 1e-12)
    expect_gte(pn$necessary_share_bounds["lower"], 0); expect_lte(pn$necessary_share_bounds["upper"], 1)
  }
  # nested (monotone) events attain a - b; disjoint events attain a
  u <- runif(2000); A <- u < 0.6; B <- u < 0.4
  expect_equal(mean(A & !B), mean(A) - mean(B), tolerance = 1e-12)
  A2 <- u < 0.3; B2 <- u > 0.7
  expect_equal(mean(A2 & !B2), mean(A2), tolerance = 1e-12)
  expect_equal(morie_probability_of_necessity(0.6, 0.4)$pn_monotone, 1 / 3)
  expect_error(morie_probability_of_necessity(1.2, 0.1), "in \\[0, 1\\]")
})

test_that("conditioning on arrest gives odds ratio pi_bg between independent factors (collider_or_eq_background)", {
  set.seed(13)
  for (k in 1:100) {
    a <- runif(1, 0.05, 0.95); e <- runif(1, 0.05, 0.95); pb <- runif(1, 0.01, 0.99)
    r <- morie_collider_arrest(a, e, pb)
    expect_equal(r$arrestee_or, pb, tolerance = 1e-12); expect_lt(r$arrestee_or, 1)
  }
  # simulation of the mechanism
  n <- 400000; A <- rbinom(n, 1, 0.3); E <- rbinom(n, 1, 0.2); U <- rbinom(n, 1, 0.1); Y <- pmax(A, E, U)
  tab <- table(A[Y == 1], E[Y == 1]); or_hat <- (tab[2, 2] * tab[1, 1]) / (tab[2, 1] * tab[1, 2])
  expect_equal(unname(or_hat), 0.1, tolerance = 0.08)
  tab0 <- table(A, E); expect_equal(unname((tab0[2, 2] * tab0[1, 1]) / (tab0[2, 1] * tab0[1, 2])), 1, tolerance = 0.05)
  expect_error(morie_collider_arrest(1, 0.5, 0.5), "in \\(0, 1\\)")
})
