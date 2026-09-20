# Expectations are computed by hand from the defining formulas, not read off
# the implementation. Each block states the arithmetic it is anchored on.

test_that("cluster design effect is 1 + (m - 1) * icc", {
  expect_equal(morie_deff_cluster(25, 0.02), 1 + 24 * 0.02)
  expect_equal(morie_deff_cluster(1, 0.9), 1)          # no clustering effect at m = 1
  expect_equal(morie_deff_cluster(10, 0), 1)           # no correlation, no loss
  # A negative ICC shrinks the design effect below 1; that is legitimate.
  expect_lt(morie_deff_cluster(5, -0.05), 1)
  expect_error(morie_deff_cluster(0.5, 0.1), "m must be")
  expect_error(morie_deff_cluster(10, -1), "non-positive design effect")
})

test_that("effective sample size divides by the design effect", {
  expect_equal(morie_neff_cluster(500, 25, 0.02), 500 / 1.48)
  expect_equal(morie_neff_cluster(500, 1, 0.5), 500)
})

test_that("proportion sample size matches the closed form", {
  # z = 1.959963985, p = 0.5, e = 0.05 -> n0 = 384.1458...
  r <- morie_sample_size_proportion(0.5, 0.05)
  z <- stats::qnorm(0.975)
  expect_equal(r$n_srs, z^2 * 0.25 / 0.05^2)
  expect_equal(r$n_design, r$n_srs)        # deff 1, N infinite
  expect_equal(r$n_invite, r$n_srs)        # full response
  expect_gt(r$n_srs, 384)
  expect_lt(r$n_srs, 385)
})

test_that("finite population correction, design effect and non-response compose", {
  n0 <- stats::qnorm(0.975)^2 * 0.25 / 0.05^2
  r <- morie_sample_size_proportion(0.5, 0.05, N = 1000, deff = 1.5,
                                    response_rate = 0.6)
  expect_equal(r$n_design, (n0 / (1 + (n0 - 1) / 1000)) * 1.5)
  expect_equal(r$n_invite, r$n_design / 0.6)
  # The correction must reduce, the design effect must inflate.
  expect_lt(r$n_design / 1.5, n0)
  expect_gt(r$n_invite, r$n_design)
})

test_that("a smaller margin of error costs quadratically", {
  a <- morie_sample_size_proportion(0.5, 0.05)$n_srs
  b <- morie_sample_size_proportion(0.5, 0.025)$n_srs
  expect_equal(b / a, 4, tolerance = 1e-9)
})

test_that("p = 0.5 is the conservative maximum", {
  half <- morie_sample_size_proportion(0.5, 0.05)$n_srs
  for (p in c(0.1, 0.3, 0.7, 0.9)) {
    expect_lt(morie_sample_size_proportion(p, 0.05)$n_srs, half)
  }
})

test_that("domain sample size divides by prevalence and coverage", {
  d <- morie_sample_size_domain(0.5, 0.05, domain_prevalence = 0.05)
  base <- morie_sample_size_proportion(0.5, 0.05)
  expect_equal(d$n_domain, base$n_design)
  expect_equal(d$n_overall, base$n_invite / 0.05)
  # Roughly 385 in the subgroup requires roughly 7683 overall.
  expect_gt(d$n_overall, 7600)
  expect_lt(d$n_overall, 7700)
  # Under-coverage of the frame divides a second time.
  d2 <- morie_sample_size_domain(0.5, 0.05, domain_prevalence = 0.05,
                                 coverage = 0.8)
  expect_equal(d2$n_overall, d$n_overall / 0.8)
})

test_that("oversampling factor reaches the target share and prices it", {
  o <- morie_oversample_factor(0.05, 0.20)
  # odds(0.2)/odds(0.05) = 0.25 / (1/19) = 4.75
  expect_equal(o$factor, 4.75)
  expect_equal(o$weight_ratio, 1 / 4.75)
  # Unequal weights must cost something.
  expect_gt(o$deff_weights, 1)
  # No oversampling when the target equals the natural share.
  flat <- morie_oversample_factor(0.2, 0.2)
  expect_equal(flat$factor, 1)
  expect_equal(flat$deff_weights, 1)
})

test_that("screening design counts screens and splits cost", {
  s <- morie_screen_design(0.05, 400, cost_screen = 5, cost_interview = 120)
  expect_equal(s$n_eligible, 400)
  expect_equal(s$n_screen, 8000)
  expect_equal(s$cost_total, 8000 * 5 + 400 * 120)
  expect_equal(s$cost_per_completed_interview, (40000 + 48000) / 400)
  expect_equal(s$screening_share_of_cost, 40000 / 88000)
  # Response rates inflate both stages.
  s2 <- morie_screen_design(0.05, 400, screen_response = 0.5,
                            interview_response = 0.8)
  expect_equal(s2$n_eligible, 500)
  expect_equal(s2$n_screen, 500 / (0.05 * 0.5))
})

test_that("optimal allocation reduces to Neyman when costs are equal", {
  N <- c(8000, 1500, 500); S <- c(1, 1.4, 2)
  a <- morie_alloc_optimal(N, S, 900)
  expect_equal(a$share, N * S / sum(N * S))
  expect_equal(sum(a$n_h), 900)
  expect_equal(sum(a$n_h_int), 900L)
  # Costlier strata get fewer units, all else equal.
  b <- morie_alloc_optimal(N, S, 900, cost_h = c(1, 3, 9))
  expect_lt(b$share[3], a$share[3])
  expect_gt(b$share[1], a$share[1])
  # Doubling every cost changes nothing: only relative cost matters.
  c2 <- morie_alloc_optimal(N, S, 900, cost_h = c(2, 6, 18))
  expect_equal(c2$share, b$share)
})

test_that("raking hits both margins", {
  df <- data.frame(region = c("north", "north", "south", "south"),
                   group = c("a", "b", "a", "b"))
  r <- morie_rake(df, list(region = c(north = 60, south = 40),
                           group = c(a = 70, b = 30)))
  expect_true(r$converged)
  expect_equal(sum(r$weights[df$region == "north"]), 60, tolerance = 1e-6)
  expect_equal(sum(r$weights[df$group == "a"]), 70, tolerance = 1e-6)
  expect_equal(sum(r$weights), 100, tolerance = 1e-6)
})

test_that("raking rejects inconsistent margins and unknown levels", {
  df <- data.frame(region = c("north", "south"), group = c("a", "b"))
  expect_error(
    morie_rake(df, list(region = c(north = 60, south = 40),
                        group = c(a = 70, b = 40))),
    "same population total")
  expect_error(
    morie_rake(df, list(region = c(north = 100))),
    "levels absent from its margin")
})

test_that("Rogan-Gladen inverts the misclassification", {
  # Perfect classifier returns the observed value unchanged.
  expect_equal(morie_misclass_correct(0.04, 1, 1)$p_corrected, 0.04)
  # sens 0.75, spec 0.999 -> (0.04 + 0.999 - 1) / 0.749
  r <- morie_misclass_correct(0.04, 0.75, 0.999)
  expect_equal(r$p_corrected, (0.04 + 0.999 - 1) / 0.749)
  expect_gt(r$p_corrected, 0.04)          # imperfect sensitivity undercounts
  expect_equal(r$youden, 0.749)
  # Round trip: classify a known truth, then recover it.
  p_true <- 0.06; sens <- 0.8; spec <- 0.99
  p_obs <- p_true * sens + (1 - p_true) * (1 - spec)
  expect_equal(morie_misclass_correct(p_obs, sens, spec)$p_corrected, p_true)
})

test_that("an uninformative classifier is refused", {
  expect_error(morie_misclass_correct(0.04, 0.5, 0.5), "carries no information")
})

test_that("misclassification interval widens as the classifier degrades", {
  good <- morie_misclass_correct(0.04, 0.95, 0.999, n = 10000)
  poor <- morie_misclass_correct(0.04, 0.60, 0.999, n = 10000)
  expect_lt(diff(good$ci), diff(poor$ci))
  expect_true(good$ci[1] < good$p_corrected && good$p_corrected < good$ci[2])
})

test_that("count adjustment reports the undercount", {
  r <- morie_misclass_count(400, 10000, 0.75, 0.999)
  p <- morie_misclass_correct(0.04, 0.75, 0.999)$p_corrected
  expect_equal(r$count_corrected, p * 10000)
  expect_equal(r$undercount, p * 10000 - 400)
  expect_gt(r$undercount, 0)
  expect_error(morie_misclass_count(400, 100, 0.9, 0.9), "at least as large")
})
