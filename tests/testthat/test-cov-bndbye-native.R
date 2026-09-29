# Coverage for Moon & Schorfheide (2012) Bayesian vs frequentist
# inference under set identification: the identified interval, the
# uniform conditional prior on an n-point grid, the HPD set of a
# conditional prior (smallest mass-ordered grid set reaching the level),
# the frequentist confidence set for the parameter or the set, and the
# comparison wrapper.

test_that("identified set and uniform conditional prior", {
  ts <- morie_identified_set_interval(1.5, 0.5)
  expect_identical(c(ts$lower, ts$upper, ts$width), c(1, 2, 1))
  expect_error(morie_identified_set_interval(0, -1), "non-negative")
  cp <- morie_conditional_prior_uniform(ts, n_grid = 5)
  expect_equal(cp$grid, seq(1, 2, length.out = 5))
  expect_equal(cp$density, rep(1, 5))
  expect_identical(morie_conditional_prior_uniform(list(lower = 2, upper = 2))$grid, 2)
  expect_error(morie_conditional_prior_uniform(list(lower = 2, upper = 1)), "empty")
})

test_that("the HPD set collects the densest grid points up to the level", {
  ts <- morie_identified_set_interval(0, 1)
  g <- seq(-1, 1, length.out = 21)
  d <- stats::dnorm(g, 0.3, 0.3)
  h <- morie_posterior_hpd(ts, level = 0.8, conditional_prior = list(grid = g, density = d))
  m <- d / sum(d)
  o <- order(-d)
  k <- which(cumsum(m[o]) >= 0.8)[1]
  expect_equal(c(h$lower, h$upper), range(g[o[1:k]]))
  expect_equal(h$covered, sum(m[o[1:k]]), tolerance = 1e-12)
  u <- morie_posterior_hpd(ts, level = 0.5, n_grid = 11)
  # a flat prior is tied everywhere: the HPD takes the leftmost points
  expect_equal(c(u$lower, u$upper), c(-1, -1 + 0.2 * 5))
  expect_identical(morie_posterior_hpd(list(lower = 1, upper = 1))$width, 0)
  expect_error(morie_posterior_hpd(ts, level = 1), "level must lie")
  expect_error(morie_posterior_hpd(ts, conditional_prior = list(grid = 1:3, density = 1:2)), "differ in length")
  expect_error(morie_posterior_hpd(ts, conditional_prior = list(grid = 1:3, density = c(0, 0, 0))), "no mass")
})

test_that("frequentist sets widen the identified set by c * se", {
  ts <- morie_identified_set_interval(0, 1)
  p <- morie_frequentist_confidence_set(ts, 0.2, level = 0.9)
  expect_equal(p$critical_value, stats::qnorm(0.9))
  expect_equal(c(p$lower, p$upper), c(-1, 1) + c(-1, 1) * stats::qnorm(0.9) * 0.2, tolerance = 1e-12)
  s <- morie_frequentist_confidence_set(ts, 0.2, level = 0.9, target = "set")
  expect_equal(s$critical_value, stats::qnorm(0.95))
  expect_equal(s$width, 2 + 2 * stats::qnorm(0.95) * 0.2, tolerance = 1e-12)
  expect_error(morie_frequentist_confidence_set(ts, -1), "non-negative")
  expect_error(morie_frequentist_confidence_set(ts, 1, target = "point"), "parameter or set")
  expect_error(morie_frequentist_confidence_set(ts, 1, level = 0), "level must lie")
})

test_that("the comparison nests HPD inside Theta inside the confidence set", {
  r <- morie_compare_sets(2, 0.5, 0.1, level = 0.9, n_grid = 101)
  expect_true(r$hpd_inside_identified_set)
  expect_true(r$cs_contains_identified_set)
  expect_equal(r$estimate, r$credible_hpd$width / r$confidence_set$width, tolerance = 1e-12)
  expect_equal(r$credible_hpd$lower, 1.5)
  expect_false(r$conditional_prior_reported)
  expect_identical(morie_bndbye, morie_compare_sets)
  expect_identical(morie_bayescrediblebound, morie_compare_sets)
  expect_identical(morie_bound_bayes_credible, morie_compare_sets)
})
