# SPDX-License-Identifier: AGPL-3.0-or-later
#
# srr PD standards completed by the general distribution object
# (R/distributions.R): general representation, name-parameterised
# operations, analytic vs numeric manipulation, integration + summation.

test_that("PD2.0 a general distribution representation is provided", {
  d <- morie_distribution("normal", mean = 0, sd = 1)
  expect_s3_class(d, "morie_distribution")
  expect_equal(morie_dist_cdf(d, 0), 0.5)
})

test_that("PD3.0 manipulation is analytic where a closed form exists", {
  g <- morie_distribution("gamma", shape = 2, rate = 1)
  # analytic mean = shape/rate = 2; numeric integration agrees
  expect_equal(morie_dist_moment(g, 1, method = "analytic"), 2)
  expect_equal(morie_dist_moment(g, 1, method = "numeric"), 2, tolerance = 1e-4)
})

test_that("PD3.1 operations accept the distribution (by name) as a parameter", {
  for (nm in c("normal", "exponential", "gamma", "poisson", "binomial")) {
    params <- switch(nm,
      normal = list(mean = 1, sd = 2), exponential = list(rate = 0.5),
      gamma = list(shape = 2, rate = 1), poisson = list(lambda = 3),
      binomial = list(size = 10, prob = 0.4))
    d <- do.call(morie_distribution, c(nm, params))
    expect_true(is.numeric(morie_dist_pdf(d, 1)))    # same op, any distribution
  }
})

test_that("PD3.4 integration documents + pre-checks stability conditions", {
  d <- morie_distribution("normal", mean = 0, sd = 1)
  # a proper density integrates to 1 over its support
  expect_equal(morie_dist_integrate(d)$value, 1, tolerance = 1e-5)
  # a non-finite integrand is rejected as unstable
  bad <- function(x) 1 / (x - x)                     # Inf/NaN
  expect_error(morie_dist_integrate(d, integrand = bad), "unstable|finite")
})

test_that("PD3.5/PD3.5a discrete summation is justified + shown to converge", {
  p <- morie_distribution("poisson", lambda = 4)
  # the mass series sums to 1 (finite limit demonstrated)
  expect_equal(morie_dist_sum(p), 1, tolerance = 1e-8)
  # analytic mean (lambda) recovered by summation
  expect_equal(morie_dist_moment(p, 1, method = "numeric"), 4, tolerance = 1e-6)
})
