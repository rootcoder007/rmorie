# Coverage tests for R/inlasm_native.R (Rue, Martino and Chopin 2009): the
# Newton Gaussian approximation, the skewness diagnostic, grid Laplace
# marginals, the hyperparameter design and the mixture over theta.

trap <- function(x, f) sum(diff(x) * (head(f, -1) + tail(f, -1)) / 2)

test_that("the Gaussian approximation is exact for a Gaussian likelihood", {
  y <- 1.3
  s2 <- 0.5
  ga <- gaussian_approximation(function(x) -(y - x)^2 / (2 * s2), function(x) (y - x) / s2, function(x) -1 / s2, prior_mean = -0.4, prior_precision = 2)
  prec <- 2 + 1 / s2
  expect_equal(ga$mode, (2 * -0.4 + y / s2) / prec, tolerance = 1e-12)
  expect_equal(ga$precision, prec)
  expect_equal(ga$sd, 1 / sqrt(prec))
  z <- integrate(function(x) exp(-(y - x)^2 / (2 * s2) - (x + 0.4)^2), -Inf, Inf, rel.tol = 1e-12)$value
  expect_equal(ga$log_norm, log(z), tolerance = 1e-9)
  # Poisson likelihood with a log link: the mode solves the score equation
  cnt <- 4
  po <- gaussian_approximation(function(x) cnt * x - exp(x), function(x) cnt - exp(x), function(x) -exp(x), 0, 1)
  expect_equal(cnt - exp(po$mode) - po$mode, 0, tolerance = 1e-12)
  expect_equal(po$precision, 1 + exp(po$mode), tolerance = 1e-12)
  expect_error(gaussian_approximation(identity, identity, function(x) 5, 0, 1), "not locally concave")
  expect_error(gaussian_approximation(identity, identity, identity, 0, 0), "precision must be positive")
})

test_that("skewness correction", {
  s <- skewness_correction(0.6, 4)
  expect_equal(s$skewness, 0.6 / 8)
  expect_false(s$gaussian_adequate)
  expect_true(skewness_correction(0, 2)$gaussian_adequate)
  expect_error(skewness_correction(1, 0), "precision must be positive")
})

test_that("grid Laplace marginal normalises by the trapezoid rule", {
  xs <- seq(-9, 11, length.out = 4001)
  lm <- laplace_marginal(function(x, th) dnorm(x, th, 1.5, log = TRUE) + 100, xs, 1)
  w <- dnorm(xs, 1, 1.5)
  expect_equal(lm$density, w / trap(xs, w), tolerance = 1e-12)
  expect_equal(lm$mean, trap(xs, xs * lm$density), tolerance = 1e-12)
  expect_equal(lm$sd, sqrt(trap(xs, (xs - lm$mean)^2 * lm$density)), tolerance = 1e-12)
  # the grid is wide and fine, so the moments are those of N(1, 1.5^2) to 1e-5
  expect_equal(c(lm$mean, lm$sd), c(1, 1.5), tolerance = 1e-5)
  expect_equal(lm$log_scale, max(dnorm(xs, 1, 1.5, log = TRUE)) + 100)
  expect_error(laplace_marginal(function(x, th) 0, 1, 0), "at least 2")
  expect_error(laplace_marginal(function(x, th) 0, c(0, 1e-13), 0), "no mass")
})

test_that("hyperparameter design: the mode plus one step either side", {
  d <- hyperparameter_design(c(0.5, -1), c(4, 0), step = 2)
  expect_equal(d$n_points, 5L)
  pts <- t(vapply(d$points, unlist, numeric(2)))
  expect_equal(pts, rbind(c(0.5, -1), c(-0.5, -1), c(1.5, -1), c(0.5, -3), c(0.5, 1)))
  expect_equal(hyperparameter_design(1, 1)$n_points, 3L)
  expect_error(hyperparameter_design(1:2, c(1, 1), dim = 3), "has 2 entries but dim is 3")
  expect_error(hyperparameter_design(1:7, rep(1, 7)), "7 hyperparameters")
})

test_that("integrate_marginals mixes the conditionals by normalised weights", {
  xs <- seq(-8, 10, length.out = 1801)
  M <- list(dnorm(xs, 0, 1), dnorm(xs, 2, 1.2), dnorm(xs, 1, 0.8))
  lw <- c(0, log(2), -1)
  r <- integrate_marginals(M, lw, xs)
  w <- exp(lw) / sum(exp(lw))
  mix <- w[1] * M[[1]] + w[2] * M[[2]] + w[3] * M[[3]]
  expect_equal(r$theta_weights, w, tolerance = 1e-12)
  expect_equal(r$density, mix / trap(xs, mix), tolerance = 1e-12)
  expect_equal(r$mean, trap(xs, xs * r$density), tolerance = 1e-12)
  expect_equal(r$mean, sum(w * c(0, 2, 1)), tolerance = 1e-5)
  expect_equal(r$sd, sqrt(trap(xs, (xs - r$mean)^2 * r$density)), tolerance = 1e-12)
  expect_equal(r$n_theta, 3L)
  z <- integrate_marginals(list(rep(0, 3)), 0, 1:3)
  expect_equal(z$density, rep(0, 3))
  expect_identical(inla, integrate_marginals)
  expect_identical(inla_spatial, integrate_marginals)
  expect_identical(inlaspatial, integrate_marginals)
  expect_same_function(morie_inlasm$laplace_marginal, laplace_marginal)
  expect_match(morie_inlasm$cheatsheet(), "LAPLACE")
  expect_error(integrate_marginals(M, lw[1:2], xs), "3 conditional marginals but 2 weights")
  expect_error(integrate_marginals(M, lw, xs[-1]), "does not match the grid")
})
