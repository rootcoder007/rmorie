# Coverage tests for R/baynav_native.R (Rezende and Mohamed 2015):
# invertibility repair, the planar flow and its log-determinant against
# the full Jacobian, flow densities, support transforms and the ELBO.

test_that("invertibility repair moves u'w onto -1 + softplus(u'w)", {
  u <- c(-2, 1)
  w <- c(1.5, 0.5)
  r <- enforce_invertibility(u, w)
  uw <- sum(u * w)
  expect_true(r$adjusted)
  expect_equal(r$u, u + ((-1 + log1p(exp(uw))) - uw) * w / sum(w^2), tolerance = 1e-12)
  expect_equal(r$u_dot_w_after, -1 + log1p(exp(uw)), tolerance = 1e-12)
  expect_gt(r$u_dot_w_after, -1)
  ok <- enforce_invertibility(c(1, 1), w)
  expect_false(ok$adjusted)
  expect_equal(ok$u, c(1, 1))
  expect_error(enforce_invertibility(1:2, 1:3), "differ in length")
})

test_that("planar flow and its log-determinant", {
  z <- c(0.3, -0.8, 1.1)
  u <- c(0.5, -0.2, 0.4)
  w <- c(1, 0.3, -0.7)
  b <- 0.2
  f <- planar_flow(z, u, w, b)
  a <- sum(w * z) + b
  expect_equal(f$z, z + u * tanh(a), tolerance = 1e-12)
  J <- diag(3) + outer(u, (1 - tanh(a)^2) * w)
  expect_equal(f$det, det(J), tolerance = 1e-12)
  expect_equal(f$log_det, log(abs(det(J))), tolerance = 1e-12)
  expect_false(f$invertibility_adjusted)
  expect_true(planar_flow(z, -3 * w, w, b)$invertibility_adjusted)
})

test_that("flow density subtracts each layer's log-determinant", {
  layers <- list(list(u = c(0.5, -0.2), w = c(1, 0.3), b = 0.1), list(u = c(-0.3, 0.6), w = c(0.2, -1), b = -0.4))
  z0 <- c(0.4, -0.5)
  r <- flow_log_density(z0, -1.7, layers)
  f1 <- planar_flow(z0, layers[[1]]$u, layers[[1]]$w, layers[[1]]$b)
  f2 <- planar_flow(f1$z, layers[[2]]$u, layers[[2]]$w, layers[[2]]$b)
  expect_equal(r$log_q, -1.7 - f1$log_det - f2$log_det, tolerance = 1e-12)
  expect_equal(r$z, f2$z)
  expect_equal(r$depth, 2L)
  expect_equal(flow_log_density(z0, -1.7, list())$log_q, -1.7)
})

test_that("support transforms and the Monte Carlo ELBO", {
  p <- transform_to_real(2.5)
  expect_equal(c(p$real, p$log_jacobian, p$inverse), c(log(2.5), -log(2.5), 2.5))
  u <- transform_to_real(0.2, "unit")
  expect_equal(u$real, qlogis(0.2), tolerance = 1e-12)
  expect_equal(u$log_jacobian, -log(0.2) - log(0.8), tolerance = 1e-12)
  expect_equal(u$inverse, 0.2, tolerance = 1e-12)
  expect_equal(transform_to_real(-3, "real")$real, -3)
  expect_error(transform_to_real(0), "must be positive, got 0")
  expect_error(transform_to_real(1.5, "unit"), "\\(0,1\\), got 1.5")
  expect_error(transform_to_real(1, "simplex"), "got simplex")
  s <- list(-1, 0.5, 2, 0.1)
  e <- elbo(function(x) dnorm(x, log = TRUE), function(x) dnorm(x, 0.5, 1.2, log = TRUE), s)
  v <- vapply(s, function(x) dnorm(x, log = TRUE) - dnorm(x, 0.5, 1.2, log = TRUE), 0)
  expect_equal(e$elbo, mean(v), tolerance = 1e-12)
  expect_equal(e$se, sd(v) / 2, tolerance = 1e-12)
  expect_equal(elbo(function(x) 1, function(x) 0, list(3))$se, 0)
  expect_error(elbo(identity, identity, list()), "no samples")
  expect_error(morie_baynav(), "is a namespace")
})
