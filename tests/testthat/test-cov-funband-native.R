# Coverage tests for R/funBand_native.R (Wahba 1983): the roughness
# matrix against the integrated squared second derivative of the natural
# spline, the influence matrix, GCV and the Bayesian band.

fb_x <- c(0.05, 0.12, 0.3, 0.41, 0.55, 0.63, 0.8, 0.92, 1)
fb_y <- sin(2 * pi * fb_x) + c(0.1, -0.05, 0.08, -0.12, 0.03, 0.06, -0.09, 0.02, -0.04)

test_that("the roughness form is the integral of the squared second derivative", {
  g <- c(0.3, -1, 0.5, 2, 1.1, -0.4, 0.2, 0.9, 1.5)
  K <- .funBand_roughness(fb_x)
  sf <- splinefun(fb_x, g, method = "natural")
  pen <- integrate(function(t) sf(t, deriv = 2)^2, min(fb_x), max(fb_x), subdivisions = 1000L, rel.tol = 1e-12)$value
  expect_equal(as.numeric(t(g) %*% K %*% g), pen, tolerance = 1e-8)
  expect_equal(as.numeric(K %*% (2 - 3 * fb_x)), rep(0, 9), tolerance = 1e-9)
  expect_error(.funBand_qr_bands(1:3), "at least four")
  expect_error(.funBand_qr_bands(c(1, 2, 2, 3)), "strictly increasing")
})

test_that("influence matrix is (I + lambda K)^-1 and GCV its score", {
  K <- .funBand_roughness(fb_x)
  A <- morie_funBand_influence_matrix(fb_x, 1e-3)
  expect_equal(A, solve(diag(9) + 1e-3 * K), tolerance = 1e-10)
  expect_equal(as.numeric(A %*% (1 + 2 * fb_x)), 1 + 2 * fb_x, tolerance = 1e-10)
  expect_equal(morie_funBand_influence_matrix(fb_x, 0), diag(9), tolerance = 1e-12)
  expect_error(morie_funBand_influence_matrix(fb_x, -1), "non-negative")
  rss <- sum((fb_y - A %*% fb_y)^2)
  expect_equal(morie_funBand_gcv_score(fb_y, A), (rss / 9) / ((9 - sum(diag(A))) / 9)^2, tolerance = 1e-12)
  expect_equal(morie_funBand_gcv_score(fb_y, diag(9)), Inf)
})

test_that("Bayesian band at a fixed lambda and by GCV", {
  r <- morie_funBand(fb_y, x = fb_x, lam = 1e-3, truth = sin(2 * pi * fb_x))
  A <- morie_funBand_influence_matrix(fb_x, 1e-3)
  f <- as.numeric(A %*% fb_y)
  edf <- 9 - sum(diag(A))
  s2 <- sum((fb_y - f)^2) / edf
  hw <- qt(0.975, edf) * sqrt(s2 * diag(A))
  expect_equal(r$fitted, f, tolerance = 1e-12)
  expect_equal(r$half_width, hw, tolerance = 1e-12)
  expect_equal(r$posterior_variance, s2 * diag(A), tolerance = 1e-12)
  expect_equal(r$coverage, mean(abs(sin(2 * pi * fb_x) - f) <= hw))
  nr <- morie_funBand(fb_y, x = fb_x, lam = 1e-3, quantile = "normal", alpha = 0.1)
  expect_equal(nr$multiplier, qnorm(0.95))
  grid <- 10^seq(-6, 2, length.out = 9)
  gs <- vapply(grid, function(l) morie_funBand_gcv_score(fb_y, morie_funBand_influence_matrix(fb_x, l)), 0)
  g <- morie_funBand(fb_y, x = fb_x, n_lambda = 9, log_lambda_range = c(-6, 2))
  expect_equal(g$lambda, grid[which.min(gs)], tolerance = 1e-12)
  expect_equal(g$gcv, min(gs), tolerance = 1e-12)
  expect_equal(morie_funBand(fb_y, lam = 0.01)$x, (1:9) / 9)
  expect_identical(morie_functional_band, morie_funBand)
  expect_match(morie_funBand_cheatsheet(), "DIAGONAL")
  expect_error(morie_funBand(1:3), "at least four")
  expect_error(morie_funBand(fb_y, alpha = 1), "alpha must lie")
  expect_error(morie_funBand(fb_y, x = 1:4), "9 observations but 4 design")
  expect_error(morie_funBand(fb_y, quantile = "z"), "'t' or 'normal'")
  expect_error(morie_funBand(fb_y, x = fb_x, lam = 1, truth = 1:2), "true values")
  expect_error(morie_funBand(fb_y, x = fb_x, lam = 0), "no residual degrees")
})
