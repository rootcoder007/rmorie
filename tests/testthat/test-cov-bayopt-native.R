# Coverage tests for R/bayopt_native.R (Snoek, Larochelle and Adams 2012):
# GP posterior and its gradient, EI/PI/LCB and their gradients, the
# multi-start acquisition maximiser and the optimisation loop.

bo_X <- cbind(c(0.1, 0.4, 0.55, 0.9), c(0.2, 0.8, 0.3, 0.6))
bo_y <- c(1.2, 0.3, 0.5, 1.9)
bo_m52 <- function(A, B, l = 0.4) {
  r2 <- (outer(A[, 1], B[, 1], "-")^2 + outer(A[, 2], B[, 2], "-")^2) / l^2
  s <- sqrt(5 * r2)
  (1 + s + 5 / 3 * r2) * exp(-s)
}

test_that("kernels and the GP posterior", {
  a <- c(0.2, 0.5)
  b <- c(0.7, 0.1)
  r2 <- sum((a - b)^2 / c(0.3, 0.6)^2)
  expect_equal(squared_exponential(a, b, 2, c(0.3, 0.6)), 2 * exp(-r2 / 2), tolerance = 1e-12)
  expect_equal(matern52(a, b, 1, 0.4), bo_m52(rbind(a), rbind(b))[1, 1], tolerance = 1e-12)
  expect_error(matern52(a, b, 1, c(1, 2, 3)), "one value per dimension")
  Xs <- rbind(c(0.3, 0.4), c(0.8, 0.9))
  K <- bo_m52(bo_X, bo_X) + 1e-6 * diag(4)
  Ks <- bo_m52(Xs, bo_X)
  m <- mean(bo_y)
  p <- gp_posterior(bo_X, bo_y, Xs, length_scale = 0.4, noise = 1e-6)
  expect_equal(p$mean, as.numeric(m + Ks %*% solve(K, bo_y - m)), tolerance = 1e-9)
  expect_equal(p$variance, 1 - rowSums(Ks * t(solve(K, t(Ks)))), tolerance = 1e-9)
  p0 <- gp_posterior(bo_X, bo_y, Xs, kernel = "se", mean = 0, noise = 0.01)
  Kse <- exp(-0.5 * as.matrix(dist(bo_X))^2) + 0.01 * diag(4)
  kse <- exp(-0.5 * (outer(Xs[, 1], bo_X[, 1], "-")^2 + outer(Xs[, 2], bo_X[, 2], "-")^2))
  expect_equal(p0$mean, as.numeric(kse %*% solve(Kse, bo_y)), tolerance = 1e-9)
  expect_error(gp_posterior(bo_X, bo_y, Xs, kernel = "rbf"), "kernel must be")
  expect_error(gp_posterior(bo_X, bo_y, Xs[, 1, drop = FALSE]), "wrong dimension")
  expect_error(gp_posterior(rbind(c(0, 0), c(0, 0)), c(1, 2), Xs, noise = 0), "not positive")
})

test_that("closed-form posterior gradients match central differences", {
  q <- c(0.35, 0.45)
  h <- 1e-6
  for (kern in c("matern52", "se")) {
    g <- gp_posterior_gradient(bo_X, bo_y, q, kernel = kern, length_scale = 0.4, noise = 1e-6)
    fd <- sapply(1:2, function(j) {
      e <- c(0, 0)
      e[j] <- h
      pp <- gp_posterior(bo_X, bo_y, rbind(q + e, q - e), kernel = kern, length_scale = 0.4, noise = 1e-6)
      c((pp$mean[1] - pp$mean[2]) / (2 * h), (pp$sd[1] - pp$sd[2]) / (2 * h))
    })
    # central differences with h = 1e-6 are accurate to ~1e-8 here
    expect_equal(g$grad_mu, fd[1, ], tolerance = 1e-6, info = kern)
    expect_equal(g$grad_sd, fd[2, ], tolerance = 1e-6, info = kern)
  }
  expect_error(gp_posterior_gradient(bo_X, bo_y, 0.3), "wrong dimension")
})

test_that("EI, PI and LCB and their gradients (minimisation)", {
  mu <- 0.4
  sd <- 0.3
  best <- 0.35
  g <- (best - 0.01 - mu) / sd
  expect_equal(expected_improvement(mu, sd, best, 0.01), sd * (g * pnorm(g) + dnorm(g)), tolerance = 1e-12)
  expect_equal(probability_of_improvement(mu, sd, best, 0.01), pnorm(g), tolerance = 1e-12)
  expect_equal(lower_confidence_bound(mu, sd, 1.5), mu - 0.45)
  expect_equal(acquire(mu, sd, best, "lcb", kappa = 1.5), -(mu - 0.45))
  expect_equal(expected_improvement(mu, 0, best), 0)
  expect_error(acquire(mu, sd, best, "ucb"), "acq must be")
  q <- c(0.35, 0.45)
  h <- 1e-6
  for (acq in c("ei", "pi", "lcb")) {
    gg <- gp_posterior_gradient(bo_X, bo_y, q, length_scale = 0.4, noise = 1e-6)
    ag <- acquisition_gradient(gg$grad_mu, gg$grad_sd, gg$mu, gg$sd, 0.3, acq)
    f <- function(z) {
      pp <- gp_posterior(bo_X, bo_y, rbind(z), length_scale = 0.4, noise = 1e-6)
      acquire(pp$mean, pp$sd, 0.3, acq)
    }
    fd <- vapply(1:2, function(j) {
      e <- c(0, 0)
      e[j] <- h
      (f(q + e) - f(q - e)) / (2 * h)
    }, 0)
    expect_equal(ag, fd, tolerance = 1e-5, info = acq)
  }
  expect_equal(acquisition_gradient(c(1, 1), c(1, 1), 0, 0, 1, "ei"), c(0, 0))
})

test_that("acquisition maximiser beats every random candidate it could have drawn", {
  box <- list(c(0, 1), c(0, 1))
  m <- maximise_acquisition(bo_X, bo_y, min(bo_y), box, length_scale = 0.4, noise = 1e-6, seed = 3)
  expect_true(all(m$x >= 0 & m$x <= 1))
  g <- expand.grid(seq(0, 1, 0.1), seq(0, 1, 0.1))
  pp <- gp_posterior(bo_X, bo_y, as.matrix(g), length_scale = 0.4, noise = 1e-6)
  grid_best <- max(vapply(seq_len(nrow(g)), function(i) expected_improvement(pp$mean[i], pp$sd[i], min(bo_y)), 0))
  expect_gte(m$acq, grid_best - 1e-3)
  expect_equal(m$n_starts, 8L)
  expect_error(maximise_acquisition(bo_X, bo_y, 0, box, starts = list()), "no starting points")
})

test_that("Bayesian optimisation loop on a smooth 1-D objective", {
  f <- function(x) (x[1] - 0.3)^2
  r <- bayopt(f, list(c(0, 1)), n_iter = 8, n_init = 3, length_scale = 0.3, noise = 1e-6, seed = 2)
  expect_equal(r$n_eval, 11L)
  expect_equal(r$y, unname(apply(r$X, 1, f)), tolerance = 1e-12)
  expect_equal(r$y_best, min(r$y))
  expect_false(is.unsorted(-vapply(r$trace, function(t) t$best, 0)))
  expect_lt(abs(r$x_best - 0.3), 0.05)
  rr <- bayopt(f, list(c(0, 1)), n_iter = 3, inner = "random", n_candidates = 50, X0 = cbind(c(0.1, 0.9)), acq = "lcb")
  expect_equal(rr$y[1:2], c(f(0.1), f(0.9)))
  expect_identical(bayesian_optimization, bayopt)
  expect_error(bayopt(f, list(c(1, 0))), "lo < hi")
  expect_error(bayopt(f, list(c(0, 1)), inner = "grid"), "inner must be")
  expect_error(bayopt(f, list(c(0, 1)), n_init = 1), "two initial points")
})
