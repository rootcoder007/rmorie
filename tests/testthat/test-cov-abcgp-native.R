# Coverage tests for R/abcgp_native.R: Sobol designs, Wilkinson (2014)
# GP-emulated ABC with history matching, and Meeds and Welling (2014)
# synthetic-likelihood / GPS-ABC samplers.

test_that("Sobol points: each 1-D projection of the first 2^m points is stratified", {
  s <- sobol_sequence(16, 5)
  for (d in 1:5) expect_equal(sort(s[, d]), (0:15) / 16, info = d)
  expect_equal(s[1, ], rep(0, 5))
  # the first coordinate is the van der Corput sequence in Gray-code order
  expect_equal(s[1:4, 1], c(0, 0.5, 0.75, 0.25))
  expect_equal(sobol_sequence(4, 2, skip = 3), sobol_sequence(7, 2)[4:7, ])
  expect_equal(.abcgp.sobol_sequence(16, 5), s)
  expect_error(sobol_sequence(0, 2), "at least 1")
  expect_error(sobol_sequence(4, 9), "between 1 and 8")
})

test_that("designs map Sobol points through the prior", {
  d <- design_from_prior(8, list(c(0, -2), c(1, 2)))
  u <- sobol_sequence(8, 2, skip = 1)
  expect_equal(d, cbind(u[, 1], -2 + 4 * u[, 2]), tolerance = 1e-12)
  q <- design_from_prior(8, list(qnorm, function(p) qexp(p, 2)))
  expect_equal(q, cbind(qnorm(u[, 1]), qexp(u[, 2], 2)), tolerance = 1e-12)
  expect_error(design_from_prior(4, list(qnorm, qnorm), dim = 3), "dim does not match")
  expect_error(design_from_prior(4, list(c(0, 1), 1)), "differ in length")
})

test_that("GP emulator: universal-kriging mean and variance", {
  X <- cbind(c(0.1, 0.3, 0.45, 0.6, 0.8, 0.95))
  y <- c(-3, -1.2, -0.4, -0.5, -1.9, -4)
  fit <- gp_fit(X, y, nugget = 1e-6, lengthscale = 0.3)
  A <- exp(-0.5 * outer(X[, 1], X[, 1], "-")^2 / 0.09) + 1e-6 * diag(6)
  H <- cbind(1, X, X^2)
  Ai <- solve(A)
  beta <- solve(t(H) %*% Ai %*% H, t(H) %*% Ai %*% y)
  expect_equal(fit$beta, as.numeric(beta), tolerance = 1e-8)
  r <- y - H %*% beta
  tau2 <- as.numeric(t(r) %*% Ai %*% r) / 3
  expect_equal(fit$tau2, tau2, tolerance = 1e-8)
  t0 <- 0.52
  k <- exp(-0.5 * (t0 - X[, 1])^2 / 0.09)
  h <- c(1, t0, t0^2)
  m <- sum(h * beta) + sum(k * (Ai %*% r))
  hh <- h - t(H) %*% Ai %*% k
  v <- tau2 * (1 - sum(k * (Ai %*% k)) + t(hh) %*% solve(t(H) %*% Ai %*% H, hh))
  pr <- gp_predict(fit, t0)
  expect_equal(unname(pr["mean"]), m, tolerance = 1e-8)
  expect_equal(unname(pr["sd"]), sqrt(as.numeric(v)), tolerance = 1e-8)
  expect_equal(unname(.abcgp.gp_predict(.abcgp.gp_fit(X, y, 1e-6, 0.3), t0)), unname(pr), tolerance = 1e-10)
  expect_equal(unname(gp_predict(fit, 0.3)["mean"]), -1.2, tolerance = 1e-4)
  expect_equal(implausible(fit, 0.52, threshold = 0.1), unname(pr["mean"] + 3 * pr["sd"] < max(y) - 0.1))
  fm <- gp_fit(X, y, kernel = "matern52")
  expect_true(all(fm$lengthscale > 0))
  expect_error(gp_fit(X, y, kernel = "rq"), "kernel must be")
  expect_error(gp_fit(X[1:2, , drop = FALSE], y[1:2]), "at least 3")
  expect_error(gp_predict(fit, c(1, 2)), "wrong dimension")
})

test_that("GABC likelihood and synthetic likelihood", {
  sim_det <- function(theta, e) theta + c(0.3, -0.4)
  g <- gabc_log_likelihood(sim_det, c(0, 0), c(0.1, 0.2), n_sim = 5, epsilon = 0.5)
  rho <- sqrt(0.4^2 + 0.2^2)
  expect_equal(g$log_lik, -0.5 * (rho / 0.5)^2, tolerance = 1e-12)
  expect_equal(g$nugget_variance, 0)
  expect_equal(gabc_log_likelihood(sim_det, c(0, 0), c(0.1, 0.2), kernel = "uniform", epsilon = 0.5)$log_lik, 0)
  expect_equal(gabc_log_likelihood(sim_det, c(0, 0), c(0.1, 0.2), kernel = "uniform", epsilon = 0.1)$log_lik, -Inf)
  expect_error(gabc_log_likelihood(sim_det, c(0, 0), 0, kernel = "box"), "kernel must be")
  draws <- list(c(1, 2), c(1.5, 1.2), c(0.7, 2.4), c(1.1, 1.9))
  sl <- synthetic_log_likelihood(draws, c(1, 1.5), epsilon = 0.2)
  M <- do.call(rbind, draws)
  S <- cov(M) + 0.04 * diag(2)
  d <- c(1, 1.5) - colMeans(M)
  ref <- -0.5 * (sum(d * solve(S, d)) + log(det(S)) + 2 * log(2 * pi))
  expect_equal(sl$log_lik, ref, tolerance = 1e-12)
  expect_equal(.abcgp.synthetic_log_likelihood(draws, c(1, 1.5), 0.2)$log_lik, ref, tolerance = 1e-12)
  expect_error(synthetic_log_likelihood(draws[1], c(1, 1)), "at least 2")
})

test_that("ABC-MH samplers respect the prior support and are reproducible", {
  sim <- function(theta, e) theta + 0.3 * .ghc_norm(e, 1)
  lp <- function(t) if (t < 0 || t > 1) -Inf else 0
  a <- synthetic_abc(sim, 0.4, lp, 0.5, n_iter = 30, n_sim = 8, proposal_sd = 0.3, seed = 2)
  expect_equal(dim(a$chain), c(31L, 1L))
  expect_true(all(a$chain >= 0 & a$chain <= 1))
  moves <- sum(diff(a$chain[, 1]) != 0)
  expect_equal(a$acceptance_rate, moves / 30)
  expect_identical(synthetic_abc(sim, 0.4, lp, 0.5, n_iter = 30, n_sim = 8, proposal_sd = 0.3, seed = 2), a)
  g <- gps_abc(sim, 0.4, lp, 0.5, n_iter = 10, n_sim = 8, proposal_sd = 0.3, seed = 3, n_alpha = 16, max_sim = 30)
  expect_true(all(g$chain >= 0 & g$chain <= 1))
  expect_gte(g$n_simulations, 2 * 8 * sum(diff(c(g$chain[, 1])) != 0))
  em <- abc_gp_emulator(sim, 0.4, method = "synthetic", log_prior = lp, theta0 = 0.5, n_iter = 20, n_sim = 8, seed = 1)
  expect_equal(em$estimate, colMeans(em$chain[11:21, , drop = FALSE]), tolerance = 1e-12)
  expect_error(abc_gp_emulator(sim, 0.4, method = "synthetic"), "need log_prior and theta0")
  expect_error(abc_gp_emulator(sim, 0.4, method = "rejection"), "method must be")
})

test_that("history matching and the Wilkinson emulator", {
  sim <- function(theta, e) theta + 0.05 * .ghc_norm(e, 1)
  hm <- history_match(sim, 0.35, list(0, 1), n_waves = 2, n_design = 10, n_sim = 10, epsilon = 0.2, threshold = 3)
  expect_length(hm$waves, 2L)
  expect_equal(hm$fit$n, hm$waves[[2]]$n_ensemble)
  # with an absurd threshold every candidate is implausible: the wave
  # falls back to the first n_design candidates instead of emptying
  hm2 <- history_match(sim, 0.35, list(0, 1), n_waves = 2, n_design = 10, n_sim = 10, epsilon = 0.2, threshold = -100)
  expect_equal(hm2$waves[[2]]$ruled_implausible, 40L)
  expect_equal(hm2$waves[[2]]$n_ensemble, 20L)
  w <- abc_gp_emulator(sim, 0.35, method = "wilkinson", prior_ppf = list(0, 1), n_waves = 2, n_design = 10,
    n_sim = 10, epsilon = 0.2, X_grid = cbind(seq(0, 1, by = 0.05)))
  expect_equal(sum(w$posterior), 1, tolerance = 1e-12)
  expect_equal(w$estimate, w$grid[which.max(w$log_likelihood), ])
  expect_lt(abs(w$estimate - 0.35), 0.1)
  expect_identical(abcgpemulator, abc_gp_emulator)
  expect_error(abc_gp_emulator(sim, 0.35, method = "wilkinson"), "needs prior_ppf")
})
