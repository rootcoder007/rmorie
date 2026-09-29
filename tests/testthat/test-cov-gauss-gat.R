# Coverage for the Gaussian helpers, graph attention layers, the genetic
# algorithm and the n-step TD return; every expectation is recomputed from
# the documented formula on a small input.

vdc2 <- function(i) {
  k <- i + 1
  f <- 1
  r <- 0
  while (k > 0) {
    f <- f / 2
    r <- r + f * (k %% 2)
    k <- k %/% 2
  }
  r
}

test_that("GaussApx, GaussCount, GaussDom and GaussTail match their formulas", {
  a <- GaussApx(2, 50)
  expect_equal(a$approx, exp(-4 / 50) / sqrt(pi * 50), tolerance = 1e-12)
  expect_equal(a$exact, dbinom(52, 100, 0.5), tolerance = 1e-12)
  expect_equal(a$rel_error, abs(a$approx - a$exact) / a$exact, tolerance = 1e-12)
  expect_error(GaussApx(1, 0), "n must be")
  expect_error(GaussApx(c(1, 2), 3), "x must be")

  g <- GaussCount(c(30, 35, 41), n_reps = 1000, mu = 35, sigma = 5.4)
  expect_equal(g$expected_count, 1000 * dnorm(c(30, 35, 41), 35, 5.4), tolerance = 1e-12)
  expect_error(GaussCount(1, sigma = 0), "sigma")
  expect_error(GaussCount(1, n_reps = -1), "n_reps")

  d <- GaussDom(3, 400)
  expect_equal(d$ratio, 3 / 20, tolerance = 1e-12)
  expect_false(d$well_inside)
  expect_true(GaussDom(-1, 10000)$well_inside)
  expect_error(GaussDom(1, 2.5), "integer")

  tl <- GaussTail(3, sigma = 2.5)
  expect_equal(tl$area_fraction, dnorm(3), tolerance = 1e-12)
  expect_equal(GaussTail(1.5)$area_fraction, dnorm(1.5), tolerance = 1e-12)
  expect_error(GaussTail(sigma = -1), "sigma")
})

test_that("Gausslik is the normal log-likelihood with fixed or profiled sigma", {
  y <- c(1.2, -0.4, 2.5, 0.9, 3.1)
  mu <- c(1, 0, 2, 1, 2)
  r <- Gausslik(y, mu, sigma = 0.7)
  expect_equal(r$loglik, sum(dnorm(y, mu, 0.7, log = TRUE)), tolerance = 1e-12)
  expect_equal(r$rss, sum((y - mu)^2), tolerance = 1e-12)
  p <- Gausslik(y, 1.5)
  s <- sqrt(mean((y - 1.5)^2))
  expect_equal(p$sigma, s, tolerance = 1e-12)
  expect_equal(p$loglik, sum(dnorm(y, 1.5, s, log = TRUE)), tolerance = 1e-12)
  expect_equal(p$aic, -2 * p$loglik + 2, tolerance = 1e-12)
  expect_error(Gausslik(c(1, 1), 1), "zero residual")
  expect_error(Gausslik(y, mu, sigma = -1), "positive")
  expect_error(Gausslik(y, c(1, 2)), "same length")
})

test_that("Gaussm adds sigma * qnorm(van der Corput) noise with the Dwork-Roth sigma", {
  fv <- c(10, 20, 30)
  r <- Gaussm(fv, l2_sens = 2, epsilon = 0.5, delta = 1e-5, draw = 3)
  sig <- sqrt(2 * log(1.25 / 1e-5)) * 2 / 0.5
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  noise <- sig * qnorm(vapply(3:5, vdc2, 0))
  expect_equal(r$noise, noise, tolerance = 1e-12)
  expect_equal(r$released, fv + noise, tolerance = 1e-12)
  expect_error(Gaussm(fv, 1, 1.5, 1e-5), "epsilon")
  expect_error(Gaussm(fv, 0, 0.5, 1e-5), "sensitivity")
  expect_error(Gaussm(fv, 1, 0.5, 2), "delta")
})

test_that("Gat, Gatemd and GatV2 reproduce the attention weights", {
  A <- matrix(c(0, 1, 0, 1,
                1, 0, 1, 0,
                0, 1, 0, 0,
                1, 0, 0, 0), 4, byrow = TRUE)
  X <- matrix(c(0.5, -1, 2, 0.3, -0.7, 1.1, 0.2, 0.9), 4, 2)
  W <- matrix(c(1, 0.5, -0.3, 2), 2, 2)
  a <- c(0.4, -0.2, 0.1, 0.3)
  r <- Gat(A, X, W, a, alpha_leaky = 0.2)
  Wh <- X %*% W
  s1 <- as.numeric(Wh %*% a[1:2])
  s2 <- as.numeric(Wh %*% a[3:4])
  E <- outer(s1, s2, "+")
  E <- ifelse(E > 0, E, 0.2 * E)
  M <- (A != 0) | diag(4) == 1
  Al <- ifelse(M, exp(E), 0)
  Al <- Al / rowSums(Al)
  expect_equal(r$alpha, Al, tolerance = 1e-12)
  expect_equal(r$H, Al %*% Wh, tolerance = 1e-12)
  expect_equal(r$estimate, mean(Al %*% Wh), tolerance = 1e-12)

  g1 <- Gatemd(A, X, heads = 1)
  g3 <- Gatemd(A, X, heads = 3)
  ref <- Gat(A, X, diag(2), rep(1, 4))$H
  expect_equal(g1$H, ref, tolerance = 1e-12)
  expect_equal(g3$H, ref, tolerance = 1e-12)
  expect_equal(g3$estimate, mean(ref), tolerance = 1e-12)
  expect_error(Gatemd(A[1:3, ], X), "square")
  expect_error(Gatemd(A, X, heads = 0), "heads")

  v <- GatV2(A, X)
  g <- rowSums(ifelse(X > 0, X, 0.2 * X))
  E2 <- outer(g, g, "+")
  B <- ifelse(M, exp(E2), 0)
  B <- B / rowSums(B)
  expect_equal(v$alpha_first, B[1, M[1, ]], tolerance = 1e-12)
  expect_equal(v$H, 1 / (1 + exp(-(B %*% X))), tolerance = 1e-12)
  expect_error(GatV2(A, X[1:3, ]), "one row per node")
})

test_that("Ga_opt keeps the elite so the best fitness never worsens", {
  f <- function(x) sum((x - c(1, -2))^2)
  pop <- matrix(c(0, 0, 3, 1, -1, -3, 2, 2, 0.5, -1.5), 5, byrow = TRUE)
  r <- Ga_opt(f, pop, generations = 6, mutation = 0.2)
  expect_length(r$best_path, 7)
  expect_equal(r$best_path[1], min(apply(pop, 1, f)), tolerance = 1e-12)
  expect_true(all(diff(r$best_path) <= 1e-15))
  expect_equal(r$best_fitness, f(r$best), tolerance = 1e-12)
  same <- Ga_opt(f, matrix(c(1, 3), 4, 2, byrow = TRUE), generations = 3, mutation = 0)
  expect_equal(same$best, c(1, 3))
  expect_equal(same$best_path, rep(f(c(1, 3)), 4))
  expect_error(Ga_opt(f, matrix(1, 1, 2)), "two individuals")
  expect_error(Ga_opt(1, pop), "callable")
  expect_error(Ga_opt(f, pop, generations = 0), "generations")
})

test_that("Nsteptd returns the n-step return and the TD update", {
  R <- c(1, 2, 3)
  V <- c(0.5, 1, 1.5, 2)
  g <- 0.9
  r <- Nsteptd(R, V, n = 2, gamma = g, alpha = 0.1)
  G <- c(1 + g * 2 + g^2 * V[3], 2 + g * 3, 3)
  expect_equal(r$returns, G, tolerance = 1e-12)
  expect_equal(r$bootstrapped, c(1, 0, 0))
  expect_equal(r$v_new, c(V[1:3] + 0.1 * (G - V[1:3]), V[4]), tolerance = 1e-12)
  r1 <- Nsteptd(R, V, n = 1, gamma = g, alpha = 0.5, states = c(3, 2, 1, 0))
  G1 <- c(1 + g * V[3], 2 + g * V[2], 3)
  expect_equal(r1$returns, G1, tolerance = 1e-12)
  vn <- V
  for (t in 1:3) {
    s <- c(3, 2, 1)[t] + 1
    vn[s] <- vn[s] + 0.5 * (G1[t] - vn[s])
  }
  expect_equal(r1$v_new, vn, tolerance = 1e-12)
})
