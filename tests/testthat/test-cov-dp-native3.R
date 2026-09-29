# Coverage for the private learners (Abadi et al. 2016 DP-SGD; Chaudhuri,
# Monteleoni & Sarwate 2011 private logistic regression; DP k-means,
# marginal synthetic data, the exponential-mechanism changepoint, DP-FedAvg
# and DP-GAN). Each seeded release is regenerated from the same seed with
# the documented clip-then-noise recipe; the deterministic parts (clipping,
# Adam moments, utilities, probabilities) are recomputed from definitions.

.lap <- function(n, b) {
  u <- stats::runif(n) - 0.5
  -b * sign(u) * log1p(-2 * abs(u))
}
.clip_rows <- function(G, C) {
  nr <- sqrt(rowSums(G^2))
  G * pmin(1, C / nr)
}

test_that("DP-SGD clips each row to C, sums, adds N(0, (sigma C)^2) and averages", {
  G <- rbind(c(3, 4), c(0.3, 0.4), c(-1, 2), c(0.1, 0))
  r <- morie_dp_sgd(G, C = 1.5, sigma = 0.8, lr = 0.2, theta = c(1, -1), seed = 4)
  set.seed(4)
  gbar <- (colSums(.clip_rows(G, 1.5)) + stats::rnorm(2, 0, 1.2)) / 4
  expect_equal(r$private_gradient, gbar, tolerance = 1e-12)
  expect_equal(r$update, -0.2 * gbar, tolerance = 1e-12)
  expect_equal(r$theta, c(1, -1) - 0.2 * gbar, tolerance = 1e-12)
  expect_equal(r$clipped_fraction, 0.5)
  expect_equal(r$noise_sd, 1.2)
  z <- morie_dp_sgd(G, C = 1.5, sigma = 0)
  expect_equal(z$private_gradient, colSums(.clip_rows(G, 1.5)) / 4, tolerance = 1e-12)
  expect_error(morie_dp_sgd(c(1, 2)), "per-example")
  expect_error(morie_dp_sgd(G, C = 0), "C must be positive")
  expect_error(morie_dp_sgd(G, sigma = -1), "non-negative")
  expect_error(morie_dp_sgd(G, theta = 1:3), "theta has 3 entries")
})

test_that("DP-Adam applies bias-corrected moments to the private gradient", {
  G <- rbind(c(2, -1, 0.5), c(0.2, 0.1, -0.3))
  r1 <- morie_dp_adam(G, C = 1, sigma = 0.5, lr = 0.01, seed = 9)
  g1 <- morie_dp_sgd(G, C = 1, sigma = 0.5, seed = 9)$private_gradient
  m <- 0.1 * g1
  v <- 0.001 * g1^2
  expect_equal(r1$update, -0.01 * (m / 0.1) / (sqrt(v / 0.001) + 1e-8), tolerance = 1e-12)
  expect_identical(r1$t, 1L)
  r2 <- morie_dp_adam(G, C = 1, sigma = 0.5, lr = 0.01, state = r1$state, seed = 10)
  g2 <- morie_dp_sgd(G, C = 1, sigma = 0.5, seed = 10)$private_gradient
  m2 <- 0.9 * m + 0.1 * g2
  v2 <- 0.999 * v + 0.001 * g2^2
  expect_equal(r2$update, -0.01 * (m2 / (1 - 0.81)) / (sqrt(v2 / (1 - 0.999^2)) + 1e-8), tolerance = 1e-12)
  expect_identical(r2$t, 2L)
  expect_equal(r2$noise_sd, 0.25)
  expect_equal(r2$signal_to_noise, sqrt(sum(g2^2)) / (0.25 * sqrt(3)), tolerance = 1e-12)
  noisy <- morie_dp_adam(G * 1e-3, C = 1, sigma = 5, seed = 1)
  expect_match(noisy$warnings, "signal-to-noise")
})

test_that("DP-FedAvg and DP-GAN clip per client / per example", {
  U <- rbind(c(1, 1), c(0.2, -0.1), c(-3, 0))
  f <- morie_dp_fedavg(U, C = 1, sigma = 0.7, seed = 2)
  set.seed(2)
  expect_equal(f$aggregate, (colSums(.clip_rows(U, 1)) + stats::rnorm(2, 0, 0.7)) / 3, tolerance = 1e-12)
  expect_equal(f$clipped_fraction, 2 / 3, tolerance = 1e-12)
  expect_equal(f$noise_sd_per_client, 0.7 / 3, tolerance = 1e-12)
  expect_equal(morie_dp_fedavg(U, sigma = 0)$aggregate, colSums(.clip_rows(U, 1)) / 3, tolerance = 1e-12)
  expect_error(morie_dp_fedavg(1:3), "one row per client")
  expect_error(morie_dp_fedavg(U, C = -1), "positive")
  g <- morie_dp_gan(U, C = 1, sigma = 0.7, lr = 0.05, n_disc_steps = 5, seed = 2)
  expect_equal(g$private_gradient, f$aggregate, tolerance = 1e-12)
  expect_equal(g$disc_update, -0.05 * f$aggregate, tolerance = 1e-12)
  expect_identical(g$steps_to_account, 5L)
  expect_error(morie_dp_gan(1:3), "per-example")
})

.dplog <- function(X, y, eps, lam, C, n_iter, lr, seed, objective) {
  n <- length(y)
  p <- ncol(X)
  Xc <- .clip_rows(X, C)
  Xu <- Xc / C
  set.seed(seed)
  draw <- function(scale) {
    d <- stats::rnorm(p)
    d / sqrt(sum(d^2)) * stats::rgamma(1, shape = p, scale = scale)
  }
  b <- numeric(p)
  extra <- 0
  if (objective) {
    slack <- log(1 + 2 * 0.25 / (n * lam) + 0.0625 / (n * lam)^2)
    if (eps > slack) {
      ep <- eps - slack
    } else {
      extra <- max(0.25 / (n * (exp(eps / 4) - 1)) - lam, 0)
      ep <- eps / 2
    }
    b <- draw(2 / ep)
  }
  beta <- numeric(p)
  for (i in seq_len(n_iter)) {
    mu <- stats::plogis(as.vector(Xu %*% beta))
    beta <- beta - lr * (as.vector(crossprod(Xu, mu - y)) / n + (lam + extra) * beta + b / n)
  }
  if (!objective) beta <- beta + draw(2 / (n * lam * eps))
  beta / C
}

test_that("private logistic regression: objective and output perturbation", {
  set.seed(1)
  X <- matrix(stats::rnorm(60), 30)
  y <- as.numeric(X[, 1] - X[, 2] + stats::rnorm(30, sd = 0.5) > 0)
  o <- morie_dp_logistic(X, y, epsilon = 2, lam = 0.05, C = 1.5, n_iter = 50, seed = 3)
  ref <- .dplog(X, y, 2, 0.05, 1.5, 50, 0.1, 3, TRUE)
  expect_equal(o$beta, ref, tolerance = 1e-12)
  pr <- stats::plogis(as.vector(.clip_rows(X, 1.5) %*% ref))
  expect_equal(o$prob, pr, tolerance = 1e-12)
  expect_equal(o$accuracy, mean((pr >= 0.5) == y))
  # budget below the slack: the regulariser is raised and eps halved
  t <- morie_dp_logistic(X, y, epsilon = 0.05, lam = 0.01, n_iter = 20, seed = 3)
  expect_equal(t$beta, .dplog(X, y, 0.05, 0.01, 1, 20, 0.1, 3, TRUE), tolerance = 1e-12)
  u <- morie_dp_logistic(X, y, epsilon = 1, method = "output", lam = 0.1, n_iter = 40, seed = 5)
  expect_equal(u$beta, .dplog(X, y, 1, 0.1, 1, 40, 0.1, 5, FALSE), tolerance = 1e-12)
  expect_error(morie_dp_logistic(X, y, lam = 0), "lam > 0")
  expect_error(morie_dp_logistic(X, y[-1]), "30 rows but y has 29")
  expect_error(morie_dp_logistic(X, y + 1), "0/1")
  expect_error(morie_dp_logistic(X, y, epsilon = 0), "finite and positive")
})

test_that("DP k-means: noisy counts and sums per iteration, bounded clipping", {
  set.seed(5)
  X <- rbind(matrix(stats::rnorm(40, 2, 0.3), 20), matrix(stats::rnorm(40, 7, 0.3), 20))
  r <- morie_dp_kmeans(X, k = 2, epsilon = 4, n_iter = 3, bounds = c(0, 10), seed = 7)
  set.seed(7)
  cen <- matrix(10 * stats::runif(4), 2, 2)
  lab <- function(cen) apply(X, 1, function(x) which.min(colSums((t(cen) - x)^2)))
  ei <- 4 / 6
  for (it in 1:3) {
    l <- lab(cen)
    for (j in 1:2) {
      cnt <- sum(l == j) + .lap(1, 1 / ei)
      s <- colSums(X[l == j, , drop = FALSE]) + .lap(2, 20 / ei)
      if (cnt < 1) cen[j, ] <- 10 * stats::runif(2) else cen[j, ] <- s / cnt
    }
  }
  expect_equal(r$centers, cen, tolerance = 1e-12)
  expect_identical(r$labels, lab(cen) - 1L)
  expect_equal(r$inertia, sum((X - cen[lab(cen), ])^2), tolerance = 1e-12)
  expect_equal(r$epsilon_per_iteration, ei)
  expect_length(r$warnings, 0L)
  d <- morie_dp_kmeans(X, k = 2, epsilon = 1e-3, n_iter = 1, seed = 7)
  expect_equal(d$bounds, range(X))
  expect_match(d$warnings[1], "non-private")
  expect_error(morie_dp_kmeans(X, k = 0), "k must be at least 1")
  expect_error(morie_dp_kmeans(X, n_iter = 0), "n_iter must be at least 1")
  expect_error(morie_dp_kmeans(X, bounds = c(3, 1)), "need a < b")
})

test_that("marginal synthetic data draws from noisy per-feature histograms", {
  X <- cbind(c(0.1, 0.5, 0.9, 0.3, 0.35, 0.8), c(2, 4, 1, 3, 5, 2.5))
  r <- morie_dp_synthetic_data(X, epsilon = 2, n_synth = 5, bins = 4, bounds = c(0, 5), seed = 6)
  set.seed(6)
  e <- seq(0, 5, length.out = 5)
  syn <- matrix(0, 5, 2)
  err <- numeric(2)
  for (j in 1:2) {
    cnt <- tabulate(pmin(findInterval(X[, j], e), 4), 4)
    nz <- pmax(cnt + .lap(4, 2), 0)
    pr <- nz / sum(nz)
    k <- sample.int(4, 5, replace = TRUE, prob = pr)
    syn[, j] <- stats::runif(5, e[k], e[k + 1])
    err[j] <- sum(abs(pr - cnt / 6))
  }
  expect_equal(r$synthetic, syn, tolerance = 1e-12)
  expect_equal(r$marginal_error, err, tolerance = 1e-12)
  expect_equal(r$correlation_real, stats::cor(X[, 1], X[, 2]), tolerance = 1e-12)
  expect_length(r$warnings, 1L)
  one <- morie_dp_synthetic_data(X[, 1, drop = FALSE], seed = 1)
  expect_true(is.na(one$correlation_real))
  expect_identical(dim(one$synthetic), c(6L, 1L))
  expect_length(one$warnings, 2L)
  expect_error(morie_dp_synthetic_data(X, epsilon = -1), "finite and positive")
})

test_that("private changepoint samples tau with prob ~ exp(eps u / 2 (b - a)^2)", {
  y <- c(1, 1.2, 0.9, 1.1, 1, 0.8, 3, 3.2, 2.9, 3.1, 3, 2.8, 3.1)
  r <- morie_dp_changepoint(y, epsilon = 3, bounds = c(0, 4), min_segment = 3, seed = 11)
  cand <- 3:9
  sst <- sum((y - mean(y))^2)
  u <- vapply(cand, function(t) sst - sum((y[1:t] - mean(y[1:t]))^2) - sum((y[-(1:t)] - mean(y[-(1:t)]))^2), 1)
  p <- exp(3 * u / 32)
  p <- p / sum(p)
  expect_identical(r$candidates, cand)
  expect_equal(r$probabilities, p, tolerance = 1e-12)
  set.seed(11)
  k <- sample.int(7, 1, prob = p)
  expect_identical(r$changepoint, cand[k])
  expect_equal(r$utility_ratio, u[k] / max(u), tolerance = 1e-12)
  expect_identical(which.max(u), 4L)
  d <- morie_dp_changepoint(y, min_segment = 3, seed = 1)
  expect_length(d$warnings, 2L)
  expect_true(is.na(morie_dp_changepoint(rep(2, 11), bounds = c(0, 4), seed = 1)$utility_ratio))
  expect_error(morie_dp_changepoint(y, min_segment = 7), "too short")
})
