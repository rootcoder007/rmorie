# Coverage for distributional RL (Bellemare, Dabney & Munos 2017): the
# atom grid, the categorical projection of r + gamma z onto it (mass split
# linearly between neighbouring atoms; mean-preserving inside the
# support), the cross-entropy loss, the C51 update with its greedy action,
# the N = 2 Bernoulli form and value-distribution iteration to a fixed
# point.

.proj <- function(r, g, p, z) {
  n <- length(z)
  dz <- z[2] - z[1]
  m <- numeric(n)
  for (j in seq_len(n)) {
    tz <- min(max(r + g * z[j], z[1]), z[n])
    b <- (tz - z[1]) / dz + 1
    l <- floor(b)
    u <- ceiling(b)
    if (l == u) m[l] <- m[l] + p[j] else {
      m[l] <- m[l] + p[j] * (u - b)
      m[u] <- m[u] + p[j] * (b - l)
    }
  }
  m
}

test_that("atoms, means and the categorical projection", {
  z <- morie_distq(-2, 2, 5)
  expect_equal(z, seq(-2, 2, by = 1))
  expect_equal(distribution_mean(c(0.1, 0.2, 0.3, 0.2, 0.2), z), sum(z * c(0.1, 0.2, 0.3, 0.2, 0.2)), tolerance = 1e-12)
  p <- c(0.1, 0.2, 0.3, 0.25, 0.15)
  m <- categorical_projection(0.1, 0.9, p, -2, 2)
  expect_equal(m, .proj(0.1, 0.9, p, z), tolerance = 1e-12)
  expect_equal(sum(m), 1, tolerance = 1e-12)
  # no clipping here, so the projection preserves the mean of r + gamma Z
  expect_equal(sum(z * m), 0.1 + 0.9 * sum(z * p), tolerance = 1e-12)
  expect_equal(categorical_projection(5, 0.9, p, -2, 2), c(0, 0, 0, 0, 1))
  expect_equal(categorical_projection(1, 0.5, p, -2, 2, done = TRUE), c(0, 0, 0, 1, 0))
  expect_identical(categoricalprojection, categorical_projection)
  expect_identical(distributional_rl, categorical_projection)
  expect_error(categorical_projection(0, 0.9, p[-1], -2, 2, n_atoms = 5), "4 next probabilities for 5")
  expect_error(categorical_projection(0, 0.9, c(-0.1, 1.1, 0, 0, 0), -2, 2), "negative entry")
  expect_error(categorical_projection(0, 0.9, p / 2, -2, 2), "not 1")
  expect_error(categorical_projection(0, 1.5, p, -2, 2), "gamma must be in")
  expect_error(morie_distq(1, 1, 3), "v_max > v_min")
  expect_error(morie_distq(0, 1, 1), "at least 2 atoms")
  expect_error(distribution_mean(1:2, 1:3), "2 probabilities for 3 atoms")
})

test_that("cross-entropy loss and the C51 update", {
  m <- c(0.2, 0.5, 0.3)
  q <- c(0.3, 0.3, 0.4)
  expect_equal(categorical_loss(m, q), -sum(m * log(q)), tolerance = 1e-12)
  expect_equal(categorical_loss(c(1, 0), c(0, 1)), -log(1e-12), tolerance = 1e-12)
  expect_error(categorical_loss(m, q[-1]), "3 targets for 2")
  nxt <- list(c(0.6, 0.3, 0.1, 0, 0), c(0, 0.1, 0.2, 0.3, 0.4), c(0.2, 0.2, 0.2, 0.2, 0.2))
  cur <- c(0.1, 0.2, 0.4, 0.2, 0.1)
  u <- c51_update(0.5, 0.8, nxt, cur, -2, 2)
  z <- -2:2
  qs <- vapply(nxt, function(p) sum(z * p), 1)
  expect_equal(u$q_values, qs, tolerance = 1e-12)
  expect_identical(u$action, 2L)
  expect_equal(u$target, .proj(0.5, 0.8, nxt[[2]], z), tolerance = 1e-12)
  expect_equal(u$loss, -sum(u$target * log(cur)), tolerance = 1e-12)
  expect_equal(u$q_current, sum(z * cur), tolerance = 1e-12)
  expect_equal(c51_update(0.5, 0.8, nxt, cur, -2, 2, done = TRUE)$target, c(0, 0, 0.5, 0.5, 0), tolerance = 1e-12)
})

test_that("Bernoulli form: clip((r + gamma E Z - v_min) / (v_max - v_min), 0, 1) for N = 2", {
  expect_equal(bernoulli_algorithm(1, 0.5, c(0.4, 0.6), -5, 5), (1 + 0.5 * (0.6 * 5 - 0.4 * 5) + 5) / 10, tolerance = 1e-12)
  expect_identical(bernoulli_algorithm(20, 0.5, c(0.5, 0.5), -5, 5), 1)
  expect_identical(bernoulli_algorithm(-20, 0.5, c(0.5, 0.5), -5, 5), 0)
  expect_equal(bernoulli_algorithm(1, 0.9, c(0, 1), 0, 10, done = TRUE), 0.1, tolerance = 1e-12)
  expect_error(bernoulli_algorithm(1, -0.1, c(0.5, 0.5), 0, 1), "gamma must be in")
})

test_that("value-distribution iteration reaches the projected Bellman fixed point", {
  ra <- c(0, 2)
  rp <- c(0.5, 0.5)
  v <- value_distribution_iteration(ra, rp, 0.5, 0, 4, 9)
  z <- seq(0, 4, by = 0.5)
  Tv <- 0.5 * .proj(0, 0.5, v$distribution, z) + 0.5 * .proj(2, 0.5, v$distribution, z)
  expect_true(v$info$converged)
  expect_equal(v$distribution, Tv, tolerance = 1e-12)
  # the mean solves m = E r + gamma m inside the support
  expect_equal(sum(z * v$distribution), 1 / (1 - 0.5), tolerance = 1e-10)
  short <- value_distribution_iteration(ra, rp, 0.5, 0, 4, 9, iters = 2)
  expect_identical(short$info$iterations, 2L)
  expect_false(short$info$converged)
  expect_error(value_distribution_iteration(ra, 1, 0.5, 0, 4, 9), "2 reward atoms but 1")
  expect_error(value_distribution_iteration(ra, c(0.5, 0.6), 0.5, 0, 4, 9), "not 1")
})
