# Coverage tests for R/alf3df_native.R (Karras et al. 2022 sampler as
# used by AlphaFold 3): noise schedule, random rotations, centre-and-
# rotate augmentation, Euler/Heun steps with churn, and sampling.

test_that("Karras noise schedule", {
  s <- morie_alf3df_schedule(5, 0.002, 80, 7)
  a <- 80^(1 / 7)
  b <- 0.002^(1 / 7)
  expect_equal(s, c((a + (0:4) * (b - a) / 4)^7, 0), tolerance = 1e-12)
  expect_equal(s[c(1, 5)], c(80, 0.002), tolerance = 1e-12)
  expect_error(morie_alf3df_schedule(1), "two noise levels")
  expect_error(morie_alf3df_schedule(4, 1, 0.5), "sigma_min < sigma_max")
})

test_that("uniform random rotations and augmentation", {
  R <- morie_alf3df_rotation(.ghc_rng(4))
  expect_equal(crossprod(R), diag(3), tolerance = 1e-12)
  expect_equal(det(R), 1, tolerance = 1e-12)
  u <- .ghc_unif(.ghc_rng(4), 3)
  q <- c(sqrt(u[1]) * cos(2 * pi * u[3]), sqrt(1 - u[1]) * sin(2 * pi * u[2]), sqrt(1 - u[1]) * cos(2 * pi * u[2]), sqrt(u[1]) * sin(2 * pi * u[3]))
  v <- c(0.3, -1, 2)
  qv <- function(p, r) c(p[1] * r[1] - sum(p[2:4] * r[2:4]), p[1] * r[2:4] + r[1] * p[2:4] +
    c(p[3] * r[4] - p[4] * r[3], p[4] * r[2] - p[2] * r[4], p[2] * r[3] - p[3] * r[2]))
  expect_equal(as.numeric(R %*% v), qv(qv(q, c(0, v)), c(q[1], -q[2:4]))[2:4], tolerance = 1e-12)
  X <- rbind(c(1, 2, 0), c(-1, 0.5, 2), c(0, -1, 1), c(2, 2, 2))
  ag <- morie_alf3df_augment(X, .ghc_rng(4))
  expect_equal(ag$centroid, colMeans(X))
  expect_equal(ag$x, t(R %*% t(sweep(X, 2, colMeans(X)))), tolerance = 1e-12)
  expect_equal(as.matrix(dist(ag$x)), as.matrix(dist(X)), tolerance = 1e-12)
})

test_that("Euler and Heun steps of the probability-flow ODE", {
  X <- rbind(c(1, 2, 0), c(-1, 0.5, 2), c(0, -1, 1))
  den <- function(x, s) x / (1 + s^2)
  st <- morie_alf3df_step(X, 2, den, sigma_next = 1, order = "euler")
  d <- (X - den(X, 2)) / 2
  expect_equal(st$x, X - d, tolerance = 1e-12)
  expect_equal(st$direction, d, tolerance = 1e-12)
  hn <- morie_alf3df_step(X, 2, den, sigma_next = 1)
  x1 <- X - d
  d2 <- (x1 - den(x1, 1)) / 1
  expect_equal(hn$x, X - 0.5 * (d + d2), tolerance = 1e-12)
  e <- .ghc_rng(7)
  ch <- morie_alf3df_step(X, 2, den, sigma_next = 1, gamma = 0.5, order = "euler", e = e)
  e2 <- .ghc_rng(7)
  amt <- sqrt(3^2 - 2^2)
  Xc <- X
  for (i in 1:3) for (j in 1:3) Xc[i, j] <- Xc[i, j] + amt * .ghc_norm(e2, 1L)
  expect_equal(ch$sigma_hat, 3)
  expect_equal(ch$x, Xc + (1 - 3) * (Xc - den(Xc, 3)) / 3, tolerance = 1e-12)
  expect_error(morie_alf3df_step(X, 2, den, gamma = 0.5), "churn needs")
  expect_error(morie_alf3df_step(X, 2, den, order = "rk4"), "order must be")
  expect_error(morie_alf3df_step(X, 2, den, sigma_next = 3), "\\[0, t\\]")
})

test_that("sampling with an ideal (zero) denoiser contracts to the origin", {
  z <- function(x, s) x * 0
  s <- morie_alf3df_sample(4, z, n_steps = 6, seed = 1)
  expect_equal(s$x, matrix(0, 4, 3))
  expect_equal(s$trajectory, s$schedule[1:6])
  r <- morie_alf3df(rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)), 2, z, sigma_next = 1, order = "euler")
  nx <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)) / 2
  expect_equal(r$x, nx, tolerance = 1e-12)
  expect_equal(r$radius_of_gyration, sqrt(mean(rowSums(sweep(nx, 2, colMeans(nx))^2))), tolerance = 1e-12)
  expect_match(morie_alf3df_cheatsheet(), "Karras")
})
