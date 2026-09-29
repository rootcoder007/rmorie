# Bootstrap particle filter (King, Nguyen & Ionides 2016, Algorithms 1
# and 2) and the 1-D Kalman filter, the latter checked against exact
# Gaussian conditioning on the joint law of (x, y).

pc_y <- c(0.3, -0.2, 0.8, 1.1, 0.4, -0.5)
pc_joint <- function(y, a, q, cc, r, m0, p0) {
  # x_n = a x_{n-1} + w_n, x_0 ~ N(m0, p0); y_n = c x_n + v_n
  N <- length(y)
  mx <- m0 * a^(1:N)
  Sx <- matrix(0, N, N)
  for (i in 1:N) for (j in 1:N) {
    k <- min(i, j)
    Sx[i, j] <- a^(i + j) * p0 + sum(a^(i - (1:k)) * a^(j - (1:k))) * q
  }
  Sy <- cc^2 * Sx + r * diag(N)
  list(mx = mx, Sx = Sx, Sy = Sy)
}

test_that("effective sample size is (sum w)^2 / sum w^2", {
  w <- c(0.1, 0.5, 0.2, 0.2)
  expect_equal(morie_prtcl_effective_sample_size(w), 1 / sum(w^2), tolerance = 1e-15)
  expect_equal(morie_prtcl_effective_sample_size(3 * w), 1 / sum(w^2), tolerance = 1e-14)
  expect_identical(morie_prtcl_effective_sample_size(c(0, 0)), 0)
  expect_identical(morie_prtcl(w), morie_prtcl_effective_sample_size(w))
})

test_that("systematic resampling picks the first cumulative weight above (m-1+u)/J", {
  w <- c(0.05, 0.4, 0.1, 0.3, 0.15)
  for (u in c(0, 0.3, 0.5, 0.99)) {
    pos <- (0:4 + u) / 5
    ref <- vapply(pos, function(p) min(which(cumsum(w) >= p - 1e-15)), 1L)
    expect_identical(morie_prtcl_systematic_resample(w, u = u), ref)
    expect_identical(systematic_resample(w, u = u), ref)
    # each particle is copied within one of J w_j times
    cnt <- tabulate(ref, 5)
    expect_true(all(abs(cnt - 5 * w) < 1))
  }
  expect_identical(morie_prtcl_systematic_resample(w), morie_prtcl_systematic_resample(w, u = 0.5))
  u0 <- .ghc_unif(.ghc_rng(0L), 1L)
  expect_identical(systematic_resample(w), systematic_resample(w, u = u0))
  expect_error(morie_prtcl_systematic_resample(c(0, 0)), "lost the signal")
  expect_error(morie_prtcl_systematic_resample(w, u = 1), "offset")
  expect_error(systematic_resample(w, u = -0.1), "got -0.1")
})

test_that("the Kalman filter equals exact Gaussian conditioning", {
  a <- 0.9
  q <- 0.4
  cc <- 1.3
  r <- 0.5
  kf <- morie_prtcl_kalman_filter_1d(pc_y, a, q, cc, r, m0 = 0.2, p0 = 1.5)
  J <- pc_joint(pc_y, a, q, cc, r, 0.2, 1.5)
  N <- length(pc_y)
  for (n in 1:N) {
    idx <- 1:n
    cxy <- cc * J$Sx[n, idx]
    m <- J$mx[n] + sum(cxy * solve(J$Sy[idx, idx], pc_y[idx] - cc * J$mx[idx]))
    expect_equal(kf$means[n], m, tolerance = 1e-12)
  }
  res <- pc_y - cc * J$mx
  ll <- -0.5 * (N * log(2 * pi) + as.numeric(determinant(J$Sy)$modulus) +
                  sum(res * solve(J$Sy, res)))
  expect_equal(kf$loglik, ll, tolerance = 1e-12)
  kr <- kalman_filter_1d(pc_y, a, q, cc, r, m0 = 0.2, p0 = 1.5)
  expect_equal(kr$filtered_mean, kf$means, tolerance = 1e-15)
  expect_equal(kr$loglik, kf$loglik, tolerance = 1e-15)
})

test_that("the particle filter is exact when every particle is the same", {
  # no process noise and a point initial law: every weight is equal
  init <- function(e) 0.7
  step <- function(x, n, e) 0.5 * x + 0.1
  lik <- function(x, y, n) stats::dnorm(y, x, 0.4, log = TRUE)
  pf <- morie_prtcl_particle_filter(pc_y, 10, init, step, lik)
  x <- numeric(6)
  s <- 0.7
  for (n in 1:6) {
    s <- 0.5 * s + 0.1
    x[n] <- s
  }
  expect_equal(pf$filtered.mean, x, tolerance = 1e-14)
  expect_equal(pf$loglik, sum(stats::dnorm(pc_y, x, 0.4, log = TRUE)), tolerance = 1e-12)
  expect_equal(pf$ess, rep(10, 6), tolerance = 1e-12)
  expect_false(any(pf$resampled))
})

test_that("on a linear-Gaussian model the filter tracks the Kalman solution", {
  a <- 0.8
  q <- 0.3
  r <- 0.4
  init <- function(e) .ghc_norm(e, 1L, 0, 1)
  step <- function(x, n, e) a * x + .ghc_norm(e, 1L, 0, sqrt(q))
  lik <- function(x, y, n) stats::dnorm(y, x, sqrt(r), log = TRUE)
  kf <- morie_prtcl_kalman_filter_1d(pc_y, a, q, 1, r, 0, 1)
  pf <- morie_prtcl_particle_filter(pc_y, 3000, init, step, lik, seed = 2, resample.threshold = 0.5)
  # Monte Carlo error with 3000 particles is O(0.02); 0.1 is 5 standard errors
  expect_lt(max(abs(pf$filtered.mean - kf$means)), 0.1)
  expect_lt(abs(pf$loglik - kf$loglik), 0.1)
  pm <- morie_prtcl_particle_filter(pc_y, 500, init, step, lik, seed = 2, systematic = FALSE)
  expect_false(pm$systematic)
  expect_true(all(pm$resampled))
  expect_identical(particlefilter, morie_prtcl_particle_filter)
  expect_error(morie_prtcl_particle_filter(pc_y, 1, init, step, lik), "at least 2")
  expect_error(morie_prtcl_particle_filter(numeric(0), 5, init, step, lik), "no observations")
  expect_error(morie_prtcl_particle_filter(pc_y, 5, init, step, lik, resample.threshold = 0), "resample")
  expect_error(morie_prtcl_particle_filter(pc_y, 5, init, step, function(x, y, n) -Inf),
               "zero likelihood")
  expect_match(prtcl_cheatsheet(), "DOWNWARD", fixed = TRUE)
})
