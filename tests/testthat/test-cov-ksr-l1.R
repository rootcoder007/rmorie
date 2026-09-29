# Coverage for the Kosorok empirical-process shelf (empirical process,
# Glivenko-Cantelli, bootstrap and multiplier bootstrap, Z- and one-step
# estimators), the L1 median, the ridge penalty, the exponential kernel,
# the Laplace mechanism and normalised-Laplacian eigenmaps.

test_that("Empproc and Glivenko", {
  x <- c(0.3, 1.2, -0.5, 0.8, 2.1, 0.1)
  t <- c(-1, 0, 0.5, 1, 2.5)
  F <- pnorm(t)
  r <- Empproc(x, t, F)
  Fn <- ecdf(x)(t)
  expect_equal(r$Fn, Fn, tolerance = 1e-12)
  expect_equal(r$Gn, sqrt(6) * (Fn - F), tolerance = 1e-12)
  expect_equal(r$cov, outer(F, F, pmin) - outer(F, F), tolerance = 1e-12)
  expect_error(Empproc(x, rev(t), F), "non-decreasing")
  g <- Glivenko(x, pnorm(x))
  ks <- ks.test(x, "pnorm")
  expect_equal(g$statistic, unname(ks$statistic), tolerance = 1e-12)
  expect_equal(g$dkw_bound, min(1, 2 * exp(-2 * 6 * g$statistic^2)), tolerance = 1e-12)
  expect_equal(morie_kosorok_glivenko_cantelli(x, pnorm(x))$d_plus,
               unname(ks.test(x, "pnorm", alternative = "greater")$statistic), tolerance = 1e-12)
  expect_error(Glivenko(x, 0.5), "same length")
})

test_that("Bootemp and Multboot replay the Park-Miller stream", {
  x <- c(2.1, 3.5, 1.8, 4.2, 2.9, 3.3)
  lcg <- function(s) {
    function() {
      s <<- (48271 * s) %% 2147483647
      s / 2147483647
    }
  }
  u <- lcg(9)
  st <- vapply(1:20, function(b) mean(x[vapply(1:6, function(i) min(floor(u() * 6), 5), 0) + 1]), 0)
  r <- Bootemp(x, B = 20, seed = 9)
  q <- sort(st)
  expect_equal(r$boot_mean, mean(st), tolerance = 1e-12)
  expect_equal(r$boot_sd, sd(st), tolerance = 1e-12)
  expect_equal(c(r$ci_lower, r$ci_upper), c(q[floor(0.025 * 19) + 1], q[ceiling(0.975 * 19) + 1]), tolerance = 1e-12)
  u <- lcg(4)
  sm <- vapply(1:15, function(b) {
    w <- vapply(1:6, function(i) -log(u()), 0)
    sum(w / mean(w) * x) / 6
  }, 0)
  m <- Multboot(x, B = 15, seed = 4)
  expect_equal(m$boot_sd, sd(sm), tolerance = 1e-12)
  expect_equal(m$process_sd, sqrt(6) * sd(sm), tolerance = 1e-12)
  expect_error(Bootemp(1), "two observations")
  expect_error(Multboot(x, B = 1), "B must")
})

test_that("Zestim bisects the estimating equation and Onestep takes one Newton step", {
  x <- c(0.2, 1.1, -0.4, 5.0, 0.7, 0.9, -0.1)
  expect_equal(Zestim(x, "mean")$estimate, mean(x), tolerance = 1e-12)
  expect_equal(Zestim(x, "median")$estimate, median(x), tolerance = 1e-12)
  h <- Zestim(x, "huber", k = 1)
  th <- uniroot(function(t) sum(pmax(-1, pmin(1, x - t))), range(x), tol = 1e-14)$root
  expect_equal(h$estimate, th, tolerance = 1e-12)
  expect_lt(abs(h$psi_at_estimate), 1e-12)
  expect_equal(Zestim(rep(2, 3))$estimate, 2)
  expect_error(Zestim(x, "trim"), "kind must be")
  o <- Onestep(x, theta0 = 0.5, kind = "huber", k = 1)
  psi <- pmax(-1, pmin(1, x - 0.5))
  expect_equal(o$estimate, 0.5 + mean(psi) / mean(abs(x - 0.5) <= 1), tolerance = 1e-12)
  expect_equal(Onestep(x, 0, "mean")$estimate, mean(x), tolerance = 1e-12)
  expect_error(Onestep(c(10, 11), 0, "huber", k = 1), "derivative is zero")
})

test_that("L1med is the Weiszfeld spatial median", {
  X <- rbind(c(0, 0), c(4, 0), c(0, 3), c(1, 1), c(10, 10))
  r <- L1med(X)
  f <- function(m) sum(sqrt(rowSums((X - matrix(m, 5, 2, byrow = TRUE))^2)))
  expect_equal(r$cost, f(r$estimate), tolerance = 1e-12)
  u <- (X - matrix(r$estimate, 5, 2, byrow = TRUE)) / sqrt(rowSums((X - matrix(r$estimate, 5, 2, byrow = TRUE))^2))
  expect_equal(r$spatial_rank_norm, sqrt(sum(colSums(u)^2)), tolerance = 1e-12)
  # the median sits on the data point (1, 1): optimal iff the other unit vectors sum to norm <= 1
  expect_lte(r$spatial_rank_norm, 1)
  sq <- L1med(rbind(c(0, 0), c(2, 0), c(0, 2), c(2, 2), c(1, 3)))
  # off the data the subgradient must vanish; 200 linear-rate Weiszfeld sweeps leave it small
  expect_lt(sq$spatial_rank_norm, 1e-6)
  op <- optim(colMeans(X), f, method = "BFGS", control = list(reltol = 1e-14))
  expect_lte(r$cost, op$value + 1e-9)
})

test_that("L2pen, Expkern and Laplc", {
  l <- L2pen(3.2, c(1, -2, 0.5), 0.4)
  expect_equal(l$penalized_loss, 3.2 + 0.2 * 5.25, tolerance = 1e-12)
  expect_error(L2pen(1, 1, -1), "non-negative")
  X <- rbind(c(0, 0), c(1, 2), c(3, 1))
  e <- Expkern(X, gamma = 0.7)
  expect_equal(e$K, exp(-0.7 * as.matrix(dist(X))), tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(Expkern(X)$gamma, 0.5)
  Z <- rbind(c(1, 1))
  expect_equal(Expkern(X, 0.7, Z)$K, matrix(exp(-0.7 * sqrt(colSums((t(X) - c(1, 1))^2))), 3), tolerance = 1e-12)
  lp <- Laplc(c(10, 20, 30), sensitivity = 2, epsilon = 0.5, seed = 7)
  set.seed(7)
  u <- runif(3) - 0.5
  expect_equal(lp$release, c(10, 20, 30) - 4 * sign(u) * log1p(-2 * abs(u)), tolerance = 1e-12)
  expect_equal(lp$noise_scale, 4)
  expect_error(Laplc(1, epsilon = 0), "epsilon")
})

test_that("Lapeig returns the smallest normalised-Laplacian eigenpairs", {
  W <- matrix(0, 6, 6)
  W[1, 2] <- W[2, 3] <- W[1, 3] <- 1
  W[4, 5] <- W[5, 6] <- 1
  W[3, 4] <- 0.1
  W <- W + t(W)
  r <- Lapeig(W, k = 3)
  d <- rowSums(W)
  Ln <- diag(6) - diag(1 / sqrt(d)) %*% W %*% diag(1 / sqrt(d))
  ev <- sort(eigen(Ln, symmetric = TRUE)$values)
  expect_equal(r$values, ev[1:3], tolerance = 1e-12)
  expect_equal(r$lambda1, ev[2], tolerance = 1e-12)
  expect_equal(abs(crossprod(r$fiedler, Ln %*% r$fiedler))[1], ev[2], tolerance = 1e-12)
  expect_equal(r$n_components, 1L)
  expect_error(Lapeig(W, k = 7), "1 <= k <= n")
})
