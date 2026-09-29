# Coverage for the MCMC sampler dispatch: the choice rule, Metropolis,
# Gibbs and HMC chains regenerated step for step from the same counter
# generator (random-walk / conditional-normal / leapfrog recursions written
# out independently), NUTS checked on invariant functionals of a Gaussian
# target, and the effective sample size against Geyer's (1992) initial
# positive sequence computed from stats::acf.

.lp <- function(x) -0.5 * sum(x^2 / c(1, 4))
.gr <- function(x) -x / c(1, 4)

test_that("the dispatch rule follows gradient, conditionals and dimension", {
  expect_identical(morie_baysmplr_choose(25, TRUE)$sampler, "nuts")
  expect_identical(morie_baysmplr_choose(19, TRUE)$sampler, "hmc")
  expect_identical(morie_baysmplr_choose(19, TRUE, nuts_threshold = 10)$sampler, "nuts")
  expect_identical(morie_baysmplr_choose(3, FALSE, TRUE)$sampler, "gibbs")
  expect_identical(morie_baysmplr_choose(5, FALSE)$sampler, "mh")
  big <- morie_baysmplr_choose(6, FALSE)
  expect_identical(big$sampler, "mh")
  expect_match(big$reason, "mix badly")
  expect_match(morie_baysmplr_cheatsheet(), "mh, gibbs, hmc, nuts")
})

test_that("Metropolis draws follow the random walk with scale 2.38/sqrt(d)", {
  x <- c(0.5, -1)
  e <- .ghc_rng(7)
  lp <- .lp(x)
  acc <- 0
  ref <- vector("list", 40)
  for (it in 1:40) {
    prop <- x + 2.38 / sqrt(2) * .ghc_norm(e, 2)
    a <- .lp(prop) - lp
    if (a >= 0 || log(.ghc_unif(e, 1)) < a) {
      x <- prop
      lp <- .lp(prop)
      acc <- acc + 1
    }
    ref[[it]] <- x
  }
  r <- morie_baysmplr_mh(.lp, c(0.5, -1), 40, .ghc_rng(7))
  expect_equal(r$draws, ref, tolerance = 1e-12)
  expect_equal(r$accept, acc / 40)
  d <- morie_baysmplr(.lp, x0 = c(0.5, -1), n_iter = 40, burn = 10, seed = 7)
  expect_identical(d$sampler, "mh")
  expect_equal(d$draws, ref[11:40], tolerance = 1e-12)
  M <- do.call(rbind, ref[11:40])
  expect_equal(d$mean, colMeans(M), tolerance = 1e-12)
  expect_equal(d$sd, apply(M, 2, stats::sd), tolerance = 1e-12)
  expect_identical(d$estimate, d$mean[1])
  ad <- morie_baysmplr_mh(.lp, c(0, 0), 20, .ghc_rng(2), scale = 1, adapt = TRUE)
  expect_false(ad$info$scale == 1)
})

test_that("Gibbs draws each coordinate from its normal full conditional", {
  S <- matrix(c(1, 0.8, 0.8, 2), 2)
  Q <- solve(S)
  mu <- c(1, -2)
  e <- .ghc_rng(3)
  x <- c(0, 0)
  ref <- vector("list", 30)
  for (it in 1:30) {
    for (j in 1:2) {
      k <- 3 - j
      x[j] <- mu[j] - Q[j, k] * (x[k] - mu[k]) / Q[j, j] + sqrt(1 / Q[j, j]) * .ghc_norm(e, 1)
    }
    ref[[it]] <- x
  }
  g <- morie_baysmplr_gibbs(mu, Q, c(0, 0), 30, .ghc_rng(3))
  expect_equal(g$draws, ref, tolerance = 1e-12)
  expect_identical(g$accept, 1)
  d <- morie_baysmplr(function(x) 0, x0 = c(0, 0), n_iter = 30, burn = 0, seed = 3, mean = mu, cov_inv = Q)
  expect_identical(d$sampler, "gibbs")
  expect_equal(d$draws, ref, tolerance = 1e-12)
})

test_that("HMC draws follow leapfrog proposals with the energy test", {
  e <- .ghc_rng(5)
  x <- c(1, 1)
  ref <- vector("list", 25)
  acc <- 0
  for (it in 1:25) {
    p <- .ghc_norm(e, 2)
    q <- x
    p1 <- p
    for (s in 1:6) {
      p1 <- p1 + 0.15 * .gr(q)
      q <- q + 0.3 * p1
      p1 <- p1 + 0.15 * .gr(q)
    }
    a <- (.lp(q) - 0.5 * sum(p1^2)) - (.lp(x) - 0.5 * sum(p^2))
    if (a >= 0 || log(.ghc_unif(e, 1)) < a) {
      x <- q
      acc <- acc + 1
    }
    ref[[it]] <- x
  }
  h <- morie_baysmplr_hmc(.lp, .gr, c(1, 1), 25, .ghc_rng(5), eps = 0.3, steps = 6)
  expect_equal(h$draws, ref, tolerance = 1e-12)
  expect_equal(h$accept, acc / 25)
  d <- morie_baysmplr(.lp, .gr, x0 = c(1, 1), n_iter = 25, burn = 5, seed = 5, eps = 0.3, steps = 6)
  expect_identical(d$sampler, "hmc")
  expect_equal(d$draws, ref[6:25], tolerance = 1e-12)
})

test_that("NUTS targets the right distribution", {
  r <- morie_baysmplr(.lp, .gr, x0 = c(0, 0), n_iter = 1200, burn = 200, seed = 11,
                      sampler = "nuts", dual_average = TRUE)
  expect_identical(r$sampler, "nuts")
  expect_identical(r$reason, "forced by the caller, dispatch not consulted")
  # invariant functionals of N(0, diag(1, 4)): |z| < 4 on the ESS-based
  # standard errors of the mean and of the second moment
  M <- do.call(rbind, r$draws)
  z_mean <- colMeans(M) / (c(1, 2) / sqrt(r$ess))
  z_var <- (colMeans(M^2) - c(1, 4)) / (sqrt(2) * c(1, 4) / sqrt(r$ess))
  expect_lt(max(abs(c(z_mean, z_var))), 4)
  expect_gt(r$accept_rate, 0)
  expect_lte(r$accept_rate, 1)
  expect_true(is.finite(r$info$eps) && r$info$eps > 0)
  n <- morie_baysmplr_nuts(.lp, .gr, c(0, 0), 5, .ghc_rng(1), max_depth = 1)
  expect_equal(n$info$mean_depth, 1)
  expect_identical(morie_baysmplr(.lp, .gr, x0 = rep(0, 20), n_iter = 4, seed = 1)$sampler, "nuts")
})

.geyer <- function(v, max_lag = 200) {
  n <- length(v)
  r <- as.numeric(stats::acf(v, lag.max = min(n - 1, max_lag), plot = FALSE)$acf)
  tot <- 0
  k <- 1
  while (k + 1 <= length(r) && r[k] + r[k + 1] > 0) {
    tot <- tot + r[k] + r[k + 1]
    k <- k + 2
  }
  n / (-1 + 2 * tot)
}

test_that("ESS is n / (-1 + 2 sum of initial positive Geyer pairs)", {
  set.seed(4)
  a <- as.numeric(stats::arima.sim(list(ar = 0.7), 400))
  b <- as.numeric(stats::arima.sim(list(ar = -0.5), 400))
  ch <- Map(c, a, b)
  es <- morie_baysmplr_ess(ch)
  # compensated sums inside vs plain acf sums: ~1e-13 relative apart
  expect_equal(es, c(.geyer(a), .geyer(b)), tolerance = 1e-10)
  # an antithetic chain is more informative than independent draws
  expect_gt(es[2], 400)
  expect_lt(es[1], 400)
  expect_equal(morie_baysmplr_ess(Map(c, a, b), max_lag = 5), c(.geyer(a, 5), .geyer(b, 5)), tolerance = 1e-10)
  expect_identical(morie_baysmplr_ess(lapply(1:5, function(i) 2)), 5)
})

test_that("dispatch argument checks", {
  expect_error(morie_baysmplr(.lp), "x0 is required")
  expect_error(morie_baysmplr(.lp, x0 = numeric(0)), "non-empty")
  expect_error(morie_baysmplr(.lp, x0 = 0, n_iter = 10, burn = 10), "burn-in consumes")
  expect_error(morie_baysmplr(.lp, x0 = 0, sampler = "slice"), "sampler must be one of")
  expect_error(morie_baysmplr(.lp, x0 = 0, sampler = "hmc"), "hmc needs a gradient")
  expect_error(morie_baysmplr(.lp, x0 = 0, sampler = "gibbs"), "gibbs needs mean and cov_inv")
})
