# Coverage for slice sampling (Neal 2003, Figs. 3 and 5) and Gibbs with
# slice updates (Damien, Wakefield & Walker 1999): one stepping-out /
# shrinkage update regenerated from the counter generator, chains as
# repeated updates, the truncated-autocorrelation ESS, and a Gibbs sweep
# on a correlated Gaussian checked on invariant moments.

.step <- function(logf, x0, e, w = 1, m = 50, lo = -1e300, hi = 1e300) {
  u <- .ghc_unif(e, 1)
  while (u <= 0) u <- .ghc_unif(e, 1)
  y <- logf(x0) + log(u)
  L <- x0 - .ghc_unif(e, 1) * w
  R <- L + w
  J <- floor(m * .ghc_unif(e, 1))
  K <- m - 1 - J
  while (J > 0 && L > lo && logf(L) > y) {
    L <- L - w
    J <- J - 1
  }
  while (K > 0 && R < hi && logf(R) > y) {
    R <- R + w
    K <- K - 1
  }
  L <- max(L, lo)
  R <- min(R, hi)
  repeat {
    x1 <- L + .ghc_unif(e, 1) * (R - L)
    if (logf(x1) > y) return(x1)
    if (x1 < x0) L <- x1 else R <- x1
  }
}

test_that("one slice update follows stepping out and shrinkage", {
  lf <- function(x) -0.5 * x^2
  for (s in 1:3) {
    r <- morie_slice_sample_1d(lf, 0.3, .ghc_rng(s), w = 0.7)
    expect_equal(r$x, .step(lf, 0.3, .ghc_rng(s), w = 0.7), tolerance = 1e-12)
    expect_gt(lf(r$x), -Inf)
  }
  b <- morie_slice_sample_1d(function(x) -x, 0.5, .ghc_rng(2), w = 2, lower = 0, upper = 3)
  expect_equal(b$x, .step(function(x) -x, 0.5, .ghc_rng(2), w = 2, lo = 0, hi = 3), tolerance = 1e-12)
  expect_gte(b$interval[1], 0)
  expect_error(morie_slice_sample_1d(lf, 0, .ghc_rng(1), w = 0), "width must be positive")
  expect_error(morie_slice_sample_1d(function(x) -Inf, 0, .ghc_rng(1)), "zero density")
})

test_that("a chain repeats the update with burn-in and thinning", {
  lf <- function(x) stats::dgamma(x, 3, 2, log = TRUE)
  ch <- morie_slice_chain(lf, 1, n = 6, burn = 2, thin = 2, seed = 4, lower = 0)
  e <- .ghc_rng(4)
  x <- 1
  keep <- numeric(0)
  for (i in 0:13) {
    x <- .step(lf, x, e, lo = 0)
    if (i >= 2 && (i - 2) %% 2 == 0) keep <- c(keep, x)
  }
  expect_equal(ch$draws, keep, tolerance = 1e-12)
  expect_equal(ch$evals_per_draw, ch$n_eval / 6)
  expect_error(morie_slice_chain(lf, 1, n = 0), "at least one draw")
  expect_error(morie_slice_chain(lf, 1, thin = 0), "thin must be at least 1")
})

test_that("ESS sums autocorrelations while they exceed 0.05", {
  set.seed(1)
  v <- as.numeric(stats::arima.sim(list(ar = 0.6), 300))
  r <- stats::acf(v, lag.max = 298, plot = FALSE)$acf[-1]
  k <- which(r < 0.05)[1]
  expect_equal(morie_ess_ipseq(v), 300 / (1 + 2 * sum(r[seq_len(k - 1)])), tolerance = 1e-10)
  expect_identical(morie_ess_ipseq(rep(2, 5)), 5)
  expect_error(morie_ess_ipseq(1:3), "too few draws")
})

test_that("Gibbs with slice updates targets a correlated Gaussian", {
  rho <- 0.6
  lc <- list(function(v, s) -0.5 * (v - rho * s[2])^2 / (1 - rho^2),
             function(v, s) -0.5 * (v - rho * s[1])^2 / (1 - rho^2))
  g <- morie_gibbs_slice(lc, c(0, 0), n = 3000, burn = 100, seed = 3)
  M <- do.call(rbind, g$draws)
  expect_identical(g$n_draws, 3000L)
  expect_equal(g$mean, colMeans(M))
  # invariants of N(0, [[1, .6], [.6, 1]]) on ESS-based standard errors
  z <- c(colMeans(M) / sqrt(1 / g$ess), (colMeans(M^2) - 1) / sqrt(2 / g$ess),
         (mean(M[, 1] * M[, 2]) - rho) / sqrt((1 + rho^2) / min(g$ess)))
  expect_lt(max(abs(z)), 4)
  h <- morie_hybrid_gibbs_slice(lc, c(0, 0), n = 5, seed = 3, w = c(1, 2), bounds = list(c(-5, 5), c(-5, 5)))
  expect_identical(h$n_draws, 5L)
  expect_error(morie_gibbs_slice(lc[1], c(0, 0)), "1 conditionals for 2")
  expect_error(morie_gibbs_slice(list(), numeric(0)), "no coordinates")
  expect_error(morie_gibbs_slice(lc, c(0, 0), w = 1:3), "w has length 3")
  expect_error(morie_gibbs_slice(lc, c(0, 0), bounds = list(c(0, 1))), "bounds has length 1")
})
