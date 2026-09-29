# Coverage tests for R/bnppct_native.R: the mixture CDF and its quantile
# against uniroot, bracket expansion, and the Bayesian-bootstrap, mixture
# and predictive routes of the quantile posterior.

bp_y <- c(1.2, 2.5, 0.7, 3.1, 1.9, 2.2, 4.0, 1.5, 2.8, 0.9)

test_that("mixture CDF, bracket expansion and the quantile root", {
  w <- c(0.2, 0.5, 0.2)
  mu <- c(-1, 0.5, 3)
  s2 <- c(0.5, 1, 2)
  expect_equal(morie_bnppct_cdf(0.3, w, mu, s2), sum(w * pnorm(0.3, mu, sqrt(s2))), tolerance = 1e-12)
  q <- morie_bnppct_quantile(0.4, w, mu, s2)
  ref <- uniroot(function(x) sum(w * pnorm(x, mu, sqrt(s2))) / 0.9 - 0.4, c(-10, 10), tol = 1e-14)$root
  expect_equal(q, ref, tolerance = 1e-9)
  expect_equal(morie_bnppct_quantile(0.4, w, mu, s2, lo = 0, hi = 1), ref, tolerance = 1e-9)
  expect_true(is.nan(morie_bnppct_quantile(0.5, 0, 0, 1)))
  br <- morie_bnppct_expand(function(x) x - 100, 0, 1)
  expect_true(br$lo < 100 && br$hi > 100)
  expect_equal(br$hi - br$lo, 2^ceiling(log2(199)))
  same <- morie_bnppct_expand(function(x) 1, 0, 1, iters = 3)
  expect_equal(c(same$lo, same$hi), c(0.5 - 4, 0.5 + 4))
})

test_that("Bayesian bootstrap route follows the .ghc gamma stream", {
  r <- morie_bnppct(bp_y, quantile = c(0.25, 0.5), route = "bayesian_bootstrap", n_bootstrap = 30, seed = 4, cred = 0.8)
  e <- .ghc_rng(4)
  ys <- sort(bp_y)
  d1 <- d2 <- numeric(30)
  wq <- function(w, q) ys[min(which(cumsum(w) >= q))]
  for (b in 1:30) {
    g <- vapply(1:10, function(i) .ghc_gamma1(e, 1, 1), 0)
    w <- g / sum(g)
    d1[b] <- wq(w, 0.25)
    d2[b] <- wq(w, 0.5)
  }
  expect_equal(r$quantiles[[1]]$draws, d1)
  expect_equal(r$quantiles[[2]]$draws, d2)
  expect_equal(r$estimate, mean(d1), tolerance = 1e-12)
  expect_equal(r$se, sd(d1), tolerance = 1e-12)
  s <- sort(d2)
  expect_equal(r$quantiles[[2]]$lower, s[ceiling(0.1 * 30 - 1e-9)])
  expect_equal(r$quantiles[[2]]$upper, s[ceiling(0.9 * 30 - 1e-9)])
  expect_equal(r$n_draws, 30L)
  expect_null(r$mean_clusters)
})

test_that("mixture and predictive routes use the stick-breaking draws", {
  fit <- morie_slbpdg(bp_y, n_iter = 40, seed = 2, keep_draws = TRUE)
  m <- morie_bnppct(bp_y, 0.5, route = "mixture", n_iter = 40, seed = 2)
  expect_equal(m$quantiles[[1]]$draws, vapply(fit$draws, function(d) morie_bnppct_quantile(0.5, d$w, d$mu, d$s2), 0))
  expect_equal(m$min_mass_carried, min(1, vapply(fit$draws, function(d) sum(d$w), 0)), tolerance = 1e-12)
  expect_equal(m$mean_clusters, fit$mean_clusters)
  p <- morie_bnppct(bp_y, c(0.3, 0.7), route = "predictive", n_iter = 40, seed = 2)
  fbar <- function(x) mean(vapply(fit$draws, function(d) morie_bnppct_cdf(x, d$w, d$mu, d$s2) / sum(d$w), 0))
  expect_equal(fbar(p$quantiles[[1]]$estimate), 0.3, tolerance = 1e-9)
  expect_equal(fbar(p$quantiles[[2]]$estimate), 0.7, tolerance = 1e-9)
  expect_equal(p$se, 0)
  expect_match(morie_bnppct_cheatsheet(), "bayesian_bootstrap")
  expect_error(morie_bnppct(bp_y, route = "dp"), "route must be one of")
  expect_error(morie_bnppct(bp_y, quantile = 1), "strictly inside")
  expect_error(morie_bnppct(bp_y, cred = 1), "cred must lie")
  expect_error(morie_bnppct(1), "at least two observations")
})
