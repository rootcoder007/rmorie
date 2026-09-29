# Coverage for GP hyperparameter MCMC (Murray & Adams 2010; Neal 2003):
# the squared-exponential and Matern kernels, the log marginal likelihood
# (against the multivariate-normal density by base chol), one slice
# update regenerated from the counter generator, and the three sampling
# routes, with the predictive mean and variance recomputed from the kept
# hyperparameter draws.

.X <- matrix(c(0, 0.4, 1.1, 1.5, 2.3, 3), ncol = 1)
.y <- sin(.X[, 1]) + c(0.05, -0.1, 0.02, 0.08, -0.04, 0.01)

test_that("kernels follow their closed forms", {
  r <- abs(outer(.X[, 1], .X[, 1], "-")) / exp(0.2)
  sf2 <- exp(2 * -0.3)
  expect_equal(morie_hyper2_kernel(.X, .X, 0.2, -0.3), sf2 * exp(-r^2 / 2), tolerance = 1e-12)
  expect_equal(morie_hyper2_kernel(.X, .X, 0.2, -0.3, "matern32"), sf2 * (1 + sqrt(3) * r) * exp(-sqrt(3) * r), tolerance = 1e-12)
  expect_equal(morie_hyper2_kernel(.X, .X, 0.2, -0.3, "matern52"),
               sf2 * (1 + sqrt(5) * r + 5 * r^2 / 3) * exp(-sqrt(5) * r), tolerance = 1e-12)
  expect_error(morie_hyper2_kernel(.X, .X, 0, 0, "rq"), "kind must be one of")
})

test_that("the log marginal likelihood is log N(y; 0, K + sn^2 I)", {
  K <- morie_hyper2_kernel(.X, .X, 0.1, 0.2) + (exp(2 * -1.5) + 1e-10) * diag(6)
  R <- chol(K)
  ref <- -0.5 * sum(backsolve(R, .y, transpose = TRUE)^2) - sum(log(diag(R))) - 3 * log(2 * pi)
  expect_equal(morie_hyper2_logml(.y, .X, 0.1, 0.2, -1.5, "squared_exponential"), ref, tolerance = 1e-10)
})

test_that("one slice update: stepping out then shrinking", {
  lf <- function(x) -0.5 * (x - 1)^2
  s <- morie_hyper2_slice(lf, 0.2, .ghc_rng(5), w = 0.8, m = 6)
  e <- .ghc_rng(5)
  ly <- lf(0.2) + log(.ghc_unif(e, 1))
  lo <- 0.2 - 0.8 * .ghc_unif(e, 1)
  hi <- lo + 0.8
  j <- floor(.ghc_unif(e, 1) * 6)
  k <- 5 - j
  while (j > 0 && ly < lf(lo)) {
    lo <- lo - 0.8
    j <- j - 1
  }
  while (k > 0 && ly < lf(hi)) {
    hi <- hi + 0.8
    k <- k - 1
  }
  repeat {
    x1 <- lo + .ghc_unif(e, 1) * (hi - lo)
    if (ly < lf(x1)) break
    if (x1 < 0.2) lo <- x1 else hi <- x1
  }
  expect_equal(s, x1, tolerance = 1e-12)
})

test_that("the predictive averages the GP posterior over the kept draws", {
  Xs <- matrix(c(0.7, 2), ncol = 1)
  r <- morie_hyper2(.X, .y, n_iter = 12, burn = 4, thin = 2, seed = 3, Xstar = Xs)
  expect_identical(r$kept, 4L)
  mu <- s2 <- m2 <- numeric(2)
  for (th in r$draws) {
    K <- morie_hyper2_kernel(.X, .X, th[1], th[2]) + (exp(2 * th[3]) + 1e-8) * diag(6)
    Ks <- morie_hyper2_kernel(Xs, .X, th[1], th[2])
    m <- as.numeric(Ks %*% solve(K, .y))
    v <- exp(2 * th[2]) - rowSums((Ks %*% solve(K)) * Ks)
    mu <- mu + m
    m2 <- m2 + m^2
    s2 <- s2 + pmax(v, 0)
  }
  mu <- mu / 4
  expect_equal(r$predict_mean, mu, tolerance = 1e-8)
  expect_equal(r$predict_sd, sqrt(s2 / 4 + m2 / 4 - mu^2), tolerance = 1e-8)
  th <- do.call(rbind, r$draws)
  expect_equal(c(r$log_lengthscale, r$log_signal_sd, r$log_noise_sd), colMeans(th), tolerance = 1e-12)
  expect_equal(r$log_target, vapply(r$draws, function(t) morie_hyper2_logml(.y, .X, t[1], t[2], t[3], "squared_exponential"), 1), tolerance = 1e-10)
  for (route in c("whitened", "surrogate")) {
    w <- morie_hyper2(.X, .y, route = route, n_iter = 4, burn = 2, seed = 1, kind = "matern32")
    expect_identical(w$kept, 2L)
    expect_true(all(is.finite(unlist(w$draws))))
  }
  expect_error(morie_hyper2(.X, .y, route = "laplace"), "route must be one of")
  expect_error(morie_hyper2(.X[1:2, , drop = FALSE], .y[1:2]), "at least three")
  expect_error(morie_hyper2(.X, .y, prior = c(0, 0)), "three starting log values")
  expect_error(morie_hyper2(.X, .y, n_iter = 3, burn = 3), "burn-in consumed")
  expect_match(morie_hyper2_cheatsheet(), "matern52")
})
