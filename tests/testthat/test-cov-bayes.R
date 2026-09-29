# Coverage for the Bayesian helpers (bay*.R, BayesRegressionSamplers.R):
# closed forms are recomputed from the formulas, optimisers are checked
# at their first-order conditions, and the samplers are replayed on the
# package's Philox / SplitMix64 streams.

test_that("Bayfac converts log evidences to Kass-Raftery categories", {
  r <- Bayfac(-10.2, -12.9)
  expect_equal(r$bf, exp(2.7), tolerance = 1e-12)
  expect_equal(r$two_log_bf, 5.4, tolerance = 1e-12)
  expect_identical(r$category, "positive")
  expect_equal(r$favours, 1L)
  expect_equal(Bayfac(0, 800)$bf, 0)
  expect_equal(Bayfac(0, 800)$bf_21, Inf)
  expect_equal(Bayfac(800, 0)$bf_21, 0)
  expect_identical(Bayfac(0, 0.5)$category, "bare mention")
  expect_identical(Bayfac(0, 4)$category, "strong")
  expect_identical(Bayfac(0, 6)$category, "very strong")
  expect_error(Bayfac(NaN, 1), "NaN")
  expect_error(Bayfac(Inf, Inf), "same infinity")
})

test_that("Bayhier and Baysrnd partially pool group means", {
  y <- c(4.1, 5.2, 3.9, 6.8, 7.1, 6.2, 7.5, 2.1, 2.9)
  g <- c("a", "a", "a", "b", "b", "b", "b", "c", "c")
  r <- Bayhier(y, g)
  ng <- c(3, 4, 2)
  yb <- tapply(y, g, mean)
  s2 <- sum((y - yb[g])^2) / 6
  gm <- mean(y)
  msb <- sum(ng * (yb - gm)^2) / 2
  nt <- (9 - sum(ng^2) / 9) / 2
  t2 <- max((msb - s2) / nt, 0)
  lam <- t2 / (t2 + s2 / ng)
  w <- 1 / (t2 + s2 / ng)
  mu <- sum(w * yb) / sum(w)
  expect_equal(c(r$sigma2, r$tau2), c(s2, t2), tolerance = 1e-12)
  expect_equal(r$theta, as.numeric(lam * yb + (1 - lam) * mu), tolerance = 1e-12)
  r2 <- Bayhier(y, g, sigma2 = 1, tau2 = 0)
  expect_equal(r2$lambda_g, c(0, 0, 0))
  expect_equal(r2$theta, rep(sum(ng * yb) / 9, 3), tolerance = 1e-12)
  expect_error(Bayhier(numeric(0), character(0)), "empty")
  expect_error(Bayhier(y, g[-1]), "different lengths")
  expect_error(Bayhier(y, rep("a", 9)), "two groups")
  expect_error(Bayhier(y[1:2], c("a", "b")), "residual degrees")
  expect_error(Bayhier(y, g, sigma2 = -1), "non-negative")
  expect_error(Bayhier(y, g, tau2 = -1), "non-negative")
  s <- Baysrnd(y, group = g)
  expect_equal(s$u_g, as.numeric(lam * (yb - mu)), tolerance = 1e-12)
  expect_equal(Baysrnd(y, X = g)$u_g, s$u_g)
  expect_error(Baysrnd(y), "grouping vector")
})

test_that("Baysrr is ridge on centred markers", {
  M <- cbind(c(0, 1, 2, 1, 0, 2, 1), c(1, 1, 0, 2, 2, 0, 1), c(2, 0, 1, 1, 0, 1, 2))
  y <- c(1.2, 2.3, 3.1, 2.8, 1.1, 3.5, 2.2)
  r <- Baysrr(y, M, lam = 0.7)
  Mc <- sweep(M, 2, colMeans(M))
  b <- solve(crossprod(Mc) + 0.7 * diag(3), crossprod(Mc, y - mean(y)))
  expect_equal(r$beta, as.numeric(b), tolerance = 1e-10)
  lam0 <- (0.5 * var(y)) / (0.5 * var(y) / 3)
  expect_equal(Baysrr(y, M)$lam, lam0, tolerance = 1e-12)
  expect_error(Baysrr(y), "marker matrix")
})

test_that("Bayth integrates prior x likelihood by Simpson's rule", {
  k <- 7
  n <- 12
  r <- Bayth(c(k, n), function(t) dbeta(t, 2, 3), function(t, y) dbinom(y[1], y[2], t))
  expect_equal(r$post_mean, (2 + k) / (5 + n), tolerance = 1e-9)
  a <- 2 + k
  b <- 3 + n - k
  expect_equal(r$post_var, a * b / ((a + b)^2 * (a + b + 1)), tolerance = 1e-9)
  expect_equal(r$marginal, choose(n, k) * beta(a, b) / beta(2, 3), tolerance = 1e-9)
  expect_equal(length(Bayth(1, dunif, function(t, y) 1, n_grid = 10)$theta), 11L)
  expect_error(Bayth(1, 1, dunif), "callables")
  expect_error(Bayth(1, dunif, function(t, y) 1, grid = c(1, 0)), "positive width")
  expect_error(Bayth(1, dunif, function(t, y) 1, n_grid = 2), "at least 3")
  expect_error(Bayth(1, function(t) -1, function(t, y) 1), "negative density")
  expect_error(Bayth(1, dunif, function(t, y) 0), "not positive")
})

test_that("Bayeslogit is the Gaussian-prior posterior mode", {
  X <- cbind(c(0.5, -1.2, 0.3, 1.8, -0.7, 0.9, -1.5, 0.2, 1.1, -0.4))
  y <- c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1)
  r <- Bayeslogit(X, y, prior_sd = 2)
  Z <- cbind(1, X)
  mu <- plogis(as.numeric(Z %*% r$estimate))
  g <- crossprod(Z, y - mu) - r$estimate / 4
  expect_lt(max(abs(g)), 1e-10)
  H <- crossprod(Z * (mu * (1 - mu)), Z) + diag(1 / 4, 2)
  expect_equal(r$se, sqrt(diag(solve(H))), tolerance = 1e-10)
  expect_equal(r$log_posterior, sum(dbinom(y, 1, mu, log = TRUE)) - sum(r$estimate^2) / 8,
               tolerance = 1e-10)
  flat <- Bayeslogit(X, y, prior_sd = 1e6)
  gl <- glm(y ~ X, family = binomial(), control = list(epsilon = 1e-14, maxit = 100))
  expect_equal(flat$estimate, unname(coef(gl)), tolerance = 1e-8)
  expect_error(Bayeslogit(X, y[-1]), "same number")
  expect_error(Bayeslogit(X, y + 1), "binary")
  expect_error(Bayeslogit(X, y, prior_sd = 0), "positive")
  expect_error(Bayeslogit(X[1, , drop = FALSE], 1), "more coefficients")
})

test_that("Baysr runs the BayesR EM sweep", {
  X <- cbind(c(0, 1, 2, 1, 0, 2, 1, 0), c(1, 1, 0, 2, 2, 0, 1, 1), c(2, 0, 1, 1, 0, 1, 2, 0))
  y <- c(1.2, 2.3, 3.1, 2.8, 1.1, 3.5, 2.2, 0.9)
  sc <- c(0, 0.1, 1)
  em <- function(iters) {
    mu <- mean(y)
    p <- 3
    K <- 3
    pv <- rep(1 / 3, 3)
    beta <- numeric(p)
    xtx <- colSums(X^2)
    vy <- var(y)
    sg2 <- vy / 2
    se2 <- vy / 2
    gam <- matrix(0, p, K)
    res <- y - mu
    for (it in seq_len(iters)) {
      for (j in 1:p) {
        res <- res + X[, j] * beta[j]
        xr <- sum(X[, j] * res)
        s2 <- sc * sg2
        den <- xtx[j] * s2 + se2
        lg <- log(pv) + 0.5 * log(se2 / den) + xr^2 * s2 / (2 * se2 * den)
        gam[j, ] <- exp(lg - max(lg)) / sum(exp(lg - max(lg)))
        beta[j] <- sum(gam[j, ] * xr * s2 / den)
        res <- res - X[, j] * beta[j]
      }
      pv <- (colSums(gam) + 1) / (p + K)
      sg2 <- sum(gam[, 2:3] * outer(beta^2, 1 / sc[2:3])) / sum(gam[, 2:3])
      se2 <- sum(res^2) / length(y)
    }
    list(beta = beta, pi = pv, sg2 = sg2, se2 = se2)
  }
  r <- Baysr(y, X, sigma_classes = sc, max_iter = 3L, tol = 0)
  ref <- em(3)
  expect_equal(r$beta_samples, ref$beta, tolerance = 1e-12)
  expect_equal(r$pi, ref$pi, tolerance = 1e-12)
  expect_equal(c(r$sigma_g2, r$sigma_e2), c(ref$sg2, ref$se2), tolerance = 1e-12)
  expect_error(Baysr(1, 1), "two observations")
  expect_error(Baysr(y, X[-1, ]), "different number of rows")
  expect_error(Baysr(y, matrix(numeric(0), 8, 0)), "no columns")
  expect_error(Baysr(y, X, sigma_classes = 0), "two variance classes")
  expect_error(Baysr(y, X, sigma_classes = c(1, 2)), "exactly 0")
  expect_error(Baysr(y, X, sigma_classes = c(0, -1)), "non-negative")
  expect_error(Baysr(y, X, pi = c(0.5, 0.5)), "one entry per")
  expect_error(Baysr(y, X, pi = c(-1, 1, 1, 1)), "non-negative")
  expect_error(Baysr(y, X, pi = c(0, 0, 0, 0)), "positive")
  expect_error(Baysr(y, X, delta = 1), "one entry per")
  expect_error(Baysr(y, X, delta = c(0, 1, 1, 1)), "positive")
})

test_that("morie_bayreg2 reaches the Student-t EM fixed point", {
  x <- c(0.1, 0.5, 0.9, 1.3, 1.7, 2.1, 2.5, 2.9, 3.3, 3.7)
  y <- 1 + 2 * x + c(0.1, -0.2, 0.05, 3, -0.1, 0.15, -0.05, 0.2, -4, 0.1)
  r <- morie_bayreg2_student_t_regression(x, y, nu = 4)
  Z <- cbind(1, x)
  w <- r$weights
  b <- solve(crossprod(Z * w, Z) + diag(1e-12, 2), crossprod(Z, w * y))
  expect_equal(r$coefficients, as.numeric(b), tolerance = 1e-9)
  res <- y - as.numeric(Z %*% r$coefficients)
  expect_equal(r$loglik, sum(dt(res / sqrt(r$scale2), 4, log = TRUE) - 0.5 * log(r$scale2)),
               tolerance = 1e-9)
  expect_true(r$converged)
  expect_equal(morie_bayreg2_student_t_regression(x, y, nu = 1e9, max_iter = 1)$coefficients,
               as.numeric(qr.solve(Z, y)), tolerance = 1e-8)
  expect_error(morie_bayreg2_student_t_regression(matrix(numeric(0), 0, 1), numeric(0)),
               "no observations")
  expect_error(morie_bayreg2_student_t_regression(x, y[-1]), "responses")
  expect_error(morie_bayreg2_student_t_regression(x, y, nu = 0), "positive")
  expect_error(morie_bayreg2_student_t_regression(x[1:2], y[1:2]), "cannot identify")
})

test_that("morie_baytsm runs the local-level filter and smoother", {
  y <- c(1.2, 0.8, 1.9, 2.4, 2.1, 3.0, 2.7)
  r <- morie_baytsm_dlm_local_level(y, V = 0.5, W = 0.2, m0 = 1, C0 = 2)
  m <- 1
  C <- 2
  ms <- Cs <- Rs <- numeric(7)
  ll <- 0
  for (t in 1:7) {
    R <- C + 0.2
    Q <- R + 0.5
    ll <- ll + dnorm(y[t], m, sqrt(Q), log = TRUE)
    m <- m + R / Q * (y[t] - m)
    C <- R - R^2 / Q
    ms[t] <- m
    Cs[t] <- C
    Rs[t] <- R
  }
  expect_equal(r$filtered, ms, tolerance = 1e-12)
  expect_equal(r$filtered_var, Cs, tolerance = 1e-12)
  expect_equal(r$loglik, ll, tolerance = 1e-12)
  sm <- ms
  for (t in 6:1) sm[t] <- ms[t] + Cs[t] / Rs[t + 1] * (sm[t + 1] - ms[t])
  expect_equal(r$smoothed, sm, tolerance = 1e-12)
  expect_error(morie_baytsm_dlm_local_level(numeric(0)), "empty")
  expect_error(morie_baytsm_dlm_local_level(y, V = 0), "positive")
  expect_error(morie_baytsm_dlm_local_level(y, W = -1), "negative")
})

test_that("Baynet variable elimination equals brute-force enumeration", {
  graph <- list(A = character(0), B = "A", C = c("A", "B"))
  cpts <- list(A = c(0.3, 0.7),
               B = list(c(0.9, 0.1), c(0.4, 0.6)),
               C = list(list(c(0.8, 0.2), c(0.5, 0.5)), list(c(0.3, 0.7), c(0.1, 0.9))))
  joint <- function(a, b, c) cpts$A[a + 1] * cpts$B[[a + 1]][b + 1] * cpts$C[[a + 1]][[b + 1]][c + 1]
  r <- Baynet(graph, cpts, evidence = list(C = 1), query = "A")
  pa <- vapply(0:1, function(a) joint(a, 0, 1) + joint(a, 1, 1), 0)
  expect_equal(r$posterior, pa / sum(pa), tolerance = 1e-12)
  expect_equal(r$estimate, which.max(pa) - 1L)
  rb <- bayes_network(graph, cpts, query = "B")
  pb <- vapply(0:1, function(b) sum(outer(0:1, 0:1, Vectorize(function(a, c) joint(a, b, c)))), 0)
  expect_equal(rb$posterior, pb, tolerance = 1e-12)
  expect_error(Baynet(graph, cpts), "required")
  expect_error(Baynet(graph, cpts, query = "Z"), "unknown query")
  expect_error(Baynet(graph, cpts, evidence = list(Z = 0), query = "A"), "unknown evidence")
  expect_error(Baynet(graph, cpts, evidence = list(C = 2), query = "A"), "out of range")
  expect_error(Baynet(graph, list(A = c(0, 0), B = cpts$B, C = cpts$C), query = "B"),
               "zero-probability")
})

test_that("Dpbayes releases one posterior draw on the Philox stream", {
  post <- c(0.2, 0.5, 0.9, 1.4, 0.7)
  r <- Dpbayes(1:10, posterior_sample = post, epsilon = 0.5, B = 2, seed = 4)
  j <- min(floor(.morie_random_uniform(1, seed = 4, stream = 0) * 5), 4)
  expect_equal(r$draw_index, j)
  expect_equal(r$released, post[j + 1])
  expect_equal(r$temperature, 8)
  expect_equal(r$laplace_scale, 0.2, tolerance = 1e-12)
  expect_equal(Dpbayes(1:10, post, epsilon = 0)$temperature, Inf)
  expect_error(Dpbayes(1:3), "posterior_sample is required")
  expect_error(Dpbayes(1:3, numeric(0)), "empty")
})

test_that("morie_bayisr resamples by importance weight", {
  xs <- as.list(c(-1, 0, 0.5, 1.5, 2))
  r <- morie_bayisr(xs, function(z) dnorm(z, 1, log = TRUE), function(z) dnorm(z, 0, 2, log = TRUE),
                    m = 6, seed = 2)
  lw <- vapply(unlist(xs), function(z) dnorm(z, 1, log = TRUE) - dnorm(z, 0, 2, log = TRUE), 0)
  w <- exp(lw - max(lw))
  expect_equal(r$weights, w / sum(w), tolerance = 1e-12)
  expect_equal(r$ess, 1 / sum((w / sum(w))^2), tolerance = 1e-12)
  e <- .ghc_rng(2)
  u <- .ghc_unif(e, 6) * sum(w)
  idx <- vapply(u, function(v) which(v <= cumsum(w))[1] - 1L, 0L)
  expect_equal(r$indices, idx)
  expect_equal(unlist(r$resample), unlist(xs)[idx + 1])
  expect_error(morie_bayisr(list(), identity, identity, 1), "non-empty")
  expect_error(morie_bayisr(xs, identity, identity, 0), "positive integer")
})

test_that("morie_bayoptr: initial design, GP acquisition and trace", {
  f <- function(x) (x[1] - 0.3)^2
  r <- morie_bayoptr(f, list(c(0, 1)), acquisition = "ucb", n_iter = 2L, n_init = 3L,
                     n_candidates = 2L, seed = 5L, length_scale = 0.4)
  e <- .ghc_rng(5)
  x0 <- .ghc_unif(e, 3, 0, 1)
  expect_equal(r$X[1:3, 1], x0, tolerance = 1e-12)
  expect_equal(r$y, apply(r$X, 1, f), tolerance = 1e-12)
  expect_equal(r$best_y, min(r$y), tolerance = 1e-12)
  expect_identical(r$acq, "lcb")
  m52 <- function(d) (1 + sqrt(5) * d / 0.4 + 5 * d^2 / (3 * 0.16)) * exp(-sqrt(5) * d / 0.4)
  gp <- function(X, y, x) {
    K <- m52(abs(outer(X, X, "-"))) + diag(1e-8, length(X))
    k <- m52(abs(X - x))
    c(sum(k * solve(K, y)), sqrt(max(0, 1 - sum(k * solve(K, k)))))
  }
  lcb <- function(x) {
    g <- gp(r$X[1:3, 1], r$y[1:3], x)
    g[1] - 2 * g[2]
  }
  x1 <- r$x_trace[[1]][1, 1]
  h <- 1e-4
  # the L-BFGS-B pick minimises the lower confidence bound locally; its
  # default factr stops within ~1e-8 of the optimum value
  expect_lte(lcb(x1), min(lcb(max(x1 - h, 0)), lcb(min(x1 + h, 1))) + 1e-7)
  ex <- morie_bayoptr(f, list(c(0, 1)), n_iter = 1L, n_init = 3L, n_candidates = 2L,
                      seed = 5L, length_scale = 0.4)
  eif <- function(x) {
    g <- gp(ex$X[1:3, 1], ex$y[1:3], x)
    z <- (min(ex$y[1:3]) - g[1]) / g[2]
    g[2] * (z * pnorm(z) + dnorm(z))
  }
  xe <- ex$x_trace[[1]][1, 1]
  gpk <- .gp_posterior(matrix(ex$X[1:3, 1]), ex$y[1:3], matrix(c(0.05, xe)), 1, 0.4, 1e-8)
  expect_equal(c(gpk$mu[2], gpk$sd[2]), gp(ex$X[1:3, 1], ex$y[1:3], xe), tolerance = 1e-9)
  expect_equal(gpk$mu[1], gp(ex$X[1:3, 1], ex$y[1:3], 0.05)[1], tolerance = 1e-9)
  expect_gte(eif(xe), max(eif(max(xe - h, 0)), eif(min(xe + h, 1))) - 1e-7)
  expect_same_function(bayesian_optimization_ei_ucb, morie_bayoptr)
  ei <- morie_bayoptr(f, list(c(0, 1)), acquisition = "pi", n_iter = 1L, n_init = 2L,
                      n_candidates = 1L, X0 = matrix(c(0.1, 0.9)), y0 = c(0.04, 0.36))
  expect_equal(ei$X[1:2, 1], c(0.1, 0.9))
  expect_error(morie_bayoptr(f, list(c(0, 1)), acquisition = "x"), "acquisition must be")
  expect_error(morie_bayoptr(f, list(c(1, 0))), "low < high")
  expect_error(morie_bayoptr(f, list(c(0, 1)), kernel = "rbf"), "matern52")
})

test_that("FiniteMixtureGibbs replays its Gibbs sweep on the Philox stream", {
  y <- c(-2.1, -1.8, -2.4, -1.9, 2.2, 1.9, 2.5, 2.0, -2.0, 2.3)
  K <- 2
  r <- FiniteMixtureGibbs(y, K, ndraw = 3, burn_in = 1, seed = 7)
  blk <- 0
  buf <- numeric(0)
  k0 <- 0
  unif <- function() {
    if (k0 >= length(buf)) {
      buf <<- .morie_random_uniform(4096, seed = 7, stream = blk)
      blk <<- blk + 1
      k0 <<- 0
    }
    k0 <<- k0 + 1
    buf[k0]
  }
  rgam <- function(a) {
    if (a < 1) return(rgam(a + 1) * unif()^(1 / a))
    d <- a - 1 / 3
    cc <- 1 / sqrt(9 * d)
    repeat {
      x <- qnorm(unif())
      v <- (1 + cc * x)^3
      if (v <= 0) next
      u <- unif()
      if (log(u) < 0.5 * x^2 + d - d * v + d * log(v)) return(d * v)
    }
  }
  n <- 10
  m0 <- mean(y)
  s2y <- var(y)
  s0 <- 10 * s2y
  mu <- sort(y)[floor((0:1 + 0.5) * n / K) + 1]
  sig <- rep(s2y, K)
  pp <- rep(0.5, K)
  keep <- matrix(0, 3, 6)
  for (it in 1:4) {
    z <- vapply(1:n, function(i) {
      w <- pp * exp(-0.5 * (y[i] - mu)^2 / sig) / sqrt(sig)
      t <- unif() * sum(w)
      which(cumsum(w) >= t)[1]
    }, 0)
    cnt <- tabulate(z, K)
    gg <- vapply(1:K, function(k) rgam(1 + cnt[k]), 0)
    pp <- gg / sum(gg)
    for (k in 1:K) {
      yk <- y[z == k]
      prec <- 1 / s0 + cnt[k] / sig[k]
      mu[k] <- (m0 / s0 + sum(yk) / sig[k]) / prec + qnorm(unif()) / sqrt(prec)
      sig[k] <- (s2y + 0.5 * sum((yk - mu[k])^2)) / rgam(2 + cnt[k] / 2)
    }
    o <- order(mu)
    if (it > 1) keep[it - 1, ] <- c(mu[o], sig[o], pp[o])
  }
  expect_equal(r$draws, keep, tolerance = 1e-12)
  expect_equal(r$mu, colMeans(keep)[1:2], tolerance = 1e-12)
})
