# Coverage for pdic .. pheno2 exports. Every expectation is recomputed in
# the test body.

test_that("Pdic gives p_D and p_V from a deviance sample", {
  d <- c(210.3, 208.1, 215.6, 212.0, 209.4, 213.8)
  r <- Pdic(d)
  expect_equal(r$p_v, stats::var(d) / 2, tolerance = 1e-12)
  expect_equal(r$estimate, r$p_v)
  expect_identical(r$variant, "p_V")
  r2 <- Pdic(d, d_at_mean = 205)
  expect_equal(r2$p_d, mean(d) - 205, tolerance = 1e-12)
  expect_equal(r2$estimate, r2$p_d)
  expect_identical(r2$variant, "p_D")
})

test_that("morie_pelt reaches the optimal-partitioning optimum", {
  x <- c(0.1, -0.2, 0.3, 0.0, 5.1, 4.8, 5.3, 5.0, 4.9, 1.1, 0.8, 1.2, 1.0)
  op <- function(x, cost, beta, ml) {
    n <- length(x)
    C <- function(a, b) {
      s <- x[(a + 1):b]
      ss <- sum((s - mean(s))^2)
      if (cost == "mean") ss else length(s) * (log(2 * pi) + log(max(ss / length(s), 1e-300)) + 1)
    }
    F <- c(-beta, rep(Inf, n))
    cp <- integer(n + 1)
    for (t in ml:n) {
      for (tau in 0:(t - ml)) {
        if (tau > 0 && tau < ml) next
        v <- F[tau + 1] + C(tau, t) + beta
        if (v < F[t + 1]) {
          F[t + 1] <- v
          cp[t + 1] <- tau
        }
      }
    }
    taus <- integer(0)
    t <- n
    while (cp[t + 1] > 0) {
      taus <- c(cp[t + 1], taus)
      t <- cp[t + 1]
    }
    list(taus = taus, obj = F[n + 1])
  }
  r <- morie_pelt(x)
  o <- op(x, "mean", log(13), 1)
  expect_equal(r$changepoints, as.numeric(o$taus))
  expect_equal(r$objective, o$obj, tolerance = 1e-10)
  expect_equal(r$penalty, log(13))
  b <- c(0, o$taus, 13)
  expect_equal(r$segment_means, vapply(1:(length(b) - 1), function(i) mean(x[(b[i] + 1):b[i + 1]]), 0),
               tolerance = 1e-12)
  rv <- morie_pelt(x, cost = "meanvar", penalty = 4)
  ov <- op(x, "meanvar", 4, 2)
  expect_equal(rv$changepoints, as.numeric(ov$taus))
  expect_equal(rv$objective, ov$obj, tolerance = 1e-9)
  expect_error(morie_pelt(1), "series too short")
  expect_error(morie_pelt(x, cost = "var", penalty = 1), "cost must be")
})

test_that("Percn applies the unit step and accumulates perceptron updates", {
  X <- rbind(c(1, 2), c(-1, 0.5), c(0.5, -1), c(0, 0))
  w <- c(0.4, -0.3)
  b <- 0.1
  y <- c(1, 1, -1, -1)
  r <- Percn(X, w, b, y = y, eta = 0.5)
  v <- as.numeric(X %*% w + b)
  expect_equal(r$v, v, tolerance = 1e-12)
  expect_equal(r$a, as.numeric(v > 0))
  expect_equal(r$sign, sign(v))
  mis <- y * v <= 0
  expect_equal(r$update, colSums(0.5 * y[mis] * X[mis, , drop = FALSE]), tolerance = 1e-12)
  expect_equal(r$update_b, sum(0.5 * y[mis]), tolerance = 1e-12)
  expect_equal(Percn(X, w, b)$update, c(0, 0))
  expect_error(Percn(X, w[1], b), "does not match the columns")
  expect_error(Percn(X, w, c(1, 2)), "single value")
  expect_error(Percn(X, w, b, y = y[-1]), "does not match the rows")
})

test_that("FAVOR+ projections, features and attention follow their formulas", {
  om <- draw_projections(5, 3, seed = 11)
  set.seed(11)
  raw <- matrix(stats::rnorm(15), 5, 3)
  expect_equal(draw_projections(5, 3, seed = 11, orthogonal = FALSE), raw)
  # each block of d rows is Gram-Schmidt on the raw rows, rescaled to the raw norms
  for (blk in list(1:3, 4:5)) {
    q <- qr(t(raw[blk, , drop = FALSE]))
    Qm <- qr.Q(q) %*% diag(sign(diag(qr.R(q))), length(blk))
    expect_equal(om[blk, , drop = FALSE], t(Qm) * sqrt(rowSums(raw[blk, , drop = FALSE]^2)),
                 tolerance = 1e-12)
  }
  X <- rbind(c(0.2, -0.1, 0.4), c(0.5, 0.3, -0.2))
  f <- favor_features(X, om)
  want <- exp(-0.5 * rowSums(X^2)) * exp(X %*% t(om)) / sqrt(5) + 1e-6
  expect_equal(f, want, tolerance = 1e-12)
  ft <- favor_features(X, om, kind = "trig")
  P <- X %*% t(om)
  expect_equal(ft, cbind(sin(P), cos(P)) * exp(0.5 * rowSums(X^2)) / sqrt(5), tolerance = 1e-12)
  expect_equal(kernel_estimate(X[1, ], X[2, ], om), sum(f[1, ] * f[2, ]), tolerance = 1e-12)
  expect_error(favor_features(X, om, kind = "relu"), "positive or trig")
  expect_error(draw_projections(0, 3), "need m >= 1")
  Q <- rbind(c(0.1, 0.2, -0.3), c(0.4, -0.1, 0.2), c(-0.2, 0.3, 0.1))
  K <- rbind(c(0.3, -0.2, 0.1), c(0.0, 0.5, -0.4), c(0.2, 0.1, 0.3))
  V <- rbind(c(1, 0), c(0, 1), c(2, -1))
  S <- exp(Q %*% t(K))
  expect_equal(softmax_attention(Q, K, V), (S / rowSums(S)) %*% V, tolerance = 1e-12)
  Sc <- S * lower.tri(S, diag = TRUE)
  expect_equal(softmax_attention(Q, K, V, causal = TRUE), (Sc / rowSums(Sc)) %*% V, tolerance = 1e-12)
  fa <- favor_attention(Q, K, V, n_features = 6, seed = 3)
  o6 <- draw_projections(6, 3, seed = 3)
  Qf <- favor_features(Q, o6)
  Kf <- favor_features(K, o6)
  expect_equal(fa$output, (Qf %*% t(Kf) %*% V) / as.numeric(Qf %*% colSums(Kf)), tolerance = 1e-12)
  fc <- favor_attention(Q, K, V, n_features = 6, seed = 3, causal = TRUE)
  A <- (Qf %*% t(Kf)) * lower.tri(diag(3), diag = TRUE)
  expect_equal(fc$output, (A %*% V) / rowSums(A), tolerance = 1e-12)
  expect_same_function(morie_perfat, favor_attention)
  expect_error(favor_attention(Q, K[1:2, ], V), "keys but")
  expect_error(favor_attention(Q, K[, 1:2], V), "dimensions differ")
})

test_that("morie_perK is the periodic kernel", {
  x <- c(0, 0.3, 1.1, 2.5)
  r <- morie_perK(x, period = 1.2, lengthscale = 0.7, variance = 2)
  expect_equal(r$K, 2 * exp(-2 * sin(pi * outer(x, x, "-") / 1.2)^2 / 0.49), tolerance = 1e-12)
  expect_true(r$diag_is_variance)
  # lags that are whole periods reach the full variance
  r2 <- morie_perK(c(0, 1), c(2.4, 3.6), period = 1.2)
  expect_equal(r2$K[1, 1], 1, tolerance = 1e-12)
  expect_equal(r2$shape, c(2, 2))
  expect_same_function(morie_perk, morie_perK)
  expect_error(morie_perK(x, period = 0), "must be positive")
})

test_that("Persample gives prioritized-replay probabilities and IS weights", {
  d <- c(0.5, -2, 0.1, 1.2)
  r <- Persample(d, alpha = 0.7, beta = 0.5, eps = 0.01, n_sample = 4)
  p <- (abs(d) + 0.01)^0.7
  P <- p / sum(p)
  w <- (4 * P)^-0.5
  expect_equal(r$prob, P, tolerance = 1e-12)
  expect_equal(r$weight, w / max(w), tolerance = 1e-12)
  # samples invert the CDF at the van der Corput points 1/2, 1/4, 3/4, 1/8
  u <- c(0.5, 0.25, 0.75, 0.125)
  expect_equal(r$sample, vapply(u, function(v) which(v < cumsum(P))[1] - 1L, 0L))
  rk <- Persample(d, variant = "rank", t = 5, T = 10)
  pr <- (1 / rank(-abs(d)))^0.6
  expect_equal(rk$prob, pr / sum(pr), tolerance = 1e-12)
  expect_equal(rk$beta_t, 0.4 + 0.6 * 0.5, tolerance = 1e-12)
})

test_that("morie_pesdol_ardl_bounds fits the conditional ECM and the bounds F test", {
  x <- cumsum(c(0.3, -0.1, 0.4, 0.2, -0.3, 0.5, 0.1, -0.2, 0.3, 0.4, -0.1, 0.2, 0.3, -0.4, 0.1,
                0.2, 0.5, -0.3, 0.1, 0.2))
  y <- 1 + 0.8 * x + c(0.1, -0.2, 0.15, 0, 0.05, -0.1, 0.2, -0.05, 0.1, 0, -0.15, 0.1, 0.05, -0.1,
                       0.2, 0, -0.05, 0.1, -0.2, 0.05)
  r <- morie_pesdol_ardl_bounds(y, x, p = 1, q = 1)
  tt <- 3:20
  X <- cbind(1, y[tt - 1], x[tt - 1], x[tt] - x[tt - 1], x[tt - 1] - x[tt - 2])
  dep <- y[tt] - y[tt - 1]
  ols <- function(X) {
    A <- crossprod(X)
    diag(A) <- diag(A) + 1e-8 * mean(diag(A))
    b <- solve(A, crossprod(X, dep))
    list(b = as.numeric(b), rss = sum((dep - X %*% b)^2))
  }
  u <- ols(X)
  rr <- ols(X[, c(1, 4, 5)])
  expect_equal(r$coefficients, u$b, tolerance = 1e-9)
  expect_equal(r$f_statistic, ((rr$rss - u$rss) / 2) / (u$rss / (18 - 5)), tolerance = 1e-9)
  expect_equal(r$long_run, -u$b[3] / u$b[2], tolerance = 1e-9)
  expect_equal(c(r$bound_lower, r$bound_upper), c(4.94, 5.73))
  F <- r$f_statistic
  expect_identical(r$verdict, if (F > 5.73) "cointegrated" else if (F < 4.94) "no long-run relationship"
                   else "inconclusive")
  expect_same_function(morie_pesdol, morie_pesdol_ardl_bounds)
  expect_error(morie_pesdol_ardl_bounds(y[1:5], x[1:5]), "too few observations")
  expect_error(morie_pesdol_ardl_bounds(y, x[-1]), "regressor rows")
  expect_error(morie_pesdol_ardl_bounds(y, x, p = 0), "need p >= 1")
})

test_that("particle filters are exact on a degenerate state model", {
  y <- c(0.3, -0.5, 1.1, 0.4)
  expect_equal(logmeanexp(c(-1, -2, -3)), log(mean(exp(c(-1, -2, -3)))), tolerance = 1e-12)
  expect_identical(logmeanexp(c(-Inf, -Inf)), -Inf)
  expect_error(logmeanexp(numeric(0)), "nothing to average")
  mk <- function(theta) {
    list(function(n) as.list(rep(theta, n)),
         function(parts, dt) parts,
         function(p, obs) stats::dnorm(obs, p, 1, log = TRUE))
  }
  m <- mk(0.2)
  pf <- particle_filter_simple(y, 10, m[[1]], m[[2]], m[[3]], seed = 1)
  expect_equal(pf$loglik, sum(stats::dnorm(y, 0.2, 1, log = TRUE)), tolerance = 1e-12)
  expect_equal(pf$min_ess, 10, tolerance = 1e-12)
  rp <- replicated_pfilter(y, 10, m[[1]], m[[2]], m[[3]], n_reps = 3)
  expect_equal(rp$loglik, pf$loglik, tolerance = 1e-12)
  expect_equal(rp$jensen_gap, 0, tolerance = 1e-12)
  expect_equal(rp$se, 0, tolerance = 1e-12)
  expect_same_function(morie_pftrep, replicated_pfilter)
  expect_error(replicated_pfilter(y, 10, m[[1]], m[[2]], m[[3]], n_reps = 0), "at least 1 replicate")
  g <- c(-0.5, 0, 0.3, 0.6)
  lp <- loglik_profile(y, g, mk, n_particles = 5, n_reps = 2)
  ll <- vapply(g, function(th) sum(stats::dnorm(y, th, 1, log = TRUE)), 0)
  expect_equal(lp$loglik, ll, tolerance = 1e-12)
  expect_equal(lp$mle, g[which.max(ll)])
  expect_error(loglik_profile(y, 1, mk), "at least 2 grid points")
})

test_that("Pgrank solves the PageRank fixed point", {
  A <- matrix(c(0, 2, 1, 0, 0,
                1, 0, 0, 1, 0,
                0, 1, 0, 1, 1,
                0, 0, 0, 0, 0,
                1, 0, 0, 0, 0), 5, byrow = TRUE)
  r <- Pgrank(A, d = 0.8, n_iter = 300)
  out <- rowSums(A)
  P <- A / ifelse(out > 0, out, 1)
  P[out == 0, ] <- 1 / 5
  # r = (1 - d)/n 1 + d P' r, a linear system
  pr <- solve(diag(5) - 0.8 * t(P), rep(0.2 * 0.2, 5))
  expect_equal(r$pr, pr, tolerance = 1e-10)
  expect_equal(r$top, which.max(pr) - 1L)
})

test_that("Pheno2 picks the Box-Cox lambda by profile likelihood and fences on hinges", {
  v <- c(1.2, 2.5, 1.8, 3.1, 2.2, 1.5, 14.0, 2.9, 2.0, 1.7)
  lams <- (-40:40) * 0.05
  prof <- vapply(lams, function(l) {
    z <- if (l == 0) log(v) else (v^l - 1) / l
    -0.5 * 10 * log(mean((z - mean(z))^2)) + (l - 1) * sum(log(v))
  }, 0)
  lam <- lams[which.max(prof)]
  r <- Pheno2(v)
  expect_equal(r$estimate, lam)
  expect_equal(r$loglik, max(prof), tolerance = 1e-12)
  z <- if (lam == 0) log(v) else (v^lam - 1) / lam
  fv <- stats::fivenum(z)
  sp <- fv[4] - fv[2]
  expect_equal(c(r$lower, r$upper), c(fv[2] - 1.5 * sp, fv[4] + 1.5 * sp), tolerance = 1e-12)
  expect_equal(r$flags, as.numeric(z < fv[2] - 1.5 * sp | z > fv[4] + 1.5 * sp))
  # lambda fixed at 1: an identity shift, fences on the raw values
  r1 <- Pheno2(v, k = 1, lambdas = 1)
  f1 <- stats::fivenum(v - 1)
  expect_equal(r1$upper, f1[4] + (f1[4] - f1[2]), tolerance = 1e-12)
  expect_error(Pheno2(c(1, 0, 2)), "strictly positive")
})
