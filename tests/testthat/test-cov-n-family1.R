# Coverage for net*/new*/ney*/ng*/nic*/nie*/nig*/nn*/Nnet*/Noise*/nov*/npbcl
# exports. Every expectation is recomputed in the test body.

test_that("Netparf splits the attributable fraction into direct and spillover parts", {
  y <- c(3.1, 4.7, 2.2, 6.3, 5.0, 3.9, 7.4)
  ex <- c(0, 1, 0, 1, 1, 0, 1)
  A <- matrix(c(0, 1, 0, 0, 2, 0, 0,
                1, 0, 1, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0,
                0, 3, 1, 0, 0, 1, 0,
                1, 0, 0, 1, 0, 0, 2,
                0, 0, 1, 1, 0, 0, 0,
                2, 0, 0, 0, 1, 1, 0), 7, 7, byrow = TRUE)
  res <- Netparf(y, ex, A)
  nu <- ifelse(rowSums(A) > 0, as.numeric(A %*% ex) / rowSums(A), 0)
  b <- unname(stats::lm.fit(cbind(1, ex, nu), y)$coefficients)
  expect_equal(c(res$b0, res$b1, res$b2), b, tolerance = 1e-12)
  expect_equal(res$paf_direct, b[2] * mean(ex) / mean(y), tolerance = 1e-12)
  expect_equal(res$paf_spillover, b[3] * mean(nu) / mean(y), tolerance = 1e-12)
  expect_equal(res$paf, res$paf_direct + res$paf_spillover, tolerance = 1e-12)
  expect_equal(res$mean_nu, mean(nu), tolerance = 1e-12)
  # the diagonal is ignored
  A2 <- A
  diag(A2) <- 5
  expect_equal(Netparf(y, ex, A2)$paf, res$paf, tolerance = 1e-12)
  expect_error(Netparf(y, ex, -A), "non-negative")
  expect_error(Netparf(y, ex[-1], A), "different lengths")
  expect_error(Netparf(y, ex, A[-1, ]), "n by n")
  expect_error(Netparf(numeric(0), numeric(0), matrix(0, 0, 0)), "empty")
  expect_error(Netparf(-3:3, ex, A), "mean outcome is zero")
})

test_that("ShortestPathLengths matches Floyd-Warshall on the same graph", {
  E <- rbind(c(0, 1, 2), c(1, 2, 1), c(0, 2, 5), c(2, 3, 1), c(4, 4, 1))
  fw <- function(W) {
    n <- nrow(W)
    for (k in seq_len(n)) for (i in seq_len(n)) for (j in seq_len(n)) {
      W[i, j] <- min(W[i, j], W[i, k] + W[k, j])
    }
    W
  }
  base <- function(directed, weighted) {
    W <- matrix(Inf, 5, 5)
    diag(W) <- 0
    for (k in seq_len(nrow(E))) {
      u <- E[k, 1] + 1
      v <- E[k, 2] + 1
      if (u == v) next
      w <- if (weighted) E[k, 3] else 1
      W[u, v] <- min(W[u, v], w)
      if (!directed) W[v, u] <- min(W[v, u], w)
    }
    fw(W)
  }
  for (dd in c(FALSE, TRUE)) for (ww in c(FALSE, TRUE)) {
    expect_equal(ShortestPathLengths(5, E, directed = dd, weighted = ww), base(dd, ww))
  }
  # node 4 (0-based) has only a self loop: unreachable from everyone else
  D <- ShortestPathLengths(5, E[, 1:2])
  expect_true(all(is.infinite(D[5, 1:4])))
})

test_that("Newraf takes one exact Newton step on a quadratic", {
  Q <- matrix(c(4, 1, 1, 3), 2)
  cvec <- c(1, -2)
  f <- function(x) 0.5 * sum(x * (Q %*% x)) - sum(cvec * x)
  g <- function(x) as.numeric(Q %*% x) - cvec
  h <- function(x) Q
  res <- Newraf(f, g, h, c(10, -7))
  xs <- solve(Q, cvec)
  expect_equal(res$x, xs, tolerance = 1e-12)
  expect_equal(res$iterations, 1L)
  expect_equal(res$converged, 1)
  expect_equal(res$fval, f(xs), tolerance = 1e-12)
  # no iterations allowed: stays at x0 and reports non-convergence
  r0 <- Newraf(f, g, h, c(10, -7), n_iter = 0)
  expect_equal(r0$x, c(10, -7))
  expect_equal(r0$converged, 0)
  expect_equal(r0$grad_norm, sqrt(sum(g(c(10, -7))^2)), tolerance = 1e-12)
  # a non-quadratic converges to the stationary point of x^4/4 - x
  r1 <- Newraf(function(x) x^4 / 4 - x, function(x) x^3 - 1, function(x) 3 * x^2, 2)
  expect_equal(r1$x, 1, tolerance = 1e-12)
  expect_error(Newraf(f, g, h, numeric(0)), "empty")
  expect_error(Newraf(f, g, h, c(1, 1), n_iter = -1), "non-negative")
  expect_error(Newraf(f, function(x) 1, h, c(1, 1)), "wrong length")
  expect_error(Newraf(f, g, function(x) diag(3), c(1, 1)), "wrong shape")
})

test_that("Neymal allocates n_h proportional to N_h S_h (Neyman 1934)", {
  Nh <- c(120, 300, 80, 500)
  Sh <- c(4, 2.5, 9, 1.2)
  n <- 90
  res <- Neymal(sum(Nh), Nh, Sh, n)
  al <- n * Nh * Sh / sum(Nh * Sh)
  expect_equal(res$allocation, al, tolerance = 1e-12)
  expect_equal(res$variance, sum(Nh * (Nh - al) * Sh^2 / al), tolerance = 1e-12)
  Tt <- sum(Nh * Sh)
  N <- sum(Nh)
  expect_equal(res$A, (N - n) / n * sum(Nh * Sh^2), tolerance = 1e-12)
  expect_equal(res$C, N / n * sum(Nh * (Sh - Tt / N)^2), tolerance = 1e-12)
  # at the Neyman optimum every N_h S_h / n_h equals T / n, so B = 0
  expect_equal(res$B, 0, tolerance = 1e-12)
  # integer allocation: floors plus largest remainders, summing to n
  fl <- floor(al)
  extra <- order(-(al - fl))[seq_len(n - sum(fl))]
  fl[extra] <- fl[extra] + 1
  expect_equal(res$allocation_int, fl)
  expect_equal(sum(res$allocation_int), n)
  expect_equal(Neymal(NULL, Nh, Sh, n)$allocation, al, tolerance = 1e-12)
  expect_error(Neymal(sum(Nh) + 1, Nh, Sh, n), "does not equal")
  expect_error(Neymal(-1, Nh, Sh, n), "must be positive")
  expect_error(Neymal(NULL, Nh, rep(0, 4), n), "sum\\(Nh Sh\\) must be positive")
})

test_that("morie_ngnest is the median of the N-BEATS members over lookbacks and block sets", {
  y <- 10 + 0.3 * seq_len(30) + 2 * sin(2 * pi * seq_len(30) / 6)
  sets <- list(
    list(list("trend", 2L, 3L), list("seasonality", 2L, 3L)),
    list(list("trend", 1L, 3L), list("seasonality", 3L, 3L)),
    list(list("generic", 0L, 0L), list("trend", 2L, 3L))
  )
  fc <- list()
  for (mult in c(2, 3)) {
    lb <- mult * 3
    w <- y[(30 - lb + 1):30]
    for (s in sets) fc[[length(fc) + 1]] <- nbeats_stack(w, 3L, s, ridge = 1e-8)$forecast
  }
  F <- do.call(rbind, fc)
  res <- morie_ngnest(y, 3, lookback_multiples = c(2, 3))
  expect_equal(res$n_members, nrow(F))
  expect_equal(res$forecast, apply(F, 2, stats::median), tolerance = 1e-12)
  expect_equal(res$mean_forecast, colMeans(F), tolerance = 1e-12)
  expect_equal(res$spread, apply(F, 2, function(v) max(v) - min(v)), tolerance = 1e-12)
  expect_equal(res$lookbacks, c(2L, 3L))
  lbm <- c(mean(F[1:3, 1]), mean(F[4:6, 1]))
  expect_equal(res$lookback_spread, max(lbm) - min(lbm), tolerance = 1e-12)
  rm <- morie_ngnest(y, 3, lookback_multiples = c(2, 3), how = "mean")
  expect_equal(rm$forecast, colMeans(F), tolerance = 1e-12)
  expect_same_function(morie_ngnest_ensemble, morie_ngnest)
  expect_error(morie_ngnest(y[1:5], 3, lookback_multiples = c(2, 3)), "no ensemble member")
})

test_that("Ngppr gives the Dirichlet-process expected number of clusters", {
  y <- c(1, 1, 2, 3, 3, 3, 4)
  a <- 1.7
  n <- length(y)
  res <- Ngppr(y, alpha = a, tau = 2.5)
  i <- seq_len(n)
  expect_equal(res$e_k, sum(a / (a + i - 1)), tolerance = 1e-12)
  expect_equal(res$var_k, sum(a * (i - 1) / (a + i - 1)^2), tolerance = 1e-12)
  # the digamma form is the same sum; the helper's asymptotic series is
  # accurate to about 1e-11, hence 1e-9 here
  expect_equal(res$e_k_digamma, a * (digamma(a + n) - digamma(a)), tolerance = 1e-9)
  expect_equal(res$k_observed, 4L)
  expect_equal(res$total_mass, a * 2.5)
  expect_equal(res$psi1, a * log(1 + 2.5), tolerance = 1e-12)
  expect_error(Ngppr(y, alpha = 0), "alpha must be positive")
  expect_error(Ngppr(y, tau = -1), "tau must be positive")
  expect_error(Ngppr(numeric(0)), "empty")
})

test_that("Niccgg reproduces the Nakagawa-Schielzeth marginal and conditional R^2", {
  x <- c(1.2, 0.4, 2.2, 3.1, 1.8, 0.9, 2.7, 3.5, 1.1, 2.0, 0.3, 2.9)
  cl <- rep(c("a", "b", "c", "d"), times = c(3, 4, 2, 3))
  y <- 1 + 0.8 * x + c(0.5, 0.5, 0.5, -0.4, -0.4, -0.4, -0.4, 0.9, 0.9, -0.2, -0.2, -0.2) +
    c(0.1, -0.2, 0.05, 0.3, -0.1, 0.2, -0.3, 0.15, -0.05, 0.1, -0.25, 0.2)
  res <- Niccgg(y, x, cluster = cl)
  fit <- stats::lm(y ~ x)
  s2f <- stats::var(fitted(fit))
  e <- stats::residuals(fit)
  tab <- stats::anova(stats::lm(e ~ factor(cl)))
  msb <- tab[["Mean Sq"]][1]
  msw <- tab[["Mean Sq"]][2]
  ni <- as.numeric(table(cl))
  N <- length(y)
  n0 <- (N - sum(ni^2) / N) / (length(ni) - 1)
  s2l <- max(0, (msb - msw) / n0)
  tot <- s2f + s2l + msw
  expect_equal(res$sigma2_f, s2f, tolerance = 1e-12)
  expect_equal(res$sigma2_e, msw, tolerance = 1e-12)
  expect_equal(res$sigma2_l, s2l, tolerance = 1e-12)
  expect_equal(res$r2_marginal, s2f / tot, tolerance = 1e-12)
  expect_equal(res$r2_conditional, (s2f + s2l) / tot, tolerance = 1e-12)
  expect_equal(res$icc, s2l / (s2l + msw), tolerance = 1e-12)
  expect_error(Niccgg(y, x), "cluster labels are required")
  expect_error(Niccgg(y, x, cluster = rep("a", N)), "at least two clusters")
  expect_error(Niccgg(y, x, cluster = as.character(seq_len(N))), "more observations than clusters")
})

test_that("Nie is the mean and standard error of the paired differences", {
  a <- c(5.1, 6.3, 4.8, 7.0, 5.5)
  b <- c(4.0, 6.1, 4.9, 5.2, 5.0)
  res <- Nie(a, b)
  expect_equal(res$estimate, mean(a - b), tolerance = 1e-12)
  expect_equal(res$se, stats::sd(a - b) / sqrt(5), tolerance = 1e-12)
  expect_equal(c(res$mean_y11, res$mean_y10, res$n), c(mean(a), mean(b), 5))
  expect_true(is.nan(Nie(1, 2)$se))
  expect_error(Nie(a, b[-1]), "same length")
})

test_that("Nignst is the BDA3 normal / scaled-inverse-chi2 conjugate update", {
  y <- c(2.3, 1.9, 3.4, 2.8, 2.0, 3.1)
  n <- length(y)
  res <- Nignst(y, mu0 = 1, kappa0 = 2, nu0 = 3, sigma0_sq = 0.5)
  kn <- 2 + n
  nn <- 3 + n
  mun <- (2 * 1 + sum(y)) / kn
  ss <- 3 * 0.5 + sum((y - mean(y))^2) + 2 * n / kn * (mean(y) - 1)^2
  expect_equal(res$mu_n, mun, tolerance = 1e-12)
  expect_equal(c(res$kappa_n, res$nu_n), c(kn, nn))
  expect_equal(res$sigma_n_sq, ss / nn, tolerance = 1e-12)
  expect_equal(res$mu_scale_sq, ss / nn / kn, tolerance = 1e-12)
  expect_equal(res$pred_scale_sq, ss / nn * (1 + 1 / kn), tolerance = 1e-12)
  expect_equal(Nignst(4, 0, 1, 1, 1)$s_sq, 0)
  expect_error(Nignst(numeric(0), 0, 1, 1, 1), "at least one")
  expect_error(Nignst(y, 0, 0, 1, 1), "must be positive")
})

test_that("Nndist computes the border-corrected nearest-neighbour G function", {
  P <- cbind(c(0.1, 0.35, 0.8, 0.52, 0.9, 0.22, 0.61),
             c(0.2, 0.75, 0.4, 0.5, 0.95, 0.44, 0.13))
  rg <- c(0, 0.05, 0.1, 0.2, 0.3)
  win <- c(0, 0, 1, 1)
  res <- Nndist(P, rg, window = win)
  D <- as.matrix(stats::dist(P))
  diag(D) <- Inf
  d <- apply(D, 1, min)
  b <- pmin(P[, 1], 1 - P[, 1], P[, 2], 1 - P[, 2])
  G <- vapply(rg, function(h) {
    k <- b > h
    if (any(k)) mean(d[k] <= h) else NA_real_
  }, 0)
  expect_equal(res$G, G, tolerance = 1e-12)
  expect_equal(res$m_used, vapply(rg, function(h) sum(b > h), 0))
  expect_equal(res$lambda_hat, 7)
  expect_equal(res$G_csr, 1 - exp(-7 * pi * rg^2), tolerance = 1e-12)
  expect_equal(res$estimate, mean(d), tolerance = 1e-12)
  expect_error(Nndist(P[1, , drop = FALSE], rg), "two points")
  expect_error(Nndist(P, numeric(0), win), "empty")
  expect_error(Nndist(P, -1, win), "non-negative")
})

test_that("Nnet1lay evaluates and fits a one-hidden-layer network", {
  X <- cbind(c(0.2, -1.1, 0.7, 1.5, -0.3, 0.9, -0.8, 0.1),
             c(1.0, 0.3, -0.6, 0.2, 1.4, -1.2, 0.5, -0.4))
  al <- matrix(c(1.5, -0.7, -0.4, 2.0), 2)
  b <- c(0.3, -0.5)
  Z <- stats::plogis(sweep(X %*% al, 2, b, "+"))
  beta <- c(2, -1)
  res <- Nnet1lay(X, al, b, beta = beta)
  expect_equal(res$hidden, Z, tolerance = 1e-12)
  expect_equal(res$fitted, as.numeric(Z %*% beta), tolerance = 1e-12)
  expect_true(is.nan(res$rss))
  y <- c(1.1, 0.2, -0.3, 0.8, 1.9, -1.0, 0.4, 0.0)
  fit <- Nnet1lay(X, al, b, y = y)
  bq <- qr.solve(Z, y)
  # the function solves the normal equations; QR differs by cond(Z)^2 * eps
  expect_equal(fit$beta, bq, tolerance = 1e-9)
  expect_equal(fit$rss, sum((y - Z %*% bq)^2), tolerance = 1e-9)
  expect_error(Nnet1lay(X, al, b), "supply beta")
  expect_error(Nnet1lay(X, al, b, beta = 1), "one entry per hidden unit")
  expect_error(Nnet1lay(X, al[1, , drop = FALSE], b, beta = beta), "one row per column")
  expect_error(Nnet1lay(X, al, 1, beta = beta), "b must have one entry")
  expect_error(Nnet1lay(X, al, b, y = y[-1]), "same number of rows")
})

test_that("EquivalentLevel and CombineLevels add sound energies", {
  L <- c(60, 70, 65)
  d <- c(1, 3, 2)
  expect_equal(EquivalentLevel(L, d), 10 * log10(stats::weighted.mean(10^(L / 10), d)),
               tolerance = 1e-12)
  expect_equal(EquivalentLevel(L), 10 * log10(mean(10^(L / 10))), tolerance = 1e-12)
  expect_equal(EquivalentLevel(c(55, 55, 55)), 55, tolerance = 1e-12)
  # two equal sources add 10 log10(2) dB
  expect_equal(CombineLevels(c(80, 80)), 80 + 10 * log10(2), tolerance = 1e-12)
  expect_equal(CombineLevels(L), 10 * log10(sum(10^(L / 10))), tolerance = 1e-12)
})

test_that("Novlt is the self-information of each item in bits", {
  pop <- c(10, 30, 0, 60)
  res <- Novlt(c(0, 1, 3, 1), pop)
  p <- pop / 100
  nov <- -log2(p[c(1, 2, 4, 2)])
  expect_equal(res$nov, nov, tolerance = 1e-12)
  expect_equal(res$estimate, mean(nov), tolerance = 1e-12)
  expect_equal(res$n_items, 4L)
  expect_identical(Novlt(2, pop)$nov, Inf)
})

test_that("Npbcl builds the sequential DP-mixture MAP partition", {
  y <- c(0.1, 5.2, -0.2, 5.0, 0.3, 11.0, 5.4)
  a <- 0.8
  s <- 1
  t0 <- 10
  mu0 <- mean(y)
  cnt <- numeric(0)
  sm <- numeric(0)
  lab <- integer(0)
  tot <- 0
  for (v in y) {
    sc <- if (length(cnt)) {
      pr <- 1 / t0^2 + cnt / s^2
      log(cnt) + stats::dnorm(v, (mu0 / t0^2 + sm / s^2) / pr, sqrt(1 / pr + s^2), log = TRUE)
    } else {
      numeric(0)
    }
    ns <- log(a) + stats::dnorm(v, mu0, sqrt(t0^2 + s^2), log = TRUE)
    if (!length(sc) || ns > max(sc)) {
      cnt <- c(cnt, 1)
      sm <- c(sm, v)
      lab <- c(lab, length(cnt) - 1L)
      tot <- tot + ns
    } else {
      k <- which.max(sc)
      cnt[k] <- cnt[k] + 1
      sm[k] <- sm[k] + v
      lab <- c(lab, k - 1L)
      tot <- tot + max(sc)
    }
  }
  res <- Npbcl(y, alpha = a, sigma = s, tau0 = t0)
  expect_equal(res$labels, lab)
  expect_equal(res$sizes, cnt)
  expect_equal(res$n_clusters, length(cnt))
  pr <- 1 / t0^2 + cnt / s^2
  expect_equal(res$means, (mu0 / t0^2 + sm / s^2) / pr, tolerance = 1e-12)
  expect_equal(res$log_score, tot, tolerance = 1e-12)
  expect_error(Npbcl(numeric(0)), "empty")
  expect_error(Npbcl(y, alpha = 0), "alpha must be positive")
  expect_error(Npbcl(y, sigma = 0), "sigma must be positive")
  expect_error(Npbcl(y, tau0 = 0), "tau0 must be positive")
})
