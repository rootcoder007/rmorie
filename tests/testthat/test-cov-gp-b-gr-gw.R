# Coverage for the spectral-mixture, sparse-variational and warped GPs,
# GP expected improvement, the GPT-2 configuration counts, gradient
# descent, the graphlet kernel, CRF training, multilevel graph bisection,
# systematic sampling, the graded response model, gross-error sensitivity,
# Grover search, AR6 GWPs and GWR bandwidth selection.

se_k <- function(P, Q, ell = 1, v = 1) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  D <- matrix(0, nrow(P), nrow(Q))
  for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q))) D[i, j] <- sum((P[i, ] - Q[j, ])^2)
  v * exp(-0.5 * D / ell^2)
}
x1 <- c(-2, -1.2, -0.3, 0.4, 1.1, 2.0)
y1 <- c(0.3, 0.9, 0.2, -0.4, -1.0, -0.2)

test_that("Gpsps uses the spectral-mixture kernel", {
  w <- c(0.6, 0.4)
  v <- c(0.05, 0.2)
  m <- c(0.1, 0.5)
  sm <- function(t) sum(w * exp(-2 * pi^2 * t^2 * v) * cos(2 * pi * t * m))
  K <- outer(x1, x1, Vectorize(function(a, b) sm(a - b))) + diag(0.05, 6)
  xt <- c(0, 0.7)
  Ks <- outer(xt, x1, Vectorize(function(a, b) sm(a - b)))
  r <- Gpsps(x1, y1, xt, Q = 2, weights = w, variances = v, means = m, noise = 0.05)
  expect_equal(r$mean, as.numeric(Ks %*% solve(K, y1)), tolerance = 1e-9)
  expect_equal(r$variance, sm(0) - rowSums(Ks * t(solve(K, t(Ks)))), tolerance = 1e-9)
  expect_equal(r$k_zero, 1, tolerance = 1e-12)
  expect_equal(r$loglik, -0.5 * sum(y1 * solve(K, y1)) - 0.5 * as.numeric(determinant(K)$modulus) - 3 * log(2 * pi),
               tolerance = 1e-9)
  expect_error(Gpsps(x1, y1, Q = 2, weights = 1), "Q entries")
  expect_error(Gpsps(x1, y1, Q = 0), "at least 1")
})

test_that("Gpsvi is the Titsias collapsed bound and its predictive", {
  X <- matrix(x1, ncol = 1)
  Z <- matrix(c(-1.5, 0, 1.5), ncol = 1)
  Xt <- matrix(c(-0.5, 1), ncol = 1)
  r <- Gpsvi(X, y1, Xt, inducing = Z, lengthscale = 0.9, variance = 1.2, noise = 0.1)
  Kmm <- se_k(Z, Z, 0.9, 1.2) + diag(1e-9, 3)
  Knm <- se_k(X, Z, 0.9, 1.2)
  Qn <- Knm %*% solve(Kmm, t(Knm))
  S <- Qn + diag(0.1, 6)
  g <- -0.5 * sum(y1 * solve(S, y1)) - 0.5 * as.numeric(determinant(S)$modulus) - 3 * log(2 * pi)
  expect_equal(r$gaussian_term, g, tolerance = 1e-9)
  expect_equal(r$trace_term, sum(1.2 - diag(Qn)), tolerance = 1e-9)
  expect_equal(r$elbo, g - sum(1.2 - diag(Qn)) / 0.2, tolerance = 1e-9)
  row <- se_k(Xt, Z, 0.9, 1.2) %*% solve(Kmm, t(Knm))
  expect_equal(r$mean, as.numeric(row %*% solve(S, y1)), tolerance = 1e-9)
  expect_equal(r$variance, 1.2 - rowSums(row * t(solve(S, t(row)))), tolerance = 1e-9)
  expect_error(Gpsvi(X, y1, inducing = matrix(0, 1, 2)), "wrong dimension")
  expect_error(Gpsvi(X, y1, noise = 0), "positive")
})

test_that("Gpvbo is expected improvement under the GP posterior", {
  X <- matrix(x1, ncol = 1)
  G <- matrix(seq(-2, 2, by = 0.5), ncol = 1)
  r <- Gpvbo(X, y1, G, lengthscale = 0.8, variance = 1, noise = 0.01, xi = 0.05)
  K <- se_k(X, X, 0.8) + diag(0.01, 6)
  Ks <- se_k(G, X, 0.8)
  mu <- as.numeric(Ks %*% solve(K, y1))
  s <- sqrt(pmax(1 - rowSums(Ks * t(solve(K, t(Ks)))), 0))
  imp <- min(y1) - mu - 0.05
  ei <- ifelse(s > 0, imp * pnorm(imp / s) + s * dnorm(imp / s), 0)
  expect_equal(r$mean, mu, tolerance = 1e-9)
  expect_equal(r$acquisition, ei, tolerance = 1e-9)
  expect_equal(r$next_index, which.max(ei))
  expect_equal(r$next_point, G[which.max(ei), ])
  expect_error(Gpvbo(X, y1, matrix(0, 1, 2)), "different dimensions")
})

test_that("Warpedgp fits kernel ridge in the warped space", {
  X <- matrix(x1, ncol = 1)
  yp <- exp(y1)
  r <- Warpedgp(X, yp, warp = "log", lam = 0.1, gamma = 0.5)
  K <- exp(-0.5 * as.matrix(dist(X))^2) + diag(0.1, 6)
  wm <- as.numeric((K - diag(0.1, 6)) %*% solve(K, y1))
  expect_equal(r$warped_mean, wm, tolerance = 1e-9)
  expect_equal(r$median, exp(wm), tolerance = 1e-9)
  expect_equal(r$log_jacobian, sum(log(1 / yp)), tolerance = 1e-12)
  s <- Warpedgp(X, yp, warp = "sqrt", lam = 0.1, gamma = 0.5)
  expect_equal(s$median, as.numeric((K - diag(0.1, 6)) %*% solve(K, sqrt(yp)))^2, tolerance = 1e-9)
  expect_equal(s$log_jacobian, sum(log(0.5 / sqrt(yp))), tolerance = 1e-12)
  expect_equal(Warpedgp(X, y1)$log_jacobian, 0)
})

test_that("Gpt2 resolves the released configurations to exact parameter counts", {
  cnt <- function(L, d, V = 50257, M = 1024) V * d + M * d + L * (12 * d^2 + 13 * d) + 2 * d
  r <- Gpt2(1:5)
  expect_equal(r$total_params, cnt(12, 768))
  expect_equal(r$all_sizes$medium, cnt(24, 1024))
  expect_equal(r$params_vs_small, 1)
  expect_equal(r$d_head, 64L)
  m <- Gpt2(1:3, size = "medium", n_layers = 2)
  expect_equal(m$total_params, cnt(2, 1024))
  expect_equal(m$params_vs_small, cnt(2, 1024) / cnt(12, 768), tolerance = 1e-12)
  expect_error(Gpt2(1, size = "huge"), "size must be")
})

test_that("Gradds takes fixed gradient steps and stops on a small gradient", {
  A <- matrix(c(2, 0.5, 0.5, 1), 2)
  f <- function(x) 0.5 * sum(x * (A %*% x)) - sum(c(1, -1) * x)
  gf <- function(x) as.numeric(A %*% x) - c(1, -1)
  r <- Gradds(f, gf, c(0, 0), lr = 0.3, steps = 5)
  x <- c(0, 0)
  path <- f(x)
  for (i in 1:5) {
    gn <- sqrt(sum(gf(x)^2))
    x <- x - 0.3 * gf(x)
    path <- c(path, f(x))
  }
  expect_equal(r$x, x, tolerance = 1e-12)
  expect_equal(r$f_path, path, tolerance = 1e-12)
  expect_equal(r$grad_norm, gn, tolerance = 1e-12)
  expect_equal(r$steps_used, 5L)
  c2 <- Gradds(f, gf, c(0, 0), lr = 0.3, steps = 500, tol = 1e-10)
  expect_true(c2$converged)
  expect_equal(c2$x, as.numeric(solve(A, c(1, -1))), tolerance = 1e-9)
  expect_error(Gradds(f, gf, c(0, 0), lr = 0), "lr")
  expect_error(Gradds(f, function(x) 1, c(0, 0)), "wrong length")
})

test_that("Graphlet counts induced size-k subgraph types", {
  tri <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 0,
                  1, 1, 0, 1,
                  0, 0, 1, 0), 4, byrow = TRUE)
  path <- matrix(0, 4, 4)
  path[cbind(1:3, 2:4)] <- 1
  path <- path + t(path)
  sig <- function(A, s) {
    B <- A[s, s]
    paste0(sum(B) / 2, ":", paste(sort(rowSums(B)), collapse = ","))
  }
  cmb <- combn(4, 3)
  s1 <- apply(cmb, 2, sig, A = tri)
  s2 <- apply(cmb, 2, sig, A = path)
  r <- Graphlet(tri, path, 3)
  types <- r$types
  expect_setequal(types, union(s1, s2))
  f1 <- vapply(types, function(t) sum(s1 == t), 0) / 4
  f2 <- vapply(types, function(t) sum(s2 == t), 0) / 4
  expect_equal(r$f1, unname(f1), tolerance = 1e-12)
  expect_equal(r$estimate, sum(f1 * f2), tolerance = 1e-12)
  u <- Graphlet(tri, path, 3, normalize = FALSE)
  expect_equal(u$f1, unname(f1) * 4, tolerance = 1e-12)
})

test_that("CrfFit reaches a stationary point of the penalised CRF likelihood", {
  seqs <- list(matrix(c(1, 0.5, -1, 0.2, 0.3, -0.4), 3), matrix(c(-0.5, 1, 0.8, -0.2), 2))
  labs <- list(c(0, 1, 1), c(1, 0))
  fit <- CrfFit(seqs, labs, n_labels = 2, l2 = 0.5, max_iter = 500)
  th <- fit$theta
  K <- 2
  d <- 2
  W <- matrix(th[1:4], 2, 2, byrow = TRUE)
  b <- th[5:6]
  Tm <- matrix(th[7:10], 2, 2, byrow = TRUE)
  feat <- function(X, y) {
    g <- numeric(10)
    for (t in seq_len(nrow(X))) {
      g[(y[t] - 1) * 2 + 1:2] <- g[(y[t] - 1) * 2 + 1:2] + X[t, ]
      g[4 + y[t]] <- g[4 + y[t]] + 1
      if (t > 1) g[6 + (y[t - 1] - 1) * 2 + y[t]] <- g[6 + (y[t - 1] - 1) * 2 + y[t]] + 1
    }
    g
  }
  obj <- 0.25 * sum(th^2)
  grad <- 0.5 * th
  for (s in 1:2) {
    X <- seqs[[s]]
    n <- nrow(X)
    ys <- as.matrix(expand.grid(rep(list(1:2), n)))
    sc <- apply(ys, 1, function(y) sum(th * feat(X, y)))
    lz <- log(sum(exp(sc)))
    p <- exp(sc - lz)
    obj <- obj + lz - sum(th * feat(X, labs[[s]] + 1))
    ef <- Reduce(`+`, lapply(seq_len(nrow(ys)), function(k) p[k] * feat(X, ys[k, ])))
    grad <- grad + ef - feat(X, labs[[s]] + 1)
  }
  expect_equal(fit$objective, obj, tolerance = 1e-9)
  expect_lt(max(abs(grad)), 1e-6)
  expect_equal(c(fit$n_labels, fit$n_features), c(2, 2))
})

test_that("morie_grclus finds the one-edge cut between two cliques", {
  A <- matrix(0, 8, 8)
  A[1:4, 1:4] <- 1
  A[5:8, 5:8] <- 1
  diag(A) <- 0
  A[4, 5] <- A[5, 4] <- 1
  r <- morie_grclus(A, k = 2)
  cut <- sum(A[r$partition == 0, r$partition == 1])
  best <- min(apply(combn(8, 4), 2, function(s) sum(A[s, -s])))
  expect_equal(r$edge_cut, cut)
  expect_equal(r$edge_cut, best)
  expect_equal(r$sizes, c(4L, 4L))
  expect_equal(r$balance, 1)
  expect_equal(r$total_edge_weight, sum(A) / 2)
  expect_equal(r$cut_fraction, cut / (sum(A) / 2), tolerance = 1e-12)
  r4 <- morie_grclus(A, k = 4, matching = "rm", refinement = "kl")
  expect_equal(sort(unique(r4$partition)), 0:3)
  expect_equal(r4$edge_cut, sum(A[outer(r4$partition, r4$partition, "!=")]) / 2)
  expect_error(morie_grclus(A, k = 9), "exceeds")
  expect_error(morie_grclus(A, matching = "x"), "matching")
  A2 <- A
  A2[1, 2] <- 3
  expect_error(morie_grclus(A2), "symmetric")
})

test_that("SystematicSample lays a randomly started grid and clips to a polygon", {
  u <- .morie_random_uniform(2, seed = 5)
  s <- SystematicSample(c(0, 0, 10, 6), 2.5, seed = 5)
  xs <- seq(u[1] * 2.5, 10, by = 2.5)
  ys <- seq(u[2] * 2.5, 6, by = 2.5)
  expect_equal(s, unname(as.matrix(expand.grid(xs, ys))), tolerance = 1e-12)
  tri <- rbind(c(0, 0), c(10, 0), c(0, 6))
  p <- SystematicSample(c(0, 0, 10, 6), 2.5, seed = 5, polygon = tri)
  keep <- s[, 1] / 10 + s[, 2] / 6 < 1
  expect_equal(p, s[keep, , drop = FALSE], tolerance = 1e-12)
})

test_that("Grmsam, gross, GroupAvg and GroverS follow their definitions", {
  b <- c(-1, 0.5, 1.5)
  th <- c(-0.3, 1, 2)
  y <- c(0, 2, 3)
  r <- Grmsam(y, th, 1.2, b)
  P <- t(vapply(th, function(t) -diff(c(1, plogis(1.2 * (t - b)), 0)), numeric(4)))
  expect_equal(r$p_observed, P[cbind(1:3, y + 1)], tolerance = 1e-12)
  expect_equal(r$loglik, sum(log(P[cbind(1:3, y + 1)])), tolerance = 1e-12)
  expect_equal(r$categories, 4L)
  expect_error(Grmsam(0, 0, 1, c(1, 0)), "increase")
  expect_error(Grmsam(4, 0, 1, b), "category range")

  IF <- c(0.5, -2.5, 1.2, 2.5)
  g <- gross(IF, x = c(10, 20, 30, 40))
  expect_equal(g$gamma_star, 2.5)
  expect_equal(g$xmax, 20)
  expect_equal(g$imax, 1L)
  expect_equal(morie_gross_error_sensitivity(IF)$xmax, 1)

  expect_equal(GroupAvg(3, 2.5)$yavg, 7.5)
  expect_error(GroupAvg(1:2, 1), "single values")

  mark <- c(0, 1, 0, 0, 0, 0, 0, 0)
  gs <- GroverS(mark, 8)
  th0 <- asin(sqrt(1 / 8))
  k <- floor(pi / (4 * th0))
  expect_equal(gs$k_opt, as.integer(k))
  expect_equal(gs$p_path, sin((2 * (0:k) + 1) * th0)^2, tolerance = 1e-12)
  expect_equal(gs$p_success, sin((2 * k + 1) * th0)^2, tolerance = 1e-12)
  expect_error(GroverS(rep(0, 4), 4), "marked")
  expect_error(GroverS(mark, 4), "N entries")
})

test_that("morie_gwPot reads the AR6 table consistently", {
  ch4 <- morie_gwPot("ch4", 100)
  co2 <- morie_gwPot("CO2", 100)
  expect_equal(co2$estimate, 1)
  expect_equal(ch4$agwp_co2, co2$agwp)
  expect_equal(ch4$gwp_from_agwp, ch4$agwp / co2$agwp, tolerance = 1e-12)
  expect_equal(abs(ch4$gwp_from_agwp - ch4$estimate) / ch4$estimate < 0.05, TRUE)
  expect_equal(morie_gwPot("hfc_134a", 20)$gas, "HFC-134a")
  expect_equal(morie_gwPot("CFC11", 500)$gas, "CFC-11")
  expect_error(morie_gwPot("H2O"), "unknown gas")
  expect_error(morie_gwPot("CH4", 50), "horizon")
})

test_that("morie_gwrcal fits local weighted least squares at the selected bandwidth", {
  set.seed(3)
  n <- 15
  C <- cbind(runif(n, 0, 10), runif(n, 0, 10))
  x <- rnorm(n)
  y <- 1 + (0.5 + 0.1 * C[, 1]) * x + rnorm(n, sd = 0.3)
  X <- cbind(1, x)
  r <- morie_gwrcal(y, X, C, kernel = "bisquare", criterion = "aicc", search = "grid",
                    bounds = c(4, 14), n_points = 6)
  D <- as.matrix(dist(C))
  fitbw <- function(h) {
    tr <- 0
    res <- numeric(n)
    B <- matrix(0, n, 2)
    for (i in 1:n) {
      w <- ifelse(D[i, ] < h, (1 - (D[i, ] / h)^2)^2, 0)
      XtW <- t(X * w)
      Bi <- solve(XtW %*% X, XtW)
      B[i, ] <- Bi %*% y
      tr <- tr + (X[i, ] %*% Bi)[i]
      res[i] <- y[i] - sum(X[i, ] * B[i, ])
    }
    s2 <- sum(res^2) / (n - tr)
    list(B = B, tr = tr, s2 = s2, aicc = 2 * n * log(s2) + n * log(2 * pi) + n * (n + tr) / (n - 2 - tr))
  }
  grid <- seq(4, 14, length.out = 6)
  prof <- vapply(grid, function(h) fitbw(h)$aicc, 0)
  expect_equal(r$profile, prof, tolerance = 1e-9)
  expect_equal(r$bandwidth, grid[which.min(prof)])
  f <- fitbw(r$bandwidth)
  expect_equal(r$coefficients, f$B, tolerance = 1e-9)
  expect_equal(r$tr_S, f$tr, tolerance = 1e-9)
  expect_equal(r$aicc, f$aicc, tolerance = 1e-9)
  ols <- lm(y ~ x)
  s2o <- sum(residuals(ols)^2) / n
  go <- 2 * n * log(s2o) + n * log(2 * pi) + n * (n + 2) / (n - 4)
  expect_equal(morie_gwrcal_global_aicc(y, X), go, tolerance = 1e-9)
  expect_equal(r$ols_aicc, go, tolerance = 1e-9)

  ad <- morie_gwrcal(y, X, C, adaptive = TRUE, bounds = c(5, 8))
  expect_equal(ad$grid, 5:8)
  nb <- ad$bandwidth
  Bad <- t(vapply(1:n, function(i) {
    idx <- order(D[i, ])[1:(nb + 1)]
    coef(lm(y[idx] ~ x[idx]))
  }, numeric(2)))
  expect_equal(unname(ad$coefficients), unname(Bad), tolerance = 1e-9)

  pr <- morie_gwrcal_prepare(y, X, C)
  expect_equal(c(pr$n, pr$p), c(15L, 2L))
  expect_error(morie_gwrcal_prepare(y[1:2], X[1:2, ], C[1:2, ]), "three observations")
  expect_error(morie_gwrcal_prepare(y, X, C[-1, ]), "coords has")
  expect_error(morie_gwrcal(y, X, C, kernel = "cosine"), "kernel must be")
  expect_error(morie_gwrcal(y, X, C, adaptive = TRUE, search = "golden"), "integer grid")
})
