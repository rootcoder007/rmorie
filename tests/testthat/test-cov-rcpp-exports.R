# Coverage tests for the exported Rcpp kernels (R/RcppExports.R). Each
# compiled kernel is recomputed with a direct base-R loop or a stats
# reference on a small deterministic input.

test_that("DSP adaptive filters: LMS, NLMS and RLS recursions", {
  n <- 40
  x <- sin((1:n) / 3) + 0.3 * cos((1:n) / 1.7)
  d <- 0.8 * c(0, x[-n]) - 0.3 * c(0, 0, x[-(n - 1):-n])
  ref <- function(kind, order, mu = 0.05, eps = 1e-3, lam = 0.99, delta = 10) {
    w <- numeric(order)
    P <- delta * diag(order)
    y <- e <- numeric(n)
    for (p in (order + 1):n) {
      seg <- x[p - seq_len(order)]
      y[p] <- sum(w * seg)
      e[p] <- d[p] - y[p]
      if (kind == "lms") w <- w + 2 * mu * e[p] * seg
      if (kind == "nlms") w <- w + mu / (sum(seg^2) + eps) * e[p] * seg
      if (kind == "rls") {
        Ps <- as.numeric(P %*% seg)
        k <- Ps / (lam + sum(seg * Ps))
        w <- w + k * e[p]
        P <- (P - outer(k, Ps)) / lam
      }
    }
    list(y = y, e = e)
  }
  expect_equal(morie_dsp_lms_cpp(x, d, 3L, 0.05), ref("lms", 3), tolerance = 1e-12)
  expect_equal(morie_dsp_nlms_cpp(x, d, 3L, 0.5, 1e-3), ref("nlms", 3, mu = 0.5), tolerance = 1e-12)
  expect_equal(morie_dsp_rls_cpp(x, d, 2L, 0.99, 10), ref("rls", 2), tolerance = 1e-10)
  expect_error(morie_dsp_lms_cpp(x, d[-1], 3L, 0.05), "same length")
  expect_error(morie_dsp_rls_cpp(x, d, 0L, 0.99, 10), "order")
})

test_that("DSP cross-correlation equals ccf(y, x) and median filter is zero-padded", {
  x <- c(1.2, -0.4, 2.2, 0.9, -1.1, 0.3, 1.7, 0.2)
  y <- c(0.5, 1.1, -0.2, 2.4, 0.8, -0.9, 0.4, 1.3)
  cc <- morie_dsp_cross_correlation_cpp(x, y, 3L)
  expect_equal(cc, as.numeric(ccf(y, x, lag.max = 3, plot = FALSE)$acf), tolerance = 1e-12)
  expect_length(morie_dsp_cross_correlation_cpp(x, y, 20L), 15L)
  expect_error(morie_dsp_cross_correlation_cpp(x, y, -1L), "max_lag")
  mf <- function(v, k) {
    h <- k %/% 2
    p <- c(rep(0, h), v, rep(0, h))
    vapply(seq_along(v), function(i) median(p[i:(i + 2 * h)]), 0)
  }
  expect_equal(morie_dsp_median_filter_cpp(x, 3L), mf(x, 3), tolerance = 1e-12)
  expect_equal(morie_dsp_median_filter_cpp(x, 4L), mf(x, 5), tolerance = 1e-12)
  expect_error(morie_dsp_median_filter_cpp(x, 0L), "kernel_size")
})

test_that("Hawkes kernels: densities, CDFs and pair excitation", {
  u <- c(0, 0.3, 1.2, 4.5)
  expect_equal(morie_hawkes_kernel_density_cpp(u, "exponential", 1.5), dexp(u, 1.5), tolerance = 1e-12)
  expect_equal(morie_hawkes_kernel_cdf_cpp(u, "exponential", 1.5), pexp(u, 1.5), tolerance = 1e-12)
  u2 <- u[-1]
  expect_equal(morie_hawkes_kernel_density_cpp(u2, "gamma", c(2.2, 1.3)), dgamma(u2, 2.2, rate = 1.3), tolerance = 1e-12)
  expect_equal(morie_hawkes_kernel_cdf_cpp(u, "gamma", c(2.2, 1.3)), pgamma(u, 2.2, rate = 1.3), tolerance = 1e-12)
  expect_equal(morie_hawkes_kernel_density_cpp(u2, "weibull", c(1.7, 2)), dweibull(u2, 1.7, 2), tolerance = 1e-12)
  expect_equal(morie_hawkes_kernel_cdf_cpp(u, "weibull", c(1.7, 2)), pweibull(u, 1.7, 2), tolerance = 1e-12)
  # Lomax, scipy convention: alpha c^alpha (u + c)^-(alpha + 1)
  expect_equal(morie_hawkes_kernel_density_cpp(u, "lomax", c(2.5, 0.8)), 2.5 * 0.8^2.5 * (u + 0.8)^-3.5, tolerance = 1e-12)
  expect_equal(morie_hawkes_kernel_cdf_cpp(u, "lomax", c(2.5, 0.8)), 1 - (0.8 / (u + 0.8))^2.5, tolerance = 1e-12)
  tt <- c(0.5, 1.1, 1.3, 2.9)
  ex <- morie_hawkes_pair_excitation_sum_cpp(tt, 0.4, "weibull", c(1.7, 2))
  expect_equal(ex, c(0, vapply(2:4, function(i) 0.4 * sum(dweibull(tt[i] - tt[1:(i - 1)], 1.7, 2)), 0)), tolerance = 1e-12)
  expect_error(morie_hawkes_kernel_density_cpp(u, "cauchy", 1), "unknown kernel")
  expect_error(morie_hawkes_kernel_cdf_cpp(u, "gamma", 1), "psi too short")
})

test_that("Hawkes seasonal baseline integral is the documented trapezoid", {
  a <- c(0.2, 0.5, 0.3, -0.1)
  nu <- function(t) exp(a[1] + a[2] * t / 100 + a[3] * sin(2 * pi / 365.25 * t) + a[4] * cos(2 * pi / 365.25 * t))
  g <- seq(0, 100, length.out = 101)
  h <- 1
  ref <- h * (sum(nu(g)) - 0.5 * (nu(0) + nu(100)))
  expect_equal(morie_hawkes_baseline_integral_cpp(100, a, 101L), ref, tolerance = 1e-12)
  # trapezoid with h = 1 on a smooth integrand: O(h^2) error ~2e-6 relative
  expect_equal(morie_hawkes_baseline_integral_cpp(100, a, 101L), integrate(nu, 0, 100)$value, tolerance = 1e-5)
  expect_equal(morie_hawkes_baseline_integral_cpp(-1, a), 0)
  expect_error(morie_hawkes_baseline_integral_cpp(10, a[1:3]), "4 alpha")
})

test_that("Hawkes constant-baseline negative log-likelihoods", {
  tt <- c(0.3, 0.9, 1.4, 1.5, 2.8, 3.3, 4.9)
  T <- 6
  nll <- function(g, G, nu, eta) {
    lam <- nu + eta * vapply(seq_along(tt), function(i) sum(g(tt[i] - tt[seq_len(i - 1)])), 0)
    -(sum(log(lam)) - (nu * T + eta * sum(G(T - tt))))
  }
  expect_equal(morie_hawkes_ll_exp_const_cpp(tt, T, log(0.8), 0.4, 1.5),
    nll(function(u) dexp(u, 1.5), function(u) pexp(u, 1.5), 0.8, 0.4), tolerance = 1e-12)
  expect_equal(morie_hawkes_ll_weibull_const_cpp(tt, T, log(0.8), 0.4, 1.7, 2),
    nll(function(u) dweibull(u, 1.7, 2), function(u) pweibull(u, 1.7, 2), 0.8, 0.4), tolerance = 1e-12)
  expect_equal(morie_hawkes_ll_gamma_const_cpp(tt, T, log(0.8), 0.4, 2.2, 1.3),
    nll(function(u) dgamma(u, 2.2, rate = 1.3), function(u) pgamma(u, 2.2, rate = 1.3), 0.8, 0.4), tolerance = 1e-10)
  # the likelihood core uses the (alpha - 1) Lomax parametrisation
  lg <- function(u) 1.5 * 0.8^1.5 * (u + 0.8)^-2.5
  lG <- function(u) 1 - (0.8 / (u + 0.8))^1.5
  expect_equal(morie_hawkes_ll_lomax_const_cpp(tt, T, log(0.8), 0.4, 2.5, 0.8), nll(lg, lG, 0.8, 0.4), tolerance = 1e-12)
  expect_equal(morie_hawkes_ll_exp_const_cpp(tt, T, 0, 1.2, 1.5), 1e12)
  expect_equal(morie_hawkes_ll_weibull_const_cpp(tt, T, 0, 0.4, 30, 2), 1e12)
  expect_equal(morie_hawkes_ll_gamma_const_cpp(tt, T, 0, 0.4, 2, 50), 1e12)
})

test_that("matching distance matrices equal direct pairwise distances, also far from the origin", {
  Xt <- rbind(c(1, 2), c(0.5, -1), c(3, 0.2))
  Xc <- rbind(c(0, 0), c(2, 1), c(-1, 4), c(1.5, 1.5))
  direct <- function(A, B, S = diag(ncol(A))) {
    outer(seq_len(nrow(A)), seq_len(nrow(B)), Vectorize(function(i, j) {
      d <- A[i, ] - B[j, ]
      sqrt(sum(d * (S %*% d)))
    }))
  }
  expect_equal(morie_matching_euclidean_pairs_cpp(Xt, Xc), direct(Xt, Xc), tolerance = 1e-12)
  S <- solve(rbind(c(2, 0.5), c(0.5, 1)))
  expect_equal(morie_matching_mahalanobis_pairs_cpp(Xt, Xc, S), direct(Xt, Xc, S), tolerance = 1e-12)
  # far from the origin the distances must still come from the differences
  # (the offset data are themselves rounded, so compare to their own
  # direct distances)
  expect_equal(morie_matching_euclidean_pairs_cpp(Xt + 1e9, Xc + 1e9), direct(Xt + 1e9, Xc + 1e9), tolerance = 1e-12)
  expect_equal(morie_matching_mahalanobis_pairs_cpp(Xt + 1e9, Xc + 1e9, S), direct(Xt + 1e9, Xc + 1e9, S), tolerance = 1e-12)
  expect_error(morie_matching_euclidean_pairs_cpp(Xt, Xc[, 1, drop = FALSE]), "same number of columns")
  expect_error(morie_matching_mahalanobis_pairs_cpp(Xt, Xc, diag(3)), "d x d")
})

test_that("nearest-neighbour selection, CEM strata and the Abadie-Imbens variance", {
  D <- rbind(c(0.5, 0.2, 0.9), c(0.1, 0.3, 0.4), c(0.7, 0.6, 0.05))
  r <- morie_matching_nn_select_cpp(D, FALSE, Inf, 1L)
  expect_equal(r$treated_pos, 1:3)
  expect_equal(r$control_pos, c(2L, 1L, 3L))
  expect_equal(r$distance, c(0.2, 0.1, 0.05))
  rw <- morie_matching_nn_select_cpp(D, TRUE, 0.35, 2L)
  expect_equal(rw$treated_pos, c(1L, 2L, 2L, 3L))
  expect_equal(rw$control_pos, c(2L, 1L, 2L, 3L))
  X <- matrix(c(1L, 2L, 1L, 3L, 2L, 1L, 1L, 1L, 1L, 1L), ncol = 2)
  expect_equal(morie_matching_cem_strata_cpp(X), match(paste(X[, 1], X[, 2]), unique(paste(X[, 1], X[, 2]))))
  y <- c(3, 1.5, 2.2, 0.4, 4.1)
  t <- c(1L, 0L, 1L, 0L, 1L)
  tp <- c(1L, 3L, 5L)
  cp <- c(2L, 2L, 4L)
  K <- c(0, 2, 0, 1, 0)
  s2 <- numeric(5)
  for (k in 1:3) s2[c(tp[k], cp[k])] <- 0.5 * (y[tp[k]] - y[cp[k]])^2
  expect_equal(morie_matching_abadie_imbens_kernel_cpp(y, t, tp, cp), (sum(s2[t == 1]) + sum(K[t == 0]^2 * s2[t == 0])) / 9, tolerance = 1e-12)
  expect_error(morie_matching_abadie_imbens_kernel_cpp(y, t[-1], tp, cp), "equal length")
})

test_that("classical MDS matches cmdscale; SMACOF step is the Guttman transform", {
  P <- rbind(c(0, 0), c(3, 0), c(1, 2), c(4, 3), c(-1, 1))
  D <- as.matrix(dist(P))
  r <- morie_spatial_classical_mds_cpp(D, 2L)
  ref <- cmdscale(D, k = 2, eig = TRUE)
  expect_equal(as.numeric(r$eigenvalues), ref$eig[1:2], tolerance = 1e-9)
  expect_equal(as.matrix(dist(r$coordinates)), D, tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(r$stress, 0, tolerance = 1e-9)
  X <- P + rbind(c(0.2, -0.1), c(0, 0.3), c(-0.2, 0), c(0.1, 0.1), c(0, -0.2))
  W <- matrix(1, 5, 5)
  s <- morie_spatial_smacof_step_cpp(X, D, W)
  dX <- as.matrix(dist(X))
  B <- -D / ifelse(dX > 0, dX, 1)
  diag(B) <- 0
  diag(B) <- -rowSums(B)
  # Guttman transform X+ = V^+ B(X) X with V the weight Laplacian; for unit
  # weights V^+ B X = B X / n because B X is column-centred
  Xn <- B %*% X / 5
  expect_equal(s$coordinates, Xn, tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(s$stress, sum((D - as.matrix(dist(Xn)))^2) / 2, tolerance = 1e-12)
})

test_that("IRT theta update is one penalised Newton step per legislator", {
  a <- rbind(c(1, 0.2), c(-0.5, 1), c(0.8, -0.7), c(1.2, 0.4))
  d <- c(0.1, -0.3, 0.5, 0)
  votes <- rbind(c(1, 0, NA, 1), c(0, 1, 1, NA), c(1, 1, 0, 0))
  th <- rbind(c(0.2, -0.1), c(-0.4, 0.3), c(0, 0))
  r <- morie_spatial_emirt_theta_update_cpp(th, a, d, votes)
  for (i in 1:3) {
    v <- !is.na(votes[i, ])
    ai <- a[v, , drop = FALSE]
    p <- plogis(pmin(pmax(ai %*% th[i, ] + d[v], -20), 20))
    wv <- as.numeric(p * (1 - p)) + 1e-10
    H <- crossprod(ai, ai * wv) + diag(2)
    g <- crossprod(ai, votes[i, v] - p)
    expect_equal(r[i, ], th[i, ] + as.numeric(solve(H, g)), tolerance = 1e-12)
  }
})

test_that("Wordfish omega update: Newton step then standardisation", {
  dtm <- rbind(c(3, 0, 5, 1), c(1, 4, 0, 2), c(2, 2, 2, 2))
  psi <- c(0.2, -0.1, 0.4)
  al <- c(0.5, 0.1, -0.2, 0.3)
  be <- c(0.8, -0.6, 1.1, 0.2)
  om <- c(0.3, -0.5, 0.1)
  r <- morie_spatial_wordfish_omega_update_cpp(dtm, psi, al, be, om)
  new <- vapply(1:3, function(i) {
    mu <- exp(pmin(pmax(psi[i] + al + be * om[i], -20), 20))
    om[i] - sum(be * (dtm[i, ] - mu)) / (-1 - sum(be^2 * mu))
  }, 0)
  expect_equal(as.numeric(r), (new - mean(new)) / (sd(new) + 1e-12), tolerance = 1e-12)
})

test_that("NOMINATE iteration: centroid midpoints, targets and the final likelihood", {
  votes <- rbind(c(1, 0, 1), c(1, 1, 0), c(0, 1, 1), c(0, 0, NA))
  X0 <- rbind(c(0.5, 0.1), c(0.2, 0.4), c(-0.3, 0.2), c(-0.6, -0.3))
  nv0 <- matrix(c(1, 0), 3, 2, byrow = TRUE)
  mid0 <- matrix(0, 3, 2)
  w <- c(1, 0.5)
  r <- morie_spatial_nominate_iterate_cpp(votes, X0, w, nv0, mid0, 5, 1L)
  nv <- nv0
  mid <- mid0
  for (j in 1:3) {
    ok <- !is.na(votes[, j])
    yc <- colMeans(X0[ok & votes[, j] == 1, , drop = FALSE])
    nc <- colMeans(X0[ok & votes[, j] == 0, , drop = FALSE])
    nv[j, ] <- (yc - nc) / sqrt(sum((yc - nc)^2))
    mid[j, ] <- (yc + nc) / 2
  }
  X <- X0
  for (i in 1:4) {
    ok <- which(!is.na(votes[i, ]))
    tg <- t(vapply(ok, function(j) mid[j, ] + ifelse(votes[i, j] == 1, 0.5, -0.5) * nv[j, ], c(0, 0)))
    if (length(ok) >= 2) X[i, ] <- colMeans(tg)
  }
  mx <- max(sqrt(rowSums(X^2)))
  if (mx > 1) X <- X / mx
  expect_equal(r$ideal_points, X, tolerance = 1e-12)
  expect_equal(r$midpoints, mid, tolerance = 1e-12)
  expect_equal(as.numeric(r$cutpoints), rowSums(nv * mid), tolerance = 1e-12)
  ll <- 0
  hit <- tot <- 0
  for (j in 1:3) for (i in 1:4) {
    if (is.na(votes[i, j])) next
    dy <- X[i, ] - (mid[j, ] + 0.5 * nv[j, ])
    dn <- X[i, ] - (mid[j, ] - 0.5 * nv[j, ])
    p <- pnorm(5 * (exp(-0.5 * sum(w * dy^2)) - exp(-0.5 * sum(w * dn^2))))
    p <- min(max(p, 1e-10), 1 - 1e-10)
    ll <- ll + votes[i, j] * log(p) + (1 - votes[i, j]) * log(1 - p)
    hit <- hit + ((p > 0.5) == (votes[i, j] == 1))
    tot <- tot + 1
  }
  expect_equal(r$log_lik, ll, tolerance = 1e-12)
  expect_equal(r$gmp, hit / tot, tolerance = 1e-12)
})
