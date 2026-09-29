# Coverage for spmsim .. spwkth exports. Every expectation is recomputed in
# the test body.

sp4_sites <- function() {
  cbind(c(0.1, 0.9, 1.7, 2.6, 0.4, 1.3, 2.2, 0.0, 0.8, 1.9, 2.7, 1.1),
        c(0.2, 0.1, 0.4, 0.0, 1.1, 0.9, 1.3, 2.0, 2.2, 1.8, 2.4, 3.0))
}

test_that("spmsim backfits a multiscale GWR on the standardised scale", {
  P <- sp4_sites()
  x1 <- c(0.3, 1.2, -0.4, 0.8, 1.9, -1.1, 0.5, 1.4, -0.2, 0.9, 2.1, -0.6)
  y <- 1 + 0.8 * x1 + 0.3 * P[, 1] + c(0.1, -0.2, 0.05, 0.3, -0.1, 0.2, -0.3, 0.15, 0.0, -0.05, 0.25, -0.15)
  X <- cbind(1, x1)
  r <- spmsim(X, y, P, init_bandwidth = 2)
  sdp <- function(v) sqrt(mean((v - mean(v))^2))
  expect_equal(r$y_centre, mean(y), tolerance = 1e-12)
  expect_equal(r$y_scale, sdp(y), tolerance = 1e-12)
  expect_equal(r$x_centre, c(0, mean(x1)), tolerance = 1e-12)
  expect_equal(r$x_scale, c(1, sdp(x1)), tolerance = 1e-12)
  Xs <- cbind(1, (x1 - mean(x1)) / sdp(x1))
  fit <- rowSums(r$local_coefficients * Xs) * sdp(y) + mean(y)
  expect_equal(r$fitted, fit, tolerance = 1e-12)
  expect_equal(r$resid, y - fit, tolerance = 1e-12)
  expect_equal(r$rss, sum((y - fit)^2), tolerance = 1e-12)
  expect_true(r$converged)
  expect_lt(r$score_history[r$n_iter], 1e-5)
  expect_equal(r$bandwidths, r$bandwidth_history[[r$n_iter]])
  expect_equal(r$score_type, "SOC-f")
  one <- spmsim(X, y, P, init_bandwidth = 2, max_iter = 1, standardize = FALSE, rss_score = TRUE)
  expect_false(one$converged)
  expect_match(one$warning, "max_iter=1")
  expect_equal(one$y_scale, 1)
  expect_equal(one$fitted, rowSums(one$local_coefficients * X), tolerance = 1e-12)
})

test_that("spmwst kriges each target from its moving window", {
  P <- sp4_sites()
  z <- c(1.2, 0.8, 1.9, 2.4, 1.1, 1.6, 2.2, 0.7, 1.0, 1.8, 2.6, 1.3)
  tg <- rbind(c(1, 1), c(2, 2))
  r <- spmwst(P, z, targets = tg, min_sites = 12, local_variogram = FALSE, n_lags = 6)
  # global exponential variogram by weighted least squares over the grid
  D <- as.matrix(stats::dist(P))
  h <- D[upper.tri(D)]
  ij <- which(upper.tri(D), arr.ind = TRUE)
  sq <- 0.5 * (z[ij[, 1]] - z[ij[, 2]])^2
  ed <- seq(0, max(h), length.out = 7)
  bin <- pmin(pmax(findInterval(h, ed), 1), 6)
  cnt <- tabulate(bin, 6)
  hb <- tapply(h, factor(bin, 1:6), mean)
  gb <- tapply(sq, factor(bin, 1:6), mean)
  ok <- cnt > 0
  hb <- hb[ok]
  gb <- gb[ok]
  w <- cnt[ok]
  best <- c(Inf, NA, NA)
  for (rg in exp(seq(log(min(hb)), log(max(hb) * 3), length.out = 60))) {
    b <- 1 - exp(-3 * hb / rg)
    s <- sum(w * b * gb) / sum(w * b * b)
    l <- sum(w * (gb - s * b)^2)
    if (s > 0 && l < best[1]) best <- c(l, s, rg)
  }
  expect_equal(r$global_sill, best[2], tolerance = 1e-12)
  expect_equal(r$global_range, best[3], tolerance = 1e-12)
  C <- best[2] * exp(-3 * D / best[3])
  pred <- vapply(1:2, function(i) {
    c0 <- best[2] * exp(-3 * sqrt(colSums((t(P) - tg[i, ])^2)) / best[3])
    mean(z) + sum(c0 * solve(C + 1e-10 * diag(12), z - mean(z)))
  }, 0)
  expect_equal(r$prediction, pred, tolerance = 1e-10)
  expect_equal(r$window_sizes, c(12, 12))
  expect_equal(r$local_variograms, cbind(r$local_sill, r$local_range))
  f <- spmwst(P, z, window_size = 1.5, n_lags = 4)
  expect_equal(f$fixed_window_counts, unname(rowSums(D <= 1.5)))
  expect_match(f$warning, "below the 35")
  expect_error(spmwst(P, z, window_size = 0), "positive")
  expect_error(spmwst(P, z[-1]), "disagree on n")
})

test_that("spnst builds the point-source nonstationary covariance", {
  P <- cbind(c(0, 1, 0.5, 2), c(0, 0.3, 1.2, 1))
  src <- c(0.5, 0.5)
  r <- spnst(P, source = src, theta1 = 0.8, theta2 = 0.2, theta3 = 0.1, sill = 2)
  ci <- sqrt(colSums((t(P) - src)^2))
  H <- as.matrix(stats::dist(P))
  Cr <- exp(-0.8 * H * exp(0.2 * abs(outer(ci, ci, "-")) + 0.1 * outer(ci, ci, pmin)))
  expect_equal(r$correlation, Cr, tolerance = 1e-12)
  expect_equal(r$nonstationary_cov, 2 * Cr, tolerance = 1e-12)
  expect_equal(r$practical_range, 3 / 0.8, tolerance = 1e-12)
  expect_equal(r$source_distance, ci, tolerance = 1e-12)
  expect_equal(r$min_eigenvalue, min(eigen(Cr, symmetric = TRUE)$values), tolerance = 1e-12)
  a <- spnst(P, theta1 = 1, anisotropy = diag(c(2, 1)))
  expect_equal(a$separation, as.matrix(stats::dist(P %*% diag(c(2, 1)))), tolerance = 1e-12)
  expect_equal(a$source, colMeans(P))
  expect_error(spnst(P, sill = 0), "sill must be positive")
  expect_error(spnst(P, theta1 = 0), "theta1 must be positive")
})

test_that("spperiod matches the direct DFT and the covariance transform", {
  Z <- matrix(c(1.2, 0.4, 2.1, 1.7, 0.9, 1.1, 2.4, 0.3, 1.5, 0.8, 1.9, 2.2), 3, 4)
  r <- spperiod(Z)
  j <- -1:1
  k <- -1:2
  w1 <- 2 * pi * j / 3
  w2 <- 2 * pi * k / 4
  d <- Z - mean(Z)
  I <- outer(w1, w2, Vectorize(function(a, b) {
    s <- sum(d * exp(-1i * outer(a * (1:3), b * (1:4), "+")))
    Mod(s)^2 / ((2 * pi)^2 * 12)
  }))
  expect_equal(r$periodogram, I, tolerance = 1e-12)
  expect_equal(r$omega1, w1, tolerance = 1e-12)
  expect_equal(r$omega2, w2, tolerance = 1e-12)
  expect_true(r$identity_holds)
  expect_true(r$mean_invariant)
  expect_equal(r$covariance[3, 4], mean(d^2), tolerance = 1e-12)
  expect_equal(r$covariance[4, 5], sum(d[1:2, 1:3] * d[2:3, 2:4]) / 12, tolerance = 1e-12)
  raw <- spperiod(Z, omit_zero_frequency = FALSE, check_identity = FALSE)
  expect_equal(raw$periodogram[2, 2], sum(Z)^2 / ((2 * pi)^2 * 12), tolerance = 1e-12)
  expect_null(raw$identity_holds)
  expect_error(spperiod(matrix(1:3, 1)), "at least 2x2")
})

test_that("sppql reaches the pseudo-likelihood fixed point", {
  X <- cbind(1, c(0.3, -0.5, 1.2, 0.8, -1.0, 0.1))
  z <- c(2, 0, 5, 3, 1, 2)
  P <- cbind(c(0, 1, 2, 0, 1, 2), c(0, 0, 0, 1, 1, 1))
  SS <- 0.4 * exp(-as.matrix(stats::dist(P)))
  r <- sppql(z, X, SS)
  expect_true(r$converged)
  mu <- exp(as.numeric(X %*% r$beta) + r$S)
  # Poisson/log: psi = mu and Sigma = sigma2 diag(mu), so the score is
  # X'(z - mu) and (z - mu) - Sigma_S^-1 S
  expect_equal(as.numeric(t(X) %*% (z - mu)), c(0, 0), tolerance = 1e-6)
  expect_equal(z - mu - as.numeric(solve(SS, r$S)), rep(0, 6), tolerance = 1e-6)
  expect_true(r$pql_pl_equivalent)
  nu <- log(r$mu) + (z - r$mu) / r$mu
  expect_equal(r$pseudo_data, nu, tolerance = 1e-12)
  Sn <- SS + diag(1 / r$mu)
  expect_equal(r$Sigma_nu, Sn, tolerance = 1e-12)
  Si <- solve(Sn)
  xsx <- t(X) %*% Si %*% X
  b <- solve(xsx, t(X) %*% Si %*% nu)
  expect_equal(r$beta, as.numeric(b), tolerance = 1e-7)
  res <- nu - X %*% b
  reml <- as.numeric(determinant(Sn)$modulus + determinant(xsx)$modulus + t(res) %*% Si %*% res + 4 * log(2 * pi))
  expect_equal(r$reml, reml, tolerance = 1e-10)
  expect_equal(r$specification, "conditional")
  s1 <- sppql(z, X, SS, max_iter = 1)
  expect_match(s1$warning, "did not converge in 1")
  expect_error(sppql(z[-1], X, SS), "agree on the sample size")
})

test_that("spsar is the concentrated-likelihood SAR lag model", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  A <- matrix(0, 8, 8)
  for (i in 1:7) A[i, i + 1] <- A[i + 1, i] <- 1
  A[1, 8] <- A[8, 1] <- 1
  W <- A / rowSums(A)
  x <- cbind(1, c(0.5, 1.2, -0.3, 0.8, 2.0, -1.1, 0.4, 1.5))
  y <- c(1.4, 2.9, 0.7, 2.1, 3.8, 0.2, 1.9, 3.1)
  r <- spsar(x, y, W)
  f <- spatialreg::lagsarlm(y ~ x[, 2], listw = spdep::mat2listw(W, style = "W"), method = "eigen")
  # two independent 1-D optimisers of the same concentrated likelihood
  expect_equal(r$rho, unname(f$rho), tolerance = 1e-6)
  ys <- y - r$rho * as.numeric(W %*% y)
  b <- stats::lm.fit(x, ys)
  expect_equal(r$estimate, unname(b$coefficients), tolerance = 1e-10)
  expect_equal(r$sigma2, sum(b$residuals^2) / 5, tolerance = 1e-10)
  expect_equal(r$se, sqrt(diag(r$sigma2 * solve(crossprod(x)))), tolerance = 1e-10)
  expect_error(spsar(x, y[-1], W), "shape mismatch")
})

test_that("spspec gives the discrete spectral covariance and a random-phase field", {
  h <- c(0, 0.7, 2)
  om <- c(0.5, 1.5, 3)
  s2 <- c(1, 0.5, 0.2)
  r <- spspec(h, sigma2 = s2, omega = om, mu = 1, seed = 11)
  expect_equal(r$covariance, vapply(h, function(v) sum(s2 * cos(om * v)), 0), tolerance = 1e-12)
  expect_equal(r$variance, 1.7, tolerance = 1e-12)
  set.seed(11)
  ph <- stats::runif(3, 0, 2 * pi)
  expect_equal(r$realisation, 1 + vapply(h, function(v) sum(sqrt(2 * s2) * cos(om * v + ph)), 0), tolerance = 1e-12)
  d <- spspec(0)
  expect_equal(d$sigma2, exp(-seq(0.2, 4, length.out = 12)), tolerance = 1e-12)
  expect_error(spspec(h, sigma2 = 1:2, omega = om), "same length")
  expect_error(spspec(h, sigma2 = -s2, omega = om), "non-negative")
})

test_that("spstcn builds nonseparable space-time covariances", {
  h <- c(0, 0.5, 1.2)
  u <- c(0, 1, 2)
  g <- spstcn(h, u, params = list(a = 2, c = 1.5, beta = 0.5, sigma2 = 2))
  psi <- 2 * u^2 + 1
  expect_equal(g$st_covariance, 2 / psi^0.5 * exp(-1.5 * h^2 / psi^0.5), tolerance = 1e-12)
  expect_false(g$separable)
  expect_equal(g$equation, "9.8")
  gt <- spstcn(h, u, params = list(beta_t = 0.7, neg2_loglik = 100, neg2_loglik_separable = 103.2))
  psi1 <- u^2 + 1
  expect_equal(gt$st_covariance, 1 / psi1^1.7 * exp(-h^2 / psi1), tolerance = 1e-12)
  expect_equal(gt$separability_test$statistic, 3.2, tolerance = 1e-12)
  expect_equal(gt$separability_test$p_value, 0.5 * stats::pchisq(3.2, 1, lower.tail = FALSE), tolerance = 1e-12)
  P <- cbind(c(0, 1, 0.5), c(0, 0, 1))
  tm <- c(0, 1, 2)
  v <- spstcn(h, u, coords = P, times = tm)
  Cm <- 1 / (abs(outer(tm, tm, "-"))^2 + 1) * exp(-as.matrix(stats::dist(P))^2 / (abs(outer(tm, tm, "-"))^2 + 1))
  expect_equal(v$min_eigenvalue, min(eigen(Cm, symmetric = TRUE)$values), tolerance = 1e-12)
  expect_true(v$valid)
  rs <- c(0.9, 0.5)
  rt <- c(0.8, 0.3)
  pm <- matrix(c(0.2, 0.3, 0.1, 0.4), 2)
  b <- spstcn(h, u, method = "power_mixture", params = list(rs = rs, rt = rt, pmf = pm))
  expect_equal(b$st_covariance, 0.2 + 0.3 * rs + 0.1 * rt + 0.4 * rs * rt, tolerance = 1e-12)
  p <- spstcn(h, u, method = "power_mixture", params = list(rs = rs, rt = rt, lam = 2))
  expect_equal(p$st_covariance, exp(2 * (rs * rt - 1)), tolerance = 1e-12)
  bi <- spstcn(h, u, method = "power_mixture", params = list(rs = rs, rt = rt, distribution = "binomial", n = 3, pi = 0.4))
  expect_equal(bi$st_covariance, (0.4 * (rs * rt - 1) + 1)^3, tolerance = 1e-12)
  sm <- spstcn(h, u, method = "scale_mixture",
               params = list(cov_spatial = function(x) exp(-x), cov_temporal = function(x) exp(-x^2),
                             nodes = c(0.5, 2), weights = c(0.3, 0.7)))
  expect_equal(sm$st_covariance, 0.3 * exp(-0.5 * h) * exp(-(0.5 * u)^2) + 0.7 * exp(-2 * h) * exp(-(2 * u)^2),
               tolerance = 1e-12)
  jz <- spstcn(c(0, 0.5), c(0, 0.3), method = "differential", params = list(theta = 1, c = 1, p = 1.5))
  ref <- vapply(1:2, function(i) {
    hh <- c(0, 0.5)[i]
    kk <- c(0, 0.3)[i]
    stats::integrate(function(t) t * exp(-kk * (t^2 + 1)^1.5) / (t^2 + 1)^1.5 * besselJ(t * hh, 0),
                     0, Inf, rel.tol = 1e-12)$value / (4 * pi)
  }, 0)
  # panelled Gauss-Legendre against adaptive quadrature; at k = 0 the
  # integrand decays like t^-2 and the panels stop at max_panels, so the
  # truncation there is bounded by the reported tail bound instead
  expect_equal(jz$st_covariance[2], ref[2], tolerance = 1e-8)
  expect_lte(abs(jz$st_covariance[1] - ref[1]), jz$quadrature$tail_bound)
  expect_gt(jz$quadrature$tail_bound, 0)
  expect_error(spstcn(h, u, method = "other"), "must be one of")
})

test_that("spstcv combines spatial and temporal covariances", {
  cs <- function(x) exp(-x)
  ct <- function(x) 1 / (1 + x^2)
  h <- c(0, 0.4, 1)
  u <- c(0, 2, 1)
  for (fm in c("product", "sum", "product_sum")) {
    r <- spstcv(h, u, cs, ct, form = fm)
    a <- cs(h)
    b <- ct(u)
    want <- switch(fm, product = a * b, sum = a + b, product_sum = a * b + a + b)
    expect_equal(r$st_covariance, want, tolerance = 1e-12)
    expect_equal(r$separable, fm != "product_sum")
    expect_equal(r$sill, switch(fm, product = 1, sum = 2, product_sum = 3), tolerance = 1e-12)
  }
  r <- spstcv(h, u, cs, ct)
  expect_equal(r$spatial_only, cs(h), tolerance = 1e-12)
  expect_equal(r$temporal_only, ct(u), tolerance = 1e-12)
  P <- cbind(c(0, 1, 0.5), c(0, 0, 1))
  tm <- c(0, 1, 3)
  v <- spstcv(h, u, cs, ct, coords = P, times = tm)
  M <- cs(as.matrix(stats::dist(P))) * ct(abs(outer(tm, tm, "-")))
  expect_equal(v$min_eigenvalue, min(eigen(M, symmetric = TRUE)$values), tolerance = 1e-12)
  expect_true(v$valid)
  expect_error(spstcv(h, u, cs, ct, form = "ratio"), "must be")
})

test_that("spstp estimates intensities and the CSTR dispersion test", {
  pts <- cbind(c(0.1, 0.6, 0.3, 0.9, 0.2, 0.7, 0.4, 0.8), c(0.2, 0.1, 0.8, 0.6, 0.5, 0.9, 0.3, 0.4))
  tm <- c(0.5, 1.2, 3.1, 2.2, 0.9, 3.8, 1.7, 2.9)
  reg <- c(0, 1, 0, 1)
  r <- spstp(pts, reg, c(0, 4), times = tm, process_type = "earthquake", n_space_bins = 2, n_time_bins = 2)
  expect_equal(r$intensity, 8 / 4, tolerance = 1e-12)
  expect_equal(r$volume, 4)
  cnt <- table(factor(pmin(floor(pts[, 1] * 2), 1) + 1, 1:2), factor(pmin(floor(pts[, 2] * 2), 1) + 1, 1:2),
               factor(pmin(floor(tm / 2), 1) + 1, 1:2))
  expect_equal(as.numeric(r$cell_counts), as.numeric(cnt))
  expect_equal(r$marginal_spatial, unname(matrix(apply(cnt, c(1, 2), sum), 2)) / 0.25, tolerance = 1e-12)
  expect_equal(r$marginal_temporal, as.numeric(apply(cnt, 3, sum)) / 2, tolerance = 1e-12)
  f <- as.numeric(cnt)
  D <- 7 * stats::var(f) / mean(f)
  expect_equal(r$index_of_dispersion, D, tolerance = 1e-12)
  expect_equal(r$p_value, stats::pchisq(D, 7, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(r$cstr$expected_count, 8, tolerance = 1e-12)
  expect_match(r$power_note, "only 8")
  expect_match(r$conditional_note, "earthquake")
  bd <- spstp(pts, reg, c(0, 4), times = tm, process_type = "birth_death")
  expect_match(bd$identifiability_note, "birth-death")
  expect_null(bd$conditional_note)
  expect_error(spstp(pts, reg, c(0, 4)), "`times` is required")
  expect_error(spstp(pts, reg, c(0, 4), times = tm, process_type = "tsunami"), "must be one of")
})

test_that("spstvg bins the space-time semivariogram", {
  P <- cbind(c(0, 1, 0.5, 1.5, 0.2, 1.2), c(0, 0.2, 1, 0.8, 0.5, 1.4))
  tm <- c(0, 0, 1, 1, 2, 2)
  z <- c(1.1, 0.7, 1.9, 1.3, 0.4, 1.6)
  mf <- function(h, u) 0.5 + h + 0.2 * u
  r <- spstvg(P, tm, z, n_space_bins = 2, n_time_bins = 2, max_dist = 1.6, max_time = 2, at_time = 1, model_fn = mf)
  ij <- which(upper.tri(diag(6)), arr.ind = TRUE)
  d <- sqrt(rowSums((P[ij[, 1], ] - P[ij[, 2], ])^2))
  u <- abs(tm[ij[, 1]] - tm[ij[, 2]])
  sq <- (z[ij[, 1]] - z[ij[, 2]])^2
  keep <- d <= 1.6 & u <= 2
  di <- pmin(floor(d[keep] / 0.8) + 1, 2)
  ui <- pmin(floor(u[keep] / 1) + 1, 2)
  cnt <- matrix(0, 2, 2)
  g <- matrix(NA_real_, 2, 2)
  for (a in 1:2) for (b in 1:2) {
    m <- di == a & ui == b
    cnt[a, b] <- sum(m)
    if (any(m)) g[a, b] <- sum(sq[keep][m]) / (2 * sum(m))
  }
  expect_equal(r$counts, matrix(as.integer(cnt), 2))
  expect_equal(r$st_variogram, g, tolerance = 1e-12)
  expect_equal(r$space_lags, c(0.4, 1.2), tolerance = 1e-12)
  expect_equal(r$time_lags, c(0.5, 1.5), tolerance = 1e-12)
  M <- outer(c(0.4, 1.2), c(0.5, 1.5), mf)
  expect_equal(r$fitted, M, tolerance = 1e-12)
  ok <- cnt > 0
  expect_equal(r$wls_objective, sum(cnt[ok] / (2 * M[ok]^2) * (g[ok] - M[ok])^2), tolerance = 1e-12)
  at <- which(tm == 1)
  dd <- sqrt(sum((P[at[1], ] - P[at[2], ])^2))
  expect_equal(r$conditional$gamma[findInterval(dd, seq(0, 1.6, length.out = 3))], (z[at[1]] - z[at[2]])^2 / 2,
               tolerance = 1e-12)
  if (any(!ok)) expect_match(r$warning, "contain no pairs")
  expect_error(spstvg(P, tm[-1], z), "same length")
})

test_that("spwkth recovers the Gaussian spectral density", {
  om <- c(0, 0.5, 2)
  r <- spwkth(function(h) exp(-h^2), omega = om, h_max = 20, n = 4001)
  # f(w) = (1 / 2 pi) int exp(-h^2) cos(w h) dh = exp(-w^2 / 4) / (2 sqrt(pi));
  # the trapezoid rule is spectrally accurate for this smooth integrand
  expect_equal(r$spectral_density, exp(-om^2 / 4) / (2 * sqrt(pi)), tolerance = 1e-10)
  expect_equal(r$variance, 1)
  expect_equal(r$nyquist_omega, 0.5 * pi / 0.01, tolerance = 1e-12)
  expect_equal(r$integrated_density, 1, tolerance = 1e-6)
  expect_error(spwkth(1), "must be a function")
})
