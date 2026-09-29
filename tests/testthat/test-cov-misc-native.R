# Privacy accounting, Haar-wavelet and linear-autoencoder anomaly scores,
# V-trace (Espeholt et al. 2018), denoising score matching and the zonal
# energy-balance model, each recomputed from its defining formula.

test_that("basic and advanced composition", {
  eps <- c(0.1, 0.2, 0.15)
  r <- morie_basic_composition(eps, delta_prime = 1e-5, deltas = c(1e-6, 0, 1e-6))
  expect_equal(r$basic_epsilon, 0.45, tolerance = 1e-12)
  adv <- sqrt(2 * 3 * log(1e5)) * 0.2 + 3 * 0.2 * (exp(0.2) - 1)
  expect_equal(r$advanced_epsilon, adv, tolerance = 1e-12)
  expect_equal(r$total_delta, 2e-6 + 1e-5, tolerance = 1e-12)
  expect_identical(r$recommended, if (0.45 <= adv) "basic" else "advanced")
  many <- morie_basic_composition(rep(0.01, 2000), delta_prime = 1e-6)
  expect_identical(many$recommended, "advanced")
  expect_equal(many$epsilon, many$advanced_epsilon)
  expect_true(is.na(morie_basic_composition(eps)$advanced_epsilon))
  expect_error(morie_basic_composition(numeric(0)), "non-empty")
  expect_error(morie_basic_composition(c(0.1, -1)), "positive")
  expect_error(morie_basic_composition(eps, deltas = 1), "deltas has")
  expect_error(morie_basic_composition(eps, delta_prime = 1), "delta_prime")
})

test_that("zCDP composition converts with rho + 2 sqrt(rho log(1/delta))", {
  r <- morie_cdp_subgaussian_amplification(0.05, k_compositions = 4, delta = 1e-6)
  expect_equal(r$rho_total, 0.2, tolerance = 1e-12)
  expect_equal(r$epsilon, 0.2 + 2 * sqrt(0.2 * log(1e6)), tolerance = 1e-12)
  expect_equal(r$equivalent_sigma, sqrt(1 / 0.4), tolerance = 1e-12)
  v <- morie_cdp_subgaussian_amplification(c(0.1, 0.3), k_compositions = 7)
  expect_equal(v$rho_total, 0.4, tolerance = 1e-12)
  expect_identical(morie_cdp_subgaussian_amplification(0)$equivalent_sigma, Inf)
  expect_error(morie_cdp_subgaussian_amplification(-1), "non-negative")
  expect_error(morie_cdp_subgaussian_amplification(1, delta = 0), "delta")
  expect_error(morie_cdp_subgaussian_amplification(1, k_compositions = 0), "at least 1")
})

test_that("Laplace accuracy: scale Delta/(n eps), tail half-width b log(1/(1-c))", {
  r <- morie_private_accuracy_tradeoff(2, 0.5, 40, 0.9)
  b <- 2 / (40 * 0.5)
  expect_equal(r$noise_scale, b, tolerance = 1e-12)
  expect_equal(r$noise_sd, sqrt(2) * b, tolerance = 1e-12)
  expect_equal(r$half_width, b * log(10), tolerance = 1e-12)
  # P(|Laplace(b)| > half_width) = 1 - confidence
  expect_equal(exp(-r$half_width / b), 0.1, tolerance = 1e-12)
  expect_equal(r$noise_to_signal_n, 2 / 0.25, tolerance = 1e-12)
  expect_error(morie_private_accuracy_tradeoff(0), "sensitivity")
  expect_error(morie_private_accuracy_tradeoff(1, 1, 0), "n must")
  expect_error(morie_private_accuracy_tradeoff(1, 1, 5, 1), "confidence")
})

test_that("Haar wavelet anomaly scores use the MAD universal threshold", {
  x <- sin(seq(0, 6, length.out = 64))
  x[37] <- x[37] + 5
  r <- morie_discrete_wavelet_anomaly(x)
  d1 <- (x[seq(1, 63, 2)] - x[seq(2, 64, 2)]) / sqrt(2)
  sig <- stats::median(abs(d1 - stats::median(d1))) / 0.6745
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  expect_equal(r$threshold, sig * sqrt(2 * log(64)), tolerance = 1e-12)
  expect_true(r$anomaly[37])
  expect_identical(r$levels, 5L)
  # the level-1 detail at pair (37, 38) scores both points
  expect_equal(r$score[37], abs(d1[19]), tolerance = 1e-12)
  expect_identical(r$per_level_count[1], sum(abs(d1) > r$threshold))
  # non-power-of-two input is reflected up to 2^m and trimmed back
  y <- c(x[1:50])
  ry <- morie_discrete_wavelet_anomaly(y, threshold = 0.5, levels = 2)
  expect_length(ry$score, 50L)
  expect_identical(ry$threshold, 0.5)
  expect_error(morie_discrete_wavelet_anomaly(1:3), "at least 4")
})

test_that("the linear autoencoder converges to the principal subspace", {
  set.seed(2)
  n <- 60
  Z <- cbind(stats::rnorm(n, sd = 3), stats::rnorm(n, sd = 2), stats::rnorm(n, sd = 0.2))
  X <- Z %*% qr.Q(qr(matrix(c(1, 2, 0, 0, 1, 1, 1, 0, 1), 3)))
  X[7, ] <- X[7, ] + c(0, 0, 5)
  r <- morie_autoencoder_anomaly(X, k = 2, n_iter = 3000, lr = 0.02)
  Xc <- sweep(X, 2, colMeans(X))
  V <- svd(Xc)$v[, 1:2]
  err <- rowSums((Xc - Xc %*% V %*% t(V))^2)
  expect_equal(r$score, err, tolerance = 1e-8)
  expect_equal(r$explained_fraction, 1 - sum(err) / sum(Xc^2), tolerance = 1e-8)
  expect_identical(which.max(r$score), 7L)
  expect_identical(r$rank[7], 0L)
  expect_equal(r$threshold, unname(stats::quantile(r$score, 0.95)), tolerance = 1e-12)
  expect_error(morie_autoencoder_anomaly(X, k = 4), "between 1 and 3")
})

test_that("V-trace targets match the truncated importance-weighted sum", {
  rw <- c(1, 0, 0.5, 2, -1)
  V <- c(0.3, 0.1, -0.2, 0.8, 0.4)
  blp <- log(c(0.5, 0.2, 0.4, 0.9, 0.3))
  tlp <- log(c(0.7, 0.6, 0.1, 0.5, 0.6))
  g <- 0.9
  r <- morie_impala_vtrace(rw, V, blp, tlp, gamma = g, rho_bar = 1.2, c_bar = 0.9,
                           bootstrap_value = 0.25)
  ratio <- exp(tlp - blp)
  rho <- pmin(1.2, ratio)
  cc <- pmin(0.9, ratio)
  Vn <- c(V[-1], 0.25)
  dl <- rho * (rw + g * Vn - V)
  vs <- vapply(1:5, function(s) {
    V[s] + sum(vapply(s:5, function(t) {
      g^(t - s) * prod(cc[seq_len(t - s) + s - 1]) * dl[t]
    }, numeric(1)))
  }, numeric(1))
  expect_equal(r$vs, vs, tolerance = 1e-12)
  expect_equal(r$advantage, rho * (rw + g * c(vs[-1], 0.25) - V), tolerance = 1e-12)
  expect_identical(r$n_truncated_rho, sum(ratio > 1.2))
  # on-policy with no truncation, vs is the n-step discounted return
  on <- morie_impala_vtrace(rw, V, blp, blp, gamma = g, bootstrap_value = 0.25)
  ret <- vapply(1:5, function(s) sum(g^(0:(5 - s)) * rw[s:5]) + g^(6 - s) * 0.25, numeric(1))
  expect_equal(on$vs, ret, tolerance = 1e-12)
  expect_error(morie_impala_vtrace(rw, V[-1], blp, tlp), "must agree")
  expect_error(morie_impala_vtrace(rw, V, blp, tlp, gamma = 2), "gamma")
})

test_that("denoising score matching is zero for the conditional score", {
  X <- matrix(c(0.1, 0.4, -1, 2, 0.3, 0.7), 3, 2)
  s <- 0.3
  exact <- morie_diffusion_score_matching(X, function(xt) -(xt - X) / s^2, sigma = s)
  expect_equal(exact$objective, 0, tolerance = 1e-12)
  off <- morie_diffusion_score_matching(X, function(xt) -(xt - X) / s^2 + 1, sigma = s,
                                        n_noise = 5)
  expect_equal(off$per_sample, rep(2, 3), tolerance = 1e-12)
  expect_equal(off$target_norm, 1 / s^2)
  expect_error(morie_diffusion_score_matching(X, function(xt) xt[, 1], sigma = s), "score returned")
  expect_error(morie_diffusion_score_matching(X, identity, sigma = 0), "positive")
})

test_that("the zonal energy balance reaches its fixed point", {
  r <- morie_zonal_ebm(1, n_zones = 12, tol = 1e-12, max_iter = 5000)
  expect_true(r$converged)
  x <- seq(-1, 1, length.out = 13)
  xm <- (x[-1] + x[-13]) / 2
  S <- 1 - 0.482 * (1.5 * xm^2 - 0.5)
  al <- ifelse(r$temperature < -10, 0.62, 0.3)
  absorbed <- 1361 / 4 * S * (1 - al)
  expect_equal(r$temperature, (absorbed - 203.3 + 3.8 * mean(r$temperature)) / (2.09 + 3.8),
               tolerance = 1e-9)
  # zone average: B Tbar = mean(absorbed) - A
  expect_equal(2.09 * r$global_mean, mean(absorbed) - 203.3, tolerance = 1e-9)
  expect_equal(r$latitude, asin(xm) * 180 / pi, tolerance = 1e-12)
  expect_equal(r$ice_fraction, mean(r$temperature < -10))
  cold <- morie_zonal_ebm(0.8, n_zones = 12, start = -40, max_iter = 5000)
  expect_true(cold$snowball)
  expect_error(morie_zonal_ebm(1, n_zones = 1), "at least 2")
  expect_error(morie_zonal_ebm(1:3, n_zones = 4), "scalar")
})
