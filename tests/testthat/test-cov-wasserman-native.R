# Coverage tests for R/wasserman_native.R. Every expected value is
# recomputed in the test body from the textbook formula or base R.

wsm_lcg <- function(count, seed = 13) {
  s <- seed
  out <- numeric(count)
  for (i in seq_len(count)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    out[i] <- (s + 0.5) / 4294967296
  }
  out
}

wsm_boot_idx <- function(n, B, seed = 13) {
  u <- wsm_lcg(B * n, seed)
  matrix(pmin(floor(u * n), n - 1) + 1, nrow = B, byrow = TRUE)
}

test_that("variance, covariance, moments match base R", {
  x <- c(1.5, 2.25, 4, 7.5, 3)
  y <- c(2, 1, 5, 9.5, 2.5)
  v <- morie_wasserman_variance(x)
  expect_equal(v$estimate, var(x) * 4 / 5, tolerance = 1e-12)
  expect_equal(v$sample_variance, var(x), tolerance = 1e-12)
  expect_equal(v$second_moment, mean(x^2), tolerance = 1e-12)
  expect_equal(v$sd, sqrt(var(x) * 4 / 5), tolerance = 1e-12)
  expect_equal(morie_wasserman_variance(3)$sample_variance, 0)
  expect_error(morie_wasserman_variance(numeric(0)), "empty")
  cv <- morie_wasserman_covariance(x, y)
  expect_equal(cv$estimate, cov(x, y) * 4 / 5, tolerance = 1e-12)
  expect_equal(cv$sample_covariance, cov(x, y), tolerance = 1e-12)
  expect_equal(cv$correlation, cor(x, y), tolerance = 1e-12)
  expect_true(is.nan(morie_wasserman_covariance(c(1, 1), c(2, 3))$correlation))
  expect_error(morie_wasserman_covariance(1:3, 1:2), "equal length")
})

test_that("Chebyshev, Markov and Hoeffding bounds follow their formulas", {
  ch <- morie_wasserman_chebyshev_ineq(c(3, 0.5, 2))
  expect_equal(ch$raw_bounds, 1 / c(3, 0.5, 2)^2, tolerance = 1e-12)
  expect_equal(ch$bounds, pmin(1 / c(3, 0.5, 2)^2, 1), tolerance = 1e-12)
  expect_equal(ch$estimate, 1 / 9, tolerance = 1e-12)
  expect_error(morie_wasserman_chebyshev_ineq(c(1, -2)), "k > 0")
  mk <- morie_wasserman_markov_ineq(1.5, 6)
  expect_equal(mk$estimate, 0.25, tolerance = 1e-12)
  expect_equal(morie_wasserman_markov_ineq(7, 2)$estimate, 1)
  expect_equal(morie_wasserman_markov_ineq(7, 2)$raw_bound, 3.5)
  expect_error(morie_wasserman_markov_ineq(-1, 2), "E\\[X\\] >= 0")
  expect_error(morie_wasserman_markov_ineq(1, 0), "a > 0")
  h <- morie_wasserman_hoeffding(40, 0.15, -1, 2)
  e <- exp(-2 * 40 * 0.15^2 / 9)
  expect_equal(h$one_sided_raw, e, tolerance = 1e-12)
  expect_equal(h$two_sided_raw, 2 * e, tolerance = 1e-12)
  expect_equal(h$estimate, min(2 * e, 1), tolerance = 1e-12)
  expect_equal(morie_wasserman_hoeffding(1, 0.01, 0, 1)$estimate, 1)
  expect_error(morie_wasserman_hoeffding(10, 0.1, 1, 0), "a < b")
  expect_error(morie_wasserman_hoeffding(10, -0.1, 0, 1), "t > 0")
})

test_that("empirical CDF, quantile, DKW band", {
  d <- c(3.2, -1, 4.5, 0.7, 2.2, 3.2)
  F <- ecdf(d)
  r <- morie_wasserman_empirical_cdf(c(0, 3.2, 10, -5), d)
  expect_equal(r$values, F(c(0, 3.2, 10, -5)), tolerance = 1e-12)
  expect_error(morie_wasserman_empirical_cdf(1, numeric(0)), "empty")
  p <- c(0.1, 0.5, 0.51, 1)
  q <- morie_wasserman_empirical_quantile(d, p)
  expect_equal(q$values, unname(quantile(d, p, type = 1)), tolerance = 1e-12)
  expect_error(morie_wasserman_empirical_quantile(d, 0), "\\(0, 1\\]")
  b <- morie_wasserman_dkw_cb(d, 0.1)
  eps <- sqrt(log(2 / 0.1) / (2 * 6))
  expect_equal(b$estimate, eps, tolerance = 1e-12)
  expect_equal(b$lower, pmax((1:6) / 6 - eps, 0), tolerance = 1e-12)
  expect_equal(b$upper, pmin((1:6) / 6 + eps, 1), tolerance = 1e-12)
  expect_error(morie_wasserman_dkw_cb(d, 1), "alpha")
})

test_that("expectation by trapezoid is exact for a linear integrand", {
  g <- seq(0, 2, length.out = 11)
  r <- morie_wasserman_expectation(g, rep(0.5, 11))
  expect_equal(r$estimate, 1, tolerance = 1e-12)
  expect_equal(r$density_mass, 1, tolerance = 1e-12)
  g2 <- c(0, 0.5, 1.5, 3)
  f2 <- c(1, 2, 0.5, 0.25)
  xf <- g2 * f2
  expect_equal(morie_wasserman_expectation(g2, f2)$estimate,
    sum(diff(g2) * (xf[-1] + xf[-4]) / 2), tolerance = 1e-12)
  expect_error(morie_wasserman_expectation(c(0, 0), c(1, 1)), "increasing")
  expect_error(morie_wasserman_expectation(1:3, c(1, -1, 1)), "negative")
  expect_error(morie_wasserman_expectation(1, 1), "at least 2")
})

test_that("empirical MGF and characteristic function", {
  x <- c(0.3, -1.2, 2, 0.5)
  tt <- c(0, 0.7, -1.1)
  m <- morie_wasserman_mgf(x, tt)
  expect_equal(m$values, vapply(tt, function(t) mean(exp(t * x)), 0), tolerance = 1e-12)
  expect_equal(m$estimate, 1)
  cf <- morie_wasserman_char_fn(x, tt)
  z <- vapply(tt, function(t) mean(exp(1i * t * x)), 0i)
  expect_equal(cf$real, Re(z), tolerance = 1e-12)
  expect_equal(cf$imag, Im(z), tolerance = 1e-12)
  expect_equal(cf$modulus, Mod(z), tolerance = 1e-12)
  expect_error(morie_wasserman_mgf(numeric(0), 1), "empty")
  expect_error(morie_wasserman_char_fn(numeric(0), 1), "empty")
})

test_that("CLT z, LLN running means, delta method", {
  d <- c(2, 4.5, 1, 7, 3.3)
  c1 <- morie_wasserman_clt(d)
  expect_equal(c1$estimate, mean(d) / (sd(d) / sqrt(5)), tolerance = 1e-12)
  expect_equal(c1$se, sd(d) / sqrt(5), tolerance = 1e-12)
  expect_error(morie_wasserman_clt(c(1, 1, 1)), "constant")
  expect_error(morie_wasserman_clt(1), "n >= 2")
  l <- morie_wasserman_lln(d)
  expect_equal(l$running_means, cumsum(d) / seq_along(d), tolerance = 1e-12)
  expect_equal(l$last_gap, abs(mean(d) - mean(d[1:4])), tolerance = 1e-12)
  expect_true(is.nan(morie_wasserman_lln(2)$last_gap))
  expect_error(morie_wasserman_lln(numeric(0)), "empty")
  dm <- morie_wasserman_delta_method(2, 0.3, -4)
  expect_equal(dm$estimate, 1.2, tolerance = 1e-12)
  expect_equal(dm$variance, 1.44, tolerance = 1e-12)
  expect_error(morie_wasserman_delta_method(2, 0.3, 0), "degenerate")
  expect_error(morie_wasserman_delta_method(2, 0, 1), "se > 0")
})

test_that("bootstrap family reproduces the documented LCG resampling", {
  d <- c(1.2, 3.4, 0.5, 2.2, 5.1)
  B <- 30
  M <- wsm_boot_idx(5, B)
  reps <- apply(M, 1, function(i) mean(d[i]))
  np <- morie_wasserman_nonparametric_boot(d, NULL, B)
  expect_equal(np$estimate, mean(d), tolerance = 1e-12)
  expect_equal(np$replicates_mean, mean(reps), tolerance = 1e-12)
  expect_equal(np$se, sqrt(mean((reps - mean(reps))^2)), tolerance = 1e-12)
  expect_equal(np$se_unbiased, sd(reps), tolerance = 1e-12)
  med <- apply(M, 1, function(i) median(d[i]))
  np2 <- morie_wasserman_nonparametric_boot(d, median, B)
  expect_equal(np2$se_unbiased, sd(med), tolerance = 1e-12)
  expect_error(morie_wasserman_nonparametric_boot(d, NULL, 1), "B >= 2")
  s <- sort(reps)
  pc <- morie_wasserman_bootstrap_percentile(d, NULL, B, 0.2)
  expect_equal(pc$lower, s[ceiling(0.1 * B)], tolerance = 1e-12)
  expect_equal(pc$upper, s[ceiling(0.9 * B)], tolerance = 1e-12)
  expect_error(morie_wasserman_bootstrap_percentile(d, NULL, B, 0), "alpha")
  pv <- morie_wasserman_bootstrap_pivotal(d, NULL, B, 0.2)
  expect_equal(pv$lower, 2 * mean(d) - s[ceiling(0.9 * B)], tolerance = 1e-12)
  expect_equal(pv$upper, 2 * mean(d) - s[ceiling(0.1 * B)], tolerance = 1e-12)
  expect_error(morie_wasserman_bootstrap_pivotal(numeric(0), NULL, B, 0.2), "empty")
  U <- matrix(wsm_lcg(B * 5), nrow = B, byrow = TRUE)
  prep <- apply(U, 1, function(u) mean(-log(1 - u) * mean(d)))
  pb <- morie_wasserman_parametric_boot(d, NULL, NULL, B)
  expect_equal(pb$replicates_mean, mean(prep), tolerance = 1e-12)
  expect_equal(pb$se_unbiased, sd(prep), tolerance = 1e-12)
  pn <- morie_wasserman_parametric_boot(d, function(th, u) qnorm(u, th), NULL, B, seed = 7)
  U7 <- matrix(wsm_lcg(B * 5, 7), nrow = B, byrow = TRUE)
  expect_equal(pn$se, sqrt(mean((rowMeans(qnorm(U7, mean(d))) - mean(rowMeans(qnorm(U7, mean(d)))))^2)), tolerance = 1e-12)
  expect_error(morie_wasserman_parametric_boot(d, NULL, NULL, 0), "B >= 2")
})

test_that("influence function is the sensitivity curve", {
  d <- c(4, 1, 7, 2.5)
  r <- morie_wasserman_influence_function(d, NULL)
  expect_equal(r$influence, d - mean(d), tolerance = 1e-12)
  expect_equal(r$epsilon, 1 / 5)
  rm <- morie_wasserman_influence_function(d, median)
  expect_equal(rm$influence, 5 * (vapply(d, function(x) median(c(d, x)), 0) - median(d)), tolerance = 1e-12)
  expect_error(morie_wasserman_influence_function(numeric(0), NULL), "empty")
})

test_that("likelihood and log-likelihood, default and custom density", {
  d <- c(0.5, 2, 1.25)
  r <- morie_wasserman_likelihood(d, NULL, 1.5)
  expect_equal(r$log_likelihood, sum(dexp(d, 1 / 1.5, log = TRUE)), tolerance = 1e-12)
  expect_equal(r$estimate, prod(dexp(d, 1 / 1.5)), tolerance = 1e-12)
  expect_equal(morie_wasserman_likelihood(c(-1, 1), NULL, 1)$estimate, 0)
  f <- function(x, th) dnorm(x, th)
  expect_equal(morie_wasserman_likelihood(d, f, 0.3)$log_likelihood, sum(dnorm(d, 0.3, log = TRUE)), tolerance = 1e-12)
  expect_error(morie_wasserman_likelihood(d, NULL, -1), "theta > 0")
  expect_error(morie_wasserman_likelihood(d, function(x, th) -x, 1), "negative")
  ll <- morie_wasserman_log_likelihood(d, NULL, 1.5)
  expect_equal(ll$per_observation, dexp(d, 1 / 1.5, log = TRUE), tolerance = 1e-12)
  expect_equal(ll$estimate, sum(ll$per_observation), tolerance = 1e-12)
  expect_equal(morie_wasserman_log_likelihood(d, f, 1)$per_observation, dnorm(d, 1, log = TRUE), tolerance = 1e-12)
})

test_that("Cramer-Rao bound, Fisher information, MLE asymptotics", {
  cr <- morie_wasserman_cramer_rao(1, 8, 0.5)
  expect_equal(cr$estimate, 1 / 4, tolerance = 1e-12)
  expect_equal(cr$se_bound, 0.5, tolerance = 1e-12)
  expect_error(morie_wasserman_cramer_rao(1, 8, 0), "I\\(theta\\) > 0")
  expect_error(morie_wasserman_cramer_rao(1, 0, 1), "n >= 1")
  # the second difference in theta divides log-density roundoff
  # (|log f| up to 40, eps ~ 2e-16) by h^2 = 1e-10, so the numeric
  # information carries ~1e-5 relative noise: compared at 1e-5.
  fi <- morie_wasserman_fisher_info(NULL, 3)
  expect_equal(fi$estimate, 1 / 9, tolerance = 1e-5)
  g <- seq(-12, 12, length.out = 40001)
  fn <- morie_wasserman_fisher_info(function(x, th) dnorm(x, th, 2), 0.5, x_grid = g)
  expect_equal(fn$estimate, 1 / 4, tolerance = 1e-5)
  expect_error(morie_wasserman_fisher_info(function(x, th) dnorm(x, th), 0), "x_grid")
  expect_error(morie_wasserman_fisher_info(NULL, -1), "theta > 0")
  ma <- morie_wasserman_mle_asymptotic(1:50, NULL, 3)
  expect_equal(ma$se, 1 / sqrt(50 * fi$estimate), tolerance = 1e-12)
  expect_equal(ma$ci_upper - ma$ci_lower, 2 * qnorm(0.975) * ma$se, tolerance = 1e-12)
  expect_error(morie_wasserman_mle_asymptotic(numeric(0), NULL, 1), "empty")
})

test_that("White-Huber HC0 sandwich equals the explicit bread-meat-bread", {
  X <- cbind(1, c(0.5, 1.7, 2.2, 3.9, 4.1, 5.5, 6.3))
  y <- c(1.1, 2.9, 2.7, 5.2, 4.4, 6.9, 7.1)
  r <- morie_wasserman_white_huber(X, y)
  b <- solve(crossprod(X), crossprod(X, y))
  e <- as.numeric(y - X %*% b)
  V <- solve(crossprod(X)) %*% t(X) %*% diag(e^2) %*% X %*% solve(crossprod(X))
  expect_equal(r$beta, as.numeric(b), tolerance = 1e-12)
  expect_equal(r$covariance, as.numeric(t(V)), tolerance = 1e-12)
  expect_equal(r$robust_se, sqrt(diag(V)), tolerance = 1e-12)
  r2 <- morie_wasserman_white_huber(X, y, f = function(X, b) X %*% b)
  expect_equal(r2$robust_se, r$robust_se, tolerance = 1e-12)
  expect_error(morie_wasserman_white_huber(X[1:2, ], y[1:2]), "n > p")
  expect_error(morie_wasserman_white_huber(X, y[-1]), "rows")
})

test_that("EM mixture: one step matches the closed-form M-step; fit is a fixed point", {
  x <- c(-0.3, 0.1, 0.4, -0.1, 4.8, 5.3, 5.1, 4.6, 5.0)
  th <- c(0.4, -1, 6, 1.5, 1.2)
  estep <- function(p) {
    d1 <- (1 - p[1]) * dnorm(x, p[2], p[4])
    d2 <- p[1] * dnorm(x, p[3], p[5])
    d2 / (d1 + d2)
  }
  mstep <- function(g) {
    m1 <- sum((1 - g) * x) / sum(1 - g)
    m2 <- sum(g * x) / sum(g)
    c(mean(g), m1, m2, sqrt(sum((1 - g) * (x - m1)^2) / sum(1 - g)), sqrt(sum(g * (x - m2)^2) / sum(g)))
  }
  one <- morie_wasserman_em_algorithm(x, th, max_iter = 1L)
  exp1 <- mstep(estep(th))
  expect_equal(c(one$pi, one$mu1, one$mu2, one$sd1, one$sd2), exp1, tolerance = 1e-12)
  fit <- morie_wasserman_em_algorithm(x, th, max_iter = 500L, tol = 1e-13)
  expect_true(fit$converged)
  p <- c(fit$pi, fit$mu1, fit$mu2, fit$sd1, fit$sd2)
  expect_equal(mstep(estep(p)), p, tolerance = 1e-9)
  ll <- sum(log((1 - p[1]) * dnorm(x, p[2], p[4]) + p[1] * dnorm(x, p[3], p[5])))
  expect_equal(fit$log_likelihood, ll, tolerance = 1e-9)
  expect_error(morie_wasserman_em_algorithm(x, c(1, 0, 1, 1, 1)), "mixing weight")
  expect_error(morie_wasserman_em_algorithm(x, c(0.5, 0, 1, 0, 1)), "positive")
  g <- morie_wasserman_gmm_em(x, 2)
  xs <- sort(x)
  ref <- morie_wasserman_em_algorithm(x, c(0.5, xs[ceiling(0.25 * 9)], xs[ceiling(0.75 * 9)], sd(x), sd(x)))
  expect_equal(g$means, c(ref$mu1, ref$mu2), tolerance = 1e-12)
  expect_equal(g$weights, c(1 - ref$pi, ref$pi), tolerance = 1e-12)
  expect_equal(g$estimate, ref$log_likelihood, tolerance = 1e-12)
  expect_error(morie_wasserman_gmm_em(x, 3), "2-component")
  expect_error(morie_wasserman_gmm_em(rep(1, 5), 2), "constant")
})

test_that("chi-square goodness of fit equals chisq.test", {
  o <- c(18, 25, 31, 26)
  e <- c(20, 30, 25, 25)
  r <- morie_wasserman_chi_sq_gof(o, e)
  ct <- suppressWarnings(chisq.test(o, p = e / sum(e)))
  expect_equal(r$estimate, unname(ct$statistic), tolerance = 1e-12)
  expect_equal(r$p_value, ct$p.value, tolerance = 1e-12)
  expect_equal(r$per_cell, (o - e)^2 / e, tolerance = 1e-12)
  expect_error(morie_wasserman_chi_sq_gof(c(1, 2), c(1, 0)), "strictly positive")
  expect_error(morie_wasserman_chi_sq_gof(1, 1), "at least 2")
})

test_that("grid posterior, credible interval and posterior mean", {
  g <- seq(-3, 4, length.out = 1401)
  d <- c(0.4, 1.3, 0.9)
  r <- morie_wasserman_posterior(d, NULL, list(g, rep(1, 1401)))
  ll <- vapply(g, function(t) sum(dnorm(d, t, log = TRUE)), 0)
  un <- exp(ll - max(ll))
  trap <- function(y) sum(diff(g) * (y[-1] + y[-length(y)]) / 2)
  expect_equal(r$posterior, un / trap(un), tolerance = 1e-12)
  expect_equal(r$evidence, trap(exp(ll)), tolerance = 1e-12)
  # flat prior, N(theta, 1) likelihood: posterior N(mean(d), 1/3); the
  # grid covers +-5 sd, so quadrature error is far below 1e-6
  expect_equal(r$estimate, mean(d), tolerance = 1e-6)
  rc <- morie_wasserman_posterior(d, function(x, th) dexp(x, th), list(seq(0.01, 5, length.out = 500), rep(1, 500)))
  expect_equal(rc$map_theta, seq(0.01, 5, length.out = 500)[which.max(vapply(seq(0.01, 5, length.out = 500), function(t) sum(dexp(d, t, log = TRUE)), 0))])
  expect_error(morie_wasserman_posterior(d, NULL, list(c(1, 0), c(1, 1))), "increasing")
  expect_error(morie_wasserman_posterior(numeric(0), NULL, list(g, g)), "needs data")
  u <- seq(0, 2, length.out = 2001)
  ci <- morie_wasserman_credible_interval(list(u, rep(0.5, 2001)), 0.1)
  expect_equal(c(ci$lower, ci$upper), c(0.1, 1.9), tolerance = 1e-12)
  expect_equal(ci$mass_drift, 0, tolerance = 1e-12)
  expect_error(morie_wasserman_credible_interval(list(u, rep(0.5, 2001)), 1.5), "alpha")
  pm <- morie_wasserman_posterior_mean(list(u, rep(0.5, 2001)))
  expect_equal(pm$estimate, 1, tolerance = 1e-12)
  # trapezoid rule on x^2 carries O(h^2) error (h = 1e-3): 1e-6 tolerance
  expect_equal(pm$posterior_sd, sqrt(1 / 3), tolerance = 1e-6)
  expect_error(morie_wasserman_posterior_mean(list(u, rep(0, 2001))), "zero mass")
})

test_that("Savage-Dickey BF uses bw.nrd0 and a Gaussian KDE", {
  s <- c(-0.8, 0.3, 1.2, 0.5, -0.1, 0.9, 1.7, 0.2, -0.4, 0.6, 1.1)
  r <- morie_bayes_factor_savage_dickey(s, function(t) dnorm(t, 0, 2), 0)
  bw <- bw.nrd0(s)
  post0 <- mean(dnorm(0, s, bw))
  expect_equal(r$bandwidth, bw, tolerance = 1e-12)
  expect_equal(r$estimate, post0 / dnorm(0, 0, 2), tolerance = 1e-12)
  expect_equal(r$bf10, 1 / r$estimate, tolerance = 1e-12)
  r2 <- morie_bayes_factor_savage_dickey(s, 0.25, 0.5, bandwidth = 0.3)
  expect_equal(r2$estimate, mean(dnorm(0.5, s, 0.3)) / 0.25, tolerance = 1e-12)
  expect_error(morie_bayes_factor_savage_dickey(1:5, 1), "at least 10")
  expect_error(morie_bayes_factor_savage_dickey(s, 0), "positive")
})

test_that("entropy, KL divergence and mutual information", {
  p <- c(0.1, 0.2, 0.3, 0.4, 0)
  h <- morie_wasserman_entropy(p)
  expect_equal(h$estimate, -sum(p[p > 0] * log(p[p > 0])), tolerance = 1e-12)
  expect_equal(h$bits, h$estimate / log(2), tolerance = 1e-12)
  hd <- morie_wasserman_entropy(rep(0.25, 5), x_grid = seq(0, 4, 1))
  expect_equal(hd$estimate, log(4), tolerance = 1e-12)
  expect_equal(hd$form, "differential")
  expect_error(morie_wasserman_entropy(c(0.5, 0.6)), "sum to 1")
  q <- c(0.25, 0.25, 0.25, 0.25)
  pp <- c(0.1, 0.2, 0.3, 0.4)
  k <- morie_wasserman_kullback_leibler(pp, q)
  expect_equal(k$estimate, sum(pp * log(pp / q)), tolerance = 1e-12)
  expect_equal(k$reverse, sum(q * log(q / pp)), tolerance = 1e-12)
  expect_equal(morie_wasserman_kullback_leibler(c(0.5, 0.5), c(1, 0))$estimate, Inf)
  xg <- c(0, 1, 2)
  kc <- morie_wasserman_kullback_leibler(c(0.2, 0.6, 0.2), c(0.5, 0.5, 0), x_grid = xg)
  expect_equal(kc$estimate, Inf)
  kc2 <- morie_wasserman_kullback_leibler(c(0.2, 0.6, 0.2), c(0.4, 0.3, 0.3), x_grid = xg)
  term <- c(0.2, 0.6, 0.2) * log(c(0.2, 0.6, 0.2) / c(0.4, 0.3, 0.3))
  expect_equal(kc2$estimate, (term[1] + term[2]) / 2 + (term[2] + term[3]) / 2, tolerance = 1e-12)
  expect_error(morie_wasserman_kullback_leibler(c(0.5, 0.5), c(1, 0, 0)), "lengths differ")
  x <- c("a", "a", "b", "b", "a", "c")
  y <- c(1, 0, 1, 1, 0, 0)
  J <- table(x, y) / 6
  mi <- sum(J[J > 0] * log((J / outer(rowSums(J), colSums(J)))[J > 0]))
  m <- morie_wasserman_mutual_info(x, y)
  expect_equal(m$estimate, mi, tolerance = 1e-12)
  expect_equal(c(m$levels_x, m$levels_y), c(3L, 2L))
  expect_error(morie_wasserman_mutual_info(1:3, 1:2), "equal length")
})

test_that("odds ratio (Woolf) and relative risk (Katz)", {
  T <- rbind(c(12, 30), c(7, 41))
  o <- morie_wasserman_odds_ratio(T)
  lo <- log(12 * 41 / (30 * 7))
  se <- sqrt(1 / 12 + 1 / 30 + 1 / 7 + 1 / 41)
  expect_equal(o$estimate, 12 * 41 / (30 * 7), tolerance = 1e-12)
  expect_equal(c(o$ci_lower, o$ci_upper), exp(lo + c(-1, 1) * qnorm(0.975) * se), tolerance = 1e-12)
  expect_error(morie_wasserman_odds_ratio(rbind(c(0, 1), c(1, 1))), "zero cell")
  expect_error(morie_wasserman_odds_ratio(matrix(1, 3, 2)), "2x2")
  r <- morie_wasserman_relative_risk(T)
  p1 <- 12 / 42
  p0 <- 7 / 48
  se_r <- sqrt((1 - p1) / 12 + (1 - p0) / 7)
  expect_equal(r$estimate, p1 / p0, tolerance = 1e-12)
  expect_equal(r$se, se_r, tolerance = 1e-12)
  expect_equal(r$ci_upper, exp(log(p1 / p0) + qnorm(0.975) * se_r), tolerance = 1e-12)
  expect_error(morie_wasserman_relative_risk(rbind(c(0, 1), c(1, 1))), "zero event")
})

test_that("graph density and closeness centrality", {
  A <- matrix(0, 5, 5)
  ed <- rbind(c(1, 2), c(2, 3), c(3, 4), c(2, 5))
  A[ed] <- 1
  A[ed[, 2:1]] <- 1
  dn <- morie_density(A)
  expect_equal(dn$estimate, 4 / choose(5, 2), tolerance = 1e-12)
  expect_equal(dn$n_edges, 4)
  expect_error(morie_density(A + diag(5)), "self-loops")
  expect_error(morie_density(A * 2), "binary")
  D <- ifelse(A > 0, 1, Inf)
  diag(D) <- 0
  for (k in 1:5) for (i in 1:5) for (j in 1:5) D[i, j] <- min(D[i, j], D[i, k] + D[k, j])
  cl <- morie_sgt_closeness_centrality(A)
  expect_equal(cl$closeness, 4 / rowSums(D), tolerance = 1e-12)
  expect_equal(cl$argmax, which.max(4 / rowSums(D)) - 1L)
  B <- A
  B[3, 4] <- B[4, 3] <- 0
  expect_error(morie_sgt_closeness_centrality(B), "connected")
  expect_error(morie_sgt_closeness_centrality(matrix(0, 2, 3)), "square")
})

test_that("OLS, ridge and lasso", {
  X <- cbind(1, c(0.2, 1.1, 1.9, 3.2, 3.8, 5.1))
  y <- c(1.3, 2.9, 5.2, 7.1, 8.8, 11.4)
  fit <- lm(y ~ X[, 2])
  r <- morie_wasserman_least_squares(X, y)
  expect_equal(r$beta, unname(coef(fit)), tolerance = 1e-12)
  expect_equal(r$se, unname(summary(fit)$coefficients[, 2]), tolerance = 1e-12)
  expect_equal(r$r_squared, summary(fit)$r.squared, tolerance = 1e-12)
  expect_equal(r$sigma2, summary(fit)$sigma^2, tolerance = 1e-12)
  expect_error(morie_wasserman_least_squares(cbind(1, 1:4, 2:5), 1:4), "rank deficient")
  expect_error(morie_wasserman_least_squares(X[1:2, ], y[1:2]), "n > p")
  lam <- 0.7
  rg <- morie_wasserman_ridge(X, y, lam)
  expect_equal(rg$beta, as.numeric(solve(crossprod(X) + lam * diag(2), crossprod(X, y))), tolerance = 1e-12)
  d <- svd(X)$d
  expect_equal(rg$effective_df, sum(d^2 / (d^2 + lam)), tolerance = 1e-12)
  expect_equal(morie_wasserman_ridge(X, y, 0)$beta, r$beta, tolerance = 1e-12)
  expect_error(morie_wasserman_ridge(X, y, -1), "non-negative")
  # orthogonal columns: lasso solution is the per-column soft threshold
  Xo <- cbind(c(1, 1, 1, 1), c(1, -1, 1, -1), c(2, 0, -2, 0))
  yo <- c(3.1, -0.4, 1.7, 0.9)
  soft <- function(z, l) sign(z) * pmax(abs(z) - l, 0)
  la <- morie_wasserman_lasso(Xo, yo, 1.5)
  expect_equal(la$beta, soft(colSums(Xo * yo), 1.5) / colSums(Xo^2), tolerance = 1e-12)
  expect_equal(la$objective, 0.5 * sum((yo - Xo %*% la$beta)^2) + 1.5 * sum(abs(la$beta)), tolerance = 1e-12)
  expect_equal(la$n_nonzero, sum(la$beta != 0))
  expect_error(morie_wasserman_lasso(cbind(1, c(0, 0)), 1:2, 1), "all-zero")
  expect_error(morie_wasserman_lasso(Xo, yo, -1), "non-negative")
})

test_that("logistic and Poisson regression equal glm", {
  x <- c(-1.5, -0.7, 0.1, 0.4, 1.2, 1.9, -0.3, 0.8, 2.5, -2)
  yb <- c(0, 0, 1, 0, 1, 1, 0, 0, 1, 1)
  X <- cbind(1, x)
  g <- glm(yb ~ x, family = binomial(), control = glm.control(epsilon = 1e-14, maxit = 100))
  r <- morie_wasserman_logistic_regression(X, yb)
  expect_equal(r$beta, unname(coef(g)), tolerance = 1e-9)
  expect_equal(unname(r$se), unname(summary(g)$coefficients[, 2]), tolerance = 1e-9)
  expect_equal(r$log_likelihood, as.numeric(logLik(g)), tolerance = 1e-9)
  expect_error(morie_wasserman_logistic_regression(X, as.numeric(x > 0)), "separation")
  expect_error(morie_wasserman_logistic_regression(X, yb + 1), "binary")
  yc <- c(0, 1, 1, 2, 3, 6, 1, 2, 9, 0)
  gp <- glm(yc ~ x, family = poisson(), control = glm.control(epsilon = 1e-14, maxit = 100))
  rp <- morie_wasserman_poisson_regression(X, yc)
  expect_equal(rp$beta, unname(coef(gp)), tolerance = 1e-9)
  expect_equal(rp$log_likelihood, as.numeric(logLik(gp)), tolerance = 1e-9)
  expect_equal(rp$deviance, deviance(gp), tolerance = 1e-9)
  expect_equal(unname(rp$se), unname(summary(gp)$coefficients[, 2]), tolerance = 1e-9)
  expect_error(morie_wasserman_poisson_regression(X, yc + 0.5), "integers")
})

test_that("AIC, BIC and k-fold CV", {
  x <- c(0.3, 1.2, 2.2, 2.9, 4.1, 5.3, 5.8, 7.4)
  y <- c(1.1, 3.4, 4.9, 7.2, 9.1, 11.5, 12.2, 15.6)
  fit <- lm(y ~ x)
  ll <- as.numeric(logLik(fit))
  expect_equal(morie_wasserman_aic(ll, 3)$estimate, AIC(fit), tolerance = 1e-12)
  expect_equal(morie_wasserman_aic(ll, 3)$aic_wasserman, ll - 3, tolerance = 1e-12)
  expect_equal(morie_wasserman_bic(ll, 3, 8)$estimate, BIC(fit), tolerance = 1e-12)
  expect_equal(morie_wasserman_bic(ll, 3, 8)$bic_wasserman, ll - 1.5 * log(8), tolerance = 1e-12)
  expect_error(morie_wasserman_aic(ll, -1), "negative")
  expect_error(morie_wasserman_bic(ll, 1, 1), "n >= 2")
  X <- cbind(1, x)
  cv <- morie_wasserman_kfold_cv(X, y, NULL, 4)
  sq <- numeric(8)
  for (f in 1:4) {
    te <- (2 * f - 1):(2 * f)
    b <- coef(lm(y[-te] ~ x[-te]))
    sq[te] <- (y[te] - (b[1] + b[2] * x[te]))^2
  }
  expect_equal(cv$estimate, mean(sq), tolerance = 1e-12)
  expect_equal(cv$fold_mse, as.numeric(tapply(sq, rep(1:4, each = 2), mean)), tolerance = 1e-12)
  cm <- morie_wasserman_kfold_cv(X, y, function(Xtr, ytr, Xte) rep(mean(ytr), nrow(Xte)), 3)
  idx <- list(1:3, 4:5, 6:8)
  expect_equal(cm$fold_sizes, c(3L, 2L, 3L))
  expect_equal(cm$estimate, sum(vapply(idx, function(te) sum((y[te] - mean(y[-te]))^2), 0)) / 8, tolerance = 1e-12)
  expect_error(morie_wasserman_kfold_cv(X, y, NULL, 9), "2 <= k <= n")
})

test_that("kernel regression, local polynomial, smoothing spline", {
  xd <- c(0, 0.7, 1.5, 2.1, 3.3)
  yd <- c(1, 2.2, 2.9, 4.4, 5.1)
  kr <- morie_wasserman_kernel_regression(c(1, 2.5), xd, yd, 0.6)
  nw <- vapply(c(1, 2.5), function(x) sum(dnorm(x, xd, 0.6) * yd) / sum(dnorm(x, xd, 0.6)), 0)
  expect_equal(kr$values, nw, tolerance = 1e-12)
  expect_true(is.nan(morie_wasserman_kernel_regression(1e6, xd, yd, 1e-3)$estimate))
  expect_error(morie_wasserman_kernel_regression(1, xd, yd, 0), "bandwidth")
  lp0 <- morie_wasserman_local_polynomial(c(1, 2.5), xd, yd, 0.6, 0)
  expect_equal(lp0$values, nw, tolerance = 1e-12)
  lp1 <- morie_wasserman_local_polynomial(1.2, xd, yd, 0.6, 1)
  w <- dnorm(1.2, xd, 0.6)
  cf <- coef(lm(yd ~ I(xd - 1.2), weights = w))
  expect_equal(c(lp1$estimate, lp1$derivatives), unname(cf), tolerance = 1e-12)
  expect_error(morie_wasserman_local_polynomial(1, xd[1:2], yd[1:2], 1, 2), "n > p")
  x <- 0:5
  y <- c(0.5, 2.1, 1.4, 3.9, 3.2, 5.5)
  D <- diff(diag(6), differences = 2)
  S <- solve(diag(6) + 2 * crossprod(D))
  ss <- morie_wasserman_smoothing_spline(x, y, 2)
  expect_equal(ss$estimate, as.numeric(S %*% y), tolerance = 1e-12)
  expect_equal(ss$effective_df, sum(diag(S)), tolerance = 1e-12)
  expect_equal(morie_wasserman_smoothing_spline(x, y, 0)$estimate, y, tolerance = 1e-12)
  expect_error(morie_wasserman_smoothing_spline(c(0, 2, 1), 1:3, 1), "increasing")
})

test_that("PCA matches prcomp; k-means matches stats::kmeans Lloyd", {
  X <- cbind(c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3), c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7))
  p <- morie_wasserman_pca(X, 2)
  pr <- prcomp(X)
  expect_equal(p$eigenvalues, pr$sdev^2, tolerance = 1e-12)
  V <- matrix(p$components, 2, byrow = TRUE)
  expect_equal(unname(abs(crossprod(V, pr$rotation))), diag(2), tolerance = 1e-12)
  expect_equal(p$explained_ratio, 1, tolerance = 1e-12)
  expect_error(morie_wasserman_pca(X, 3), "k must lie")
  Y <- rbind(c(0, 0), c(0.3, 0.1), c(-0.2, 0.2), c(5, 5), c(5.2, 4.9), c(4.8, 5.3), c(10, 0), c(9.7, 0.4))
  km <- morie_wasserman_kmeans(Y, 3)
  lab <- km$labels + 1L
  wcss <- sum(vapply(1:3, function(j) sum(sweep(Y[lab == j, , drop = FALSE], 2, colMeans(Y[lab == j, , drop = FALSE]))^2), 0))
  expect_equal(km$estimate, wcss, tolerance = 1e-12)
  ref <- kmeans(Y, matrix(km$centers, 3, byrow = TRUE), algorithm = "Lloyd")
  expect_equal(ref$tot.withinss, wcss, tolerance = 1e-12)
  expect_equal(length(unique(lab[1:3])), 1L)
  expect_error(morie_wasserman_kmeans(Y, 9), "n >= k")
})

test_that("HMM forward and Viterbi agree with brute-force path enumeration", {
  A <- rbind(c(0.6, 0.3, 0.1), c(0.2, 0.5, 0.3), c(0.25, 0.25, 0.5))
  B <- rbind(c(0.7, 0.2, 0.1), c(0.1, 0.6, 0.3), c(0.2, 0.2, 0.6))
  pi0 <- c(0.5, 0.3, 0.2)
  obs <- c(0, 2, 1, 1)
  paths <- as.matrix(expand.grid(1:3, 1:3, 1:3, 1:3))
  pp <- apply(paths, 1, function(s) {
    pi0[s[1]] * B[s[1], obs[1] + 1] * prod(vapply(2:4, function(t) A[s[t - 1], s[t]] * B[s[t], obs[t] + 1], 0))
  })
  fw <- morie_wasserman_hmm_forward(obs, A, B, pi0)
  expect_equal(fw$estimate, log(sum(pp)), tolerance = 1e-12)
  expect_equal(fw$filtered, as.numeric(tapply(pp, paths[, 4], sum)) / sum(pp), tolerance = 1e-12)
  expect_equal(morie_wasserman_hmm_forward(c(0, 1), diag(2), diag(2), c(1, 0))$estimate, -Inf)
  expect_error(morie_wasserman_hmm_forward(c(0, 3), A, B, pi0), "outside")
  expect_error(morie_wasserman_hmm_forward(obs, A * 2, B, pi0), "sum to 1")
  vt <- morie_wasserman_viterbi(obs, A, B, pi0)
  expect_equal(vt$estimate, log(max(pp)), tolerance = 1e-12)
  expect_equal(vt$path, unname(paths[which.max(pp), ]) - 1L)
  expect_error(morie_wasserman_viterbi(integer(0), A, B, pi0), "at least one")
})

test_that("Gibbs and Metropolis chains follow the documented LCG recursion", {
  # the samplers use Acklam's normal quantile (|error| < 1.15e-9);
  # qnorm is exact, so chains are compared at 1e-8.
  u <- wsm_lcg(2 * 25)
  rho <- 0.6
  s <- sqrt(1 - rho^2)
  x <- 0.5
  y <- -1
  xs <- ys <- numeric(25)
  for (t in 1:25) {
    x <- rho * y + s * qnorm(u[2 * t - 1])
    y <- rho * x + s * qnorm(u[2 * t])
    xs[t] <- x
    ys[t] <- y
  }
  g <- morie_wasserman_gibbs_sampler(rho, c(0.5, -1), 25)
  expect_equal(g$samples_x, xs, tolerance = 1e-8)
  expect_equal(g$samples_y, ys, tolerance = 1e-8)
  expect_equal(g$estimate, cor(g$samples_x, g$samples_y), tolerance = 1e-12)
  expect_error(morie_wasserman_gibbs_sampler(1, c(0, 0), 5), "rho")
  tgt <- function(z) exp(-abs(z - 1))
  u2 <- wsm_lcg(2 * 40, 5)
  x <- 0
  acc <- 0
  ch <- numeric(40)
  for (t in 1:40) {
    pr <- x + 0.8 * qnorm(u2[2 * t - 1])
    if (u2[2 * t] < min(1, tgt(pr) / tgt(x))) {
      x <- pr
      acc <- acc + 1
    }
    ch[t] <- x
  }
  m <- morie_wasserman_mcmc_metropolis(tgt, 0.8, 0, 40, seed = 5)
  expect_equal(m$samples, ch, tolerance = 1e-8)
  expect_equal(m$acceptance_rate, acc / 40)
  expect_error(morie_wasserman_mcmc_metropolis(function(z) 0, 1, 0, 5), "positive")
  expect_error(morie_wasserman_mcmc_metropolis(tgt, 0, 0, 5), "proposal")
})

test_that("directed and undirected graphical models", {
  dag <- list(
    list(parents = integer(0), cpt = list(0.3)),
    list(parents = 0L, cpt = list(`0` = 0.2, `1` = 0.9)),
    list(parents = c(0L, 1L), cpt = list(`00` = 0.1, `01` = 0.5, `10` = 0.4, `11` = 0.8))
  )
  r <- morie_wasserman_directed_graph(dag, c(1, 0, 1))
  expect_equal(r$factors, c(0.3, 0.1, 0.4), tolerance = 1e-12)
  expect_equal(r$estimate, 0.3 * 0.1 * 0.4, tolerance = 1e-12)
  expect_equal(r$log_joint, log(0.012), tolerance = 1e-12)
  bad <- dag
  bad[[2]]$parents <- 2L
  expect_error(morie_wasserman_directed_graph(bad, c(1, 0, 1)), "topological")
  expect_error(morie_wasserman_directed_graph(dag, c(1, 2, 0)), "binary")
  psi1 <- function(t) 1 + t[1] + 2 * t[2]
  psi2 <- function(t) if (t[1] == 1) 3 else 0.5
  ug <- morie_wasserman_undirected_graph(list(3, list(c(0, 1), 2)), list(psi1, psi2))
  cfg <- as.matrix(expand.grid(x2 = 0:1, x1 = 0:1, x0 = 0:1))[, 3:1]
  w <- apply(cfg, 1, function(v) psi1(v[1:2]) * psi2(v[3]))
  expect_equal(ug$estimate, sum(w), tolerance = 1e-12)
  expect_equal(ug$probabilities, unname(w / sum(w)), tolerance = 1e-12)
  expect_error(morie_wasserman_undirected_graph(list(2, list(c(0, 5))), list(psi1)), "outside")
  expect_error(morie_wasserman_undirected_graph(list(2, list(c(0, 1))), list()), "potentials")
})

test_that("saturated log-linear model decomposes log counts", {
  T <- rbind(c(12, 30, 8), c(7, 41, 15))
  r <- morie_wasserman_log_linear(T)
  E <- suppressWarnings(chisq.test(T, correct = FALSE))$expected
  expect_equal(r$estimate, 2 * sum(T * log(T / E)), tolerance = 1e-12)
  expect_equal(r$independence_fit, as.numeric(t(E)), tolerance = 1e-12)
  li <- matrix(r$lambda_int, 2, byrow = TRUE)
  rec <- r$lambda0 + outer(r$lambda_row, rep(1, 3)) + outer(rep(1, 2), r$lambda_col) + li
  expect_equal(rec, log(T), tolerance = 1e-12)
  expect_equal(c(sum(r$lambda_row), sum(r$lambda_col), rowSums(li), colSums(li)), rep(0, 7), tolerance = 1e-12)
  expect_equal(r$df, 2L)
  expect_error(morie_wasserman_log_linear(rbind(c(0, 1), c(1, 1))), "strictly positive")
})

test_that("AdaBoost stumps and custom learner branch", {
  X <- matrix(c(0, 1, 2, 3), ncol = 1)
  r <- morie_wasserman_boosting(X, c(1, 1, -1, -1), NULL, 5)
  expect_equal(r$rounds_used, 1L)
  expect_equal(r$alphas, 10)
  expect_equal(r$estimate, 0)
  X2 <- matrix(c(0, 1, 2, 3, 4), ncol = 1)
  y2 <- c(1, -1, 1, 1, -1)
  r2 <- morie_wasserman_boosting(X2, y2, NULL, 1)
  # best first stump: x <= 0 -> +1 else -1 misclassifies x = 2, 3 (err 0.4);
  # x <= 3 -> +1 misclassifies x = 1 only (err 0.2), the unique minimum
  expect_equal(r2$alphas, 0.5 * log(0.8 / 0.2), tolerance = 1e-12)
  expect_equal(r2$prediction, c(1L, 1L, 1L, 1L, -1L))
  const <- function(X, y, w) function(Z) rep(1, nrow(Z))
  r3 <- morie_wasserman_boosting(X, c(1, -1, 1, -1), const, 3)
  expect_equal(r3$rounds_used, 0L)
  expect_equal(r3$estimate, 0.5)
  expect_error(morie_wasserman_boosting(X, c(1, 0, 1, 1), NULL, 1), "labels")
  expect_error(morie_wasserman_boosting(X, c(1, -1, 1, -1), NULL, 0), "T >= 1")
})

test_that("linear SVM recovers the hard-margin separator", {
  X <- rbind(c(-2, 0.5), c(-1, -0.3), c(1, 0.2), c(2.5, -0.4))
  y <- c(-1, -1, 1, 1)
  r <- morie_wasserman_svm(X, y)
  # the two inner points (-1, -0.3) and (1, 0.2) define the margin:
  # w is proportional to their difference and both sit at margin 1.
  dlt <- X[3, ] - X[2, ]
  w <- 2 * dlt / sum(dlt^2)
  b <- 1 - sum(w * X[3, ])
  expect_equal(r$w, w, tolerance = 1e-9)
  expect_equal(r$b, b, tolerance = 1e-9)
  expect_equal(r$estimate, 2 / sqrt(sum(w^2)), tolerance = 1e-9)
  expect_equal(r$support_vectors, c(1L, 2L))
  expect_equal(sum(r$alphas * y), 0, tolerance = 1e-12)
  expect_error(morie_wasserman_svm(X, rep(1, 4)), "both classes")
})

test_that("minimax over a finite risk matrix", {
  R <- rbind(c(2, 5, 1), c(3, 3, 4), c(6, 1, 2))
  r <- morie_wasserman_minimax(R, c("a", "b", "c"), c("f", "g", "h"))
  expect_equal(r$worst_case, apply(R, 1, max))
  expect_equal(r$estimate, min(apply(R, 1, max)))
  expect_equal(r$minimax_estimator, "b")
  expect_equal(r$maximin, max(apply(R, 2, min)))
  expect_false(r$has_pure_saddle)
  s <- morie_wasserman_minimax(rbind(c(1, 2), c(3, 4)), c("a", "b"), c("f", "g"))
  expect_true(s$has_pure_saddle)
  expect_error(morie_wasserman_minimax(R, c("a", "b"), c("f", "g", "h")), "estimator labels")
})

test_that("graphical model reports the normalised mode", {
  psi1 <- function(t) 1 + t[1] + 2 * t[2]
  psi2 <- function(t) if (t[1] == 1) 3 else 0.5
  g <- morie_wasserman_graphical_model(list(3, list(c(0, 1), 2)), list(psi1, psi2))
  cfg <- as.matrix(expand.grid(x2 = 0:1, x1 = 0:1, x0 = 0:1))[, 3:1]
  w <- apply(cfg, 1, function(v) psi1(v[1:2]) * psi2(v[3]))
  expect_equal(g$mode, unname(cfg[which.max(w), ]))
  expect_equal(g$estimate, max(w) / sum(w), tolerance = 1e-12)
  expect_equal(g$partition_function, sum(w), tolerance = 1e-12)
  expect_error(morie_wasserman_graphical_model(list(21, list()), list()), "20 nodes")
})
