# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Wasserman (2004), "All of Statistics" shelf -- native R ports of the
# morie.fn wsm*/bfsd/densty/sgtclo modules. Cross-language parity is
# anchored in tests/testthat/test-wasserman-parity.R against values
# GENERATED from the Python core, never recalled.
#
# Conventions carried over from the Python side, stated once:
#   * numpy's default variance/sd is POPULATION (ddof = 0); where a
#     payload reports the population value we use the centred mean of
#     squares, and stats::var (n - 1) only where the payload says so.
#   * matrices flatten ROW-MAJOR in the payloads (numpy .ravel()), so
#     R side uses t(M) before as.vector, and byrow = TRUE to rebuild.
#   * 0-based indices in payloads stay 0-based; +1 only when subsetting.

.morie_wsm_need <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

#' Variance Var(X) = E\\[X^2\\] - E\\[X\\]^2 (Wasserman Ch 3, morie.fn wsmvar)
#'
#' @param x Numeric sample, at least one observation.
#' @return List with `estimate` (population variance, divisor n),
#'   `sample_variance` (divisor n - 1), `mean`, `second_moment`, `sd`,
#'   `n`, `method`.
#' @examples
#' morie_wasserman_variance(c(1, 2, 3, 4))$estimate
#' @export
morie_wasserman_variance <- function(x) {
  x <- as.numeric(x)
  .morie_wsm_need(length(x) > 0, "variance of an empty sample is undefined.")
  n <- length(x); mu <- mean(x)
  var_pop <- mean((x - mu)^2)
  list(estimate = var_pop,
       sample_variance = if (n > 1L) stats::var(x) else 0,
       mean = mu, second_moment = mean(x^2), sd = sqrt(var_pop), n = n,
       method = "Variance Var(X) = E[X^2] - E[X]^2")
}

#' Chebyshev bound P(|X-mu| >= k sigma) <= 1/k^2 (Ch 4, wsmcby)
#'
#' @param k Numeric vector of positive multiples of sigma.
#' @return List with `estimate` (bound for the first k), `bounds`
#'   (capped at 1), `raw_bounds` (uncapped 1/k^2), `k`, `n`, `method`.
#' @examples
#' morie_wasserman_chebyshev_ineq(2)$estimate
#' @export
morie_wasserman_chebyshev_ineq <- function(k) {
  k <- as.numeric(k)
  .morie_wsm_need(all(k > 0),
                  sprintf("Chebyshev inequality needs k > 0; got %s.", k[k <= 0][1]))
  raw <- 1 / k^2
  capped <- pmin(raw, 1)
  list(estimate = capped[1], bounds = capped, raw_bounds = raw, k = k,
       n = length(k), method = "Chebyshev bound 1/k^2 (capped at 1)")
}

#' Empirical distribution function (Ch 7, wsmcdf)
#'
#' @param x Evaluation point(s).
#' @param data Observed sample, non-empty.
#' @return List with `estimate`, `values`, `x`, `n`, `method`.
#' @examples
#' morie_wasserman_empirical_cdf(2.5, c(1, 2, 3, 4))$estimate
#' @export
morie_wasserman_empirical_cdf <- function(x, data) {
  x <- as.numeric(x); data <- as.numeric(data)
  .morie_wsm_need(length(data) > 0, "the eCDF of an empty sample is undefined.")
  n <- length(data)
  vals <- vapply(x, function(xi) sum(data <= xi) / n, numeric(1))
  list(estimate = vals[1], values = vals, x = x, n = n,
       method = "eCDF F_n(x) = (1/n) sum I(X_i <= x)")
}

#' Expectation E\\[X\\] = int x f(x) dx by trapezoid (Ch 3, wsmexp)
#'
#' @param x Strictly increasing support grid, at least 2 points.
#' @param f Non-negative density values on the grid.
#' @return List with `estimate`, `density_mass`, `n`, `method`.
#' @examples
#' g <- seq(0, 1, length.out = 1001)
#' round(morie_wasserman_expectation(g, rep(1, 1001))$estimate, 6)
#' @export
morie_wasserman_expectation <- function(x, f) {
  x <- as.numeric(x); f <- as.numeric(f)
  .morie_wsm_need(length(x) >= 2L,
                  "the grid needs at least 2 points for a trapezoid rule.")
  .morie_wsm_need(length(x) == length(f),
                  sprintf("grid (%d) and density (%d) lengths differ.", length(x), length(f)))
  .morie_wsm_need(all(diff(x) > 0), "the grid must be strictly increasing.")
  .morie_wsm_need(all(f >= 0), "a density cannot be negative.")
  dx <- diff(x); xf <- x * f
  list(estimate = sum(0.5 * dx * (xf[-1] + xf[-length(xf)])),
       density_mass = sum(0.5 * dx * (f[-1] + f[-length(f)])),
       n = length(x), method = "E[X] = int x f(x) dx (trapezoid)")
}

#' Covariance Cov(X,Y) = E\\[XY\\] - E\\[X\\]E\\[Y\\] (Ch 4, wsmcov)
#'
#' @param x,y Paired numeric samples of equal length.
#' @return List with `estimate` (population, divisor n),
#'   `sample_covariance`, `correlation`, `mean_x`, `mean_y`, `n`, `method`.
#' @examples
#' morie_wasserman_covariance(c(1, 2, 3), c(2, 4, 6))$correlation
#' @export
morie_wasserman_covariance <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  .morie_wsm_need(length(x) == length(y),
                  sprintf("paired samples must have equal length; got %d and %d.",
                          length(x), length(y)))
  n <- length(x)
  .morie_wsm_need(n > 0, "covariance of an empty sample is undefined.")
  mx <- mean(x); my <- mean(y)
  cov_pop <- mean((x - mx) * (y - my))
  sx <- sqrt(mean((x - mx)^2)); sy <- sqrt(mean((y - my)^2))
  list(estimate = cov_pop,
       sample_covariance = if (n > 1L) cov_pop * n / (n - 1) else 0,
       correlation = if (sx > 0 && sy > 0) cov_pop / (sx * sy) else NaN,
       mean_x = mx, mean_y = my, n = n,
       method = "Cov(X,Y) = E[XY] - E[X]E[Y] (population divisor n)")
}

#' Markov bound P(X >= a) <= E\\[X\\]/a for X >= 0 (Ch 4, wsmmrk)
#'
#' @param mean Non-negative expectation E\\[X\\].
#' @param a Strictly positive threshold.
#' @return List with `estimate` (capped at 1), `raw_bound`, `mean`, `a`, `method`.
#' @examples
#' morie_wasserman_markov_ineq(1, 4)$estimate
#' @export
morie_wasserman_markov_ineq <- function(mean, a) {
  mean <- as.numeric(mean)[1]; a <- as.numeric(a)[1]
  .morie_wsm_need(mean >= 0, sprintf(
    "Markov's inequality needs E[X] >= 0 (X non-negative); got %s.", mean))
  .morie_wsm_need(a > 0, sprintf("Markov's inequality needs a > 0; got %s.", a))
  raw <- mean / a
  list(estimate = min(raw, 1), raw_bound = raw, mean = mean, a = a,
       method = "Markov bound E[X]/a (capped at 1)")
}

#' Hoeffding bound for bounded variables (Ch 4, wsmhfd)
#'
#' @param n Sample size, at least 1.
#' @param t Positive deviation.
#' @param a,b Support bounds with a < b.
#' @return List with `estimate` (two-sided, capped), `two_sided_raw`,
#'   `one_sided`, `one_sided_raw`, `n`, `t`, `a`, `b`, `method`.
#' @examples
#' morie_wasserman_hoeffding(100, 0.1, 0, 1)$estimate
#' @export
morie_wasserman_hoeffding <- function(n, t, a, b) {
  n <- as.integer(n); t <- as.numeric(t)[1]
  a <- as.numeric(a)[1]; b <- as.numeric(b)[1]
  .morie_wsm_need(n >= 1L, sprintf("Hoeffding needs n >= 1; got %d.", n))
  .morie_wsm_need(t > 0, sprintf("Hoeffding needs t > 0; got %s.", t))
  .morie_wsm_need(a < b, sprintf("Hoeffding needs a < b; got a=%s, b=%s.", a, b))
  expo <- exp(-2 * n * t^2 / (b - a)^2)
  list(estimate = min(2 * expo, 1), two_sided_raw = 2 * expo,
       one_sided = min(expo, 1), one_sided_raw = expo,
       n = n, t = t, a = a, b = b,
       method = "Hoeffding 2 exp(-2 n t^2/(b-a)^2) (capped at 1)")
}

#' Empirical MGF M(t) = (1/n) sum e^{t X_i} (Ch 3, wsmmgf)
#'
#' @param x Numeric sample, non-empty.
#' @param t Evaluation point(s).
#' @return List with `estimate`, `values`, `t`, `n`, `method`.
#' @examples
#' morie_wasserman_mgf(c(1, 2), 0)$estimate
#' @export
morie_wasserman_mgf <- function(x, t) {
  x <- as.numeric(x); t <- as.numeric(t)
  .morie_wsm_need(length(x) > 0, "the MGF of an empty sample is undefined.")
  vals <- vapply(t, function(ti) mean(exp(ti * x)), numeric(1))
  list(estimate = vals[1], values = vals, t = t, n = length(x),
       method = "empirical MGF (1/n) sum e^{tX_i}")
}

#' Empirical characteristic function (Ch 3, wsmcfn)
#'
#' @param x Numeric sample, non-empty.
#' @param t Evaluation point(s).
#' @return List with `estimate` (|phi| at first t), `real`, `imag`,
#'   `modulus`, `t`, `n`, `method`.
#' @examples
#' morie_wasserman_char_fn(c(1, 2, 3), 0)$estimate
#' @export
morie_wasserman_char_fn <- function(x, t) {
  x <- as.numeric(x); t <- as.numeric(t)
  .morie_wsm_need(length(x) > 0,
                  "the characteristic function of an empty sample is undefined.")
  re <- vapply(t, function(ti) mean(cos(ti * x)), numeric(1))
  im <- vapply(t, function(ti) mean(sin(ti * x)), numeric(1))
  mod <- sqrt(re^2 + im^2)
  list(estimate = mod[1], real = re, imag = im, modulus = mod, t = t,
       n = length(x), method = "empirical phi(t) = mean cos(tX) + i mean sin(tX)")
}

#' CLT standardised mean (Ch 5, wsmclt)
#'
#' @param data Numeric sample with at least 2 non-constant values.
#' @return List with `estimate` (z for mu0 = 0), `mean`, `sd` (n - 1),
#'   `se`, `n`, `method`.
#' @examples
#' morie_wasserman_clt(c(1, 2, 3, 4))$mean
#' @export
morie_wasserman_clt <- function(data) {
  data <- as.numeric(data); n <- length(data)
  .morie_wsm_need(n >= 2L, "CLT standardisation needs n >= 2 for a sample sd.")
  s <- stats::sd(data)
  .morie_wsm_need(s > 0, "a constant sample has sd 0; z is undefined.")
  se <- s / sqrt(n)
  list(estimate = mean(data) / se, mean = mean(data), sd = s, se = se, n = n,
       method = "CLT z = sqrt(n)(X_bar - mu0)/s, mu0 = 0")
}

#' Law of large numbers: running means (Ch 5, wsmlln)
#'
#' @param data Numeric sample in observation order, non-empty.
#' @return List with `estimate` (final running mean), `running_means`,
#'   `last_gap`, `n`, `method`.
#' @examples
#' morie_wasserman_lln(c(2, 4, 6))$running_means
#' @export
morie_wasserman_lln <- function(data) {
  data <- as.numeric(data); n <- length(data)
  .morie_wsm_need(n > 0, "the running mean of an empty sample is undefined.")
  running <- cumsum(data) / seq_len(n)
  list(estimate = running[n], running_means = running,
       last_gap = if (n > 1L) abs(running[n] - running[n - 1L]) else NaN,
       n = n, method = "LLN running means X_bar_1..X_bar_n")
}

#' Delta method standard error (Ch 5, wsmdlm)
#'
#' @param theta_hat Point estimate.
#' @param se Positive standard error of theta_hat.
#' @param g_prime Nonzero derivative g'(theta) at theta_hat.
#' @return List with `estimate` (se of g), `variance`, `theta_hat`,
#'   `se_theta`, `g_prime`, `method`.
#' @examples
#' morie_wasserman_delta_method(3, 0.5, 6)$estimate
#' @export
morie_wasserman_delta_method <- function(theta_hat, se, g_prime) {
  theta_hat <- as.numeric(theta_hat)[1]
  se <- as.numeric(se)[1]; g_prime <- as.numeric(g_prime)[1]
  .morie_wsm_need(se > 0, sprintf("the delta method needs se > 0; got %s.", se))
  .morie_wsm_need(g_prime != 0,
                  "g'(theta) = 0: first-order delta method degenerate; use second order.")
  se_g <- abs(g_prime) * se
  list(estimate = se_g, variance = se_g^2, theta_hat = theta_hat,
       se_theta = se, g_prime = g_prime,
       method = "delta method se(g) = |g'| se(theta)")
}

#' Empirical quantile, Hyndman-Fan type 1 (Ch 7, wsmqtl)
#'
#' @param data Numeric sample, non-empty.
#' @param p Probability level(s) in (0, 1\\].
#' @return List with `estimate`, `values`, `p`, `n`, `method`.
#' @examples
#' morie_wasserman_empirical_quantile(c(3, 1, 4, 2), 0.5)$estimate
#' @export
morie_wasserman_empirical_quantile <- function(data, p) {
  data <- sort(as.numeric(data)); p <- as.numeric(p); n <- length(data)
  .morie_wsm_need(n > 0, "the quantile of an empty sample is undefined.")
  .morie_wsm_need(all(p > 0 & p <= 1), sprintf(
    "quantile levels must lie in (0, 1]; got %s.", p[p <= 0 | p > 1][1]))
  vals <- data[ceiling(p * n)]
  list(estimate = vals[1], values = vals, p = p, n = n,
       method = "type-1 quantile q_p = X_(ceil(np))")
}

#' DKW confidence band for F (Ch 7 Thm 7.5, wsmcb)
#'
#' @param data Numeric sample, non-empty.
#' @param alpha Level in (0, 1).
#' @return List with `estimate` (half-width eps), `lower`, `upper`,
#'   `ecdf`, `x_sorted`, `alpha`, `n`, `method`.
#' @examples
#' morie_wasserman_dkw_cb(c(1, 2, 5), 0.05)$estimate
#' @export
morie_wasserman_dkw_cb <- function(data, alpha) {
  data <- sort(as.numeric(data)); alpha <- as.numeric(alpha)[1]
  n <- length(data)
  .morie_wsm_need(n > 0, "the DKW band of an empty sample is undefined.")
  .morie_wsm_need(alpha > 0 && alpha < 1,
                  sprintf("alpha must lie in (0, 1); got %s.", alpha))
  eps <- sqrt(log(2 / alpha) / (2 * n))
  ecdf_v <- seq_len(n) / n
  list(estimate = eps, lower = pmax(ecdf_v - eps, 0),
       upper = pmin(ecdf_v + eps, 1), ecdf = ecdf_v, x_sorted = data,
       alpha = alpha, n = n,
       method = "DKW band F_n +/- sqrt(log(2/alpha)/(2n))")
}

# --- bootstrap family -------------------------------------------------
# The shared exact-integer LCG (s <- (1664525 s + 1013904223) mod 2^32,
# u = (s + 0.5)/2^32) is the same stream the Python side uses, so the
# resampling indices -- and therefore every replicate -- agree
# bit-for-bit across the two languages.

.morie_wsm_lcg_u <- function(count, seed = 13) {
  s <- as.numeric(seed); out <- numeric(count)
  m <- 4294967296
  for (i in seq_len(count)) {
    s <- (1664525 * s + 1013904223) %% m
    out[i] <- (s + 0.5) / m
  }
  out
}

.morie_wsm_boot_reps <- function(data, T, B, seed) {
  n <- length(data)
  u <- .morie_wsm_lcg_u(B * n, seed)
  idx <- pmin(as.integer(u * n), n - 1L) + 1L
  M <- matrix(idx, nrow = B, ncol = n, byrow = TRUE)
  vapply(seq_len(B), function(b) as.numeric(T(data[M[b, ]])), numeric(1))
}

.morie_wsm_q1 <- function(sorted_vals, p) sorted_vals[ceiling(p * length(sorted_vals))]

#' Nonparametric bootstrap standard error (Ch 8, wsmnpb)
#'
#' @param data Numeric sample, non-empty.
#' @param T Statistic mapping a numeric vector to a scalar; NULL = mean.
#' @param B Replications, at least 2.
#' @param seed LCG seed (default 13), matching the Python default.
#' @return List with `estimate`, `se` (1/B divisor), `se_unbiased`,
#'   `replicates_mean`, `B`, `n`, `method`.
#' @examples
#' morie_wasserman_nonparametric_boot(c(1, 2, 3, 4), NULL, 50)$estimate
#' @export
morie_wasserman_nonparametric_boot <- function(data, T, B, seed = 13) {
  data <- as.numeric(data); B <- as.integer(B); n <- length(data)
  .morie_wsm_need(n > 0, "the bootstrap of an empty sample is undefined.")
  .morie_wsm_need(B >= 2L, sprintf("the bootstrap needs B >= 2; got %d.", B))
  if (is.null(T)) T <- function(a) mean(a)
  reps <- .morie_wsm_boot_reps(data, T, B, seed)
  rbar <- mean(reps)
  list(estimate = as.numeric(T(data)),
       se = sqrt(mean((reps - rbar)^2)),
       se_unbiased = sqrt(sum((reps - rbar)^2) / (B - 1)),
       replicates_mean = rbar, B = B, n = n,
       method = "nonparametric bootstrap, LCG resampling, 1/B divisor")
}

#' Bootstrap percentile interval (Ch 8, wsmbpc)
#'
#' @param data Numeric sample, non-empty.
#' @param T Statistic; NULL = mean.
#' @param B Replications, at least 2.
#' @param alpha Level in (0, 1).
#' @param seed LCG seed.
#' @return List with `estimate`, `lower`, `upper`, `alpha`, `B`, `n`, `method`.
#' @examples
#' morie_wasserman_bootstrap_percentile(c(1, 2, 3, 4), NULL, 50, 0.1)$estimate
#' @export
morie_wasserman_bootstrap_percentile <- function(data, T, B, alpha, seed = 13) {
  data <- as.numeric(data); B <- as.integer(B); alpha <- as.numeric(alpha)[1]
  .morie_wsm_need(length(data) > 0, "the bootstrap of an empty sample is undefined.")
  .morie_wsm_need(B >= 2L, sprintf("the bootstrap needs B >= 2; got %d.", B))
  .morie_wsm_need(alpha > 0 && alpha < 1,
                  sprintf("alpha must lie in (0, 1); got %s.", alpha))
  if (is.null(T)) T <- function(a) mean(a)
  reps <- sort(.morie_wsm_boot_reps(data, T, B, seed))
  list(estimate = as.numeric(T(data)),
       lower = .morie_wsm_q1(reps, alpha / 2),
       upper = .morie_wsm_q1(reps, 1 - alpha / 2),
       alpha = alpha, B = B, n = length(data),
       method = "bootstrap percentile CI, type-1 quantiles, LCG")
}

#' Bootstrap pivotal interval (Ch 8, wsmbpv)
#'
#' @param data Numeric sample, non-empty.
#' @param T Statistic; NULL = mean.
#' @param B Replications, at least 2.
#' @param alpha Level in (0, 1).
#' @param seed LCG seed.
#' @return List with `estimate`, `lower`, `upper`, `alpha`, `B`, `n`, `method`.
#' @examples
#' morie_wasserman_bootstrap_pivotal(c(1, 2, 3, 4), NULL, 50, 0.1)$lower
#' @export
morie_wasserman_bootstrap_pivotal <- function(data, T, B, alpha, seed = 13) {
  data <- as.numeric(data); B <- as.integer(B); alpha <- as.numeric(alpha)[1]
  .morie_wsm_need(length(data) > 0, "the bootstrap of an empty sample is undefined.")
  .morie_wsm_need(B >= 2L, sprintf("the bootstrap needs B >= 2; got %d.", B))
  .morie_wsm_need(alpha > 0 && alpha < 1,
                  sprintf("alpha must lie in (0, 1); got %s.", alpha))
  if (is.null(T)) T <- function(a) mean(a)
  theta <- as.numeric(T(data))
  reps <- sort(.morie_wsm_boot_reps(data, T, B, seed))
  list(estimate = theta,
       lower = 2 * theta - .morie_wsm_q1(reps, 1 - alpha / 2),
       upper = 2 * theta - .morie_wsm_q1(reps, alpha / 2),
       alpha = alpha, B = B, n = length(data),
       method = "bootstrap pivotal CI (2 theta - q*_{1-a/2}, 2 theta - q*_{a/2})")
}

#' Parametric bootstrap (Ch 8, wsmprb)
#'
#' @param data Observed numeric sample.
#' @param f Sampler f(theta_hat, u) -> draws; NULL = exponential inversion.
#' @param T Statistic; NULL = mean.
#' @param B Replications, at least 2.
#' @param seed LCG seed.
#' @return List with `estimate`, `se`, `se_unbiased`, `replicates_mean`,
#'   `B`, `n`, `method`.
#' @examples
#' morie_wasserman_parametric_boot(c(1, 2, 3, 4), NULL, NULL, 50)$estimate
#' @export
morie_wasserman_parametric_boot <- function(data, f, T, B, seed = 13) {
  data <- as.numeric(data); B <- as.integer(B); n <- length(data)
  .morie_wsm_need(n > 0, "the bootstrap of an empty sample is undefined.")
  .morie_wsm_need(B >= 2L, sprintf("the bootstrap needs B >= 2; got %d.", B))
  if (is.null(T)) T <- function(a) mean(a)
  if (is.null(f)) f <- function(theta, u) -log(1 - u) * theta
  theta_hat <- as.numeric(T(data))
  u <- matrix(.morie_wsm_lcg_u(B * n, seed), nrow = B, ncol = n, byrow = TRUE)
  reps <- vapply(seq_len(B),
                 function(b) as.numeric(T(as.numeric(f(theta_hat, u[b, ])))),
                 numeric(1))
  rbar <- mean(reps)
  list(estimate = theta_hat, se = sqrt(mean((reps - rbar)^2)),
       se_unbiased = sqrt(sum((reps - rbar)^2) / (B - 1)),
       replicates_mean = rbar, B = B, n = n,
       method = "parametric bootstrap, inversion sampler f(theta,u), LCG")
}

#' Empirical influence via the sensitivity curve (Ch 7, wsmifn)
#'
#' @param data Numeric sample, non-empty.
#' @param T Statistic; NULL = mean.
#' @return List with `estimate`, `influence`, `epsilon`,
#'   `mean_influence`, `n`, `method`.
#' @examples
#' morie_wasserman_influence_function(c(1, 2, 3, 4), NULL)$influence
#' @export
morie_wasserman_influence_function <- function(data, T) {
  data <- as.numeric(data); n <- length(data)
  .morie_wsm_need(n > 0, "the influence function of an empty sample is undefined.")
  if (is.null(T)) T <- function(a) mean(a)
  base <- as.numeric(T(data))
  infl <- vapply(data, function(xi) (n + 1) * (as.numeric(T(c(data, xi))) - base),
                 numeric(1))
  list(estimate = base, influence = infl, epsilon = 1 / (n + 1),
       mean_influence = mean(infl), n = n,
       method = "sensitivity curve SC(x) = (n+1)(T(data+x) - T(data))")
}

# --- likelihood and information ---------------------------------------

#' Likelihood L(theta) = prod f(X_i; theta) (Ch 9, wsmlik)
#'
#' Evaluated through the log domain so long samples cannot underflow
#' before the product finishes. A zero density anywhere makes L exactly
#' 0 (log-likelihood -Inf), reported rather than signalled.
#'
#' @param data Numeric sample, non-empty.
#' @param f Density f(x, theta) vectorised in x; NULL = exponential(theta).
#' @param theta Parameter value.
#' @return List with `estimate` (L), `log_likelihood`, `theta`, `n`, `method`.
#' @examples
#' round(morie_wasserman_likelihood(c(1, 2), NULL, 1)$log_likelihood, 12)
#' @export
morie_wasserman_likelihood <- function(data, f, theta) {
  data <- as.numeric(data); theta <- as.numeric(theta)[1]
  .morie_wsm_need(length(data) > 0, "the likelihood on an empty sample is undefined.")
  if (is.null(f)) {
    .morie_wsm_need(theta > 0,
                    sprintf("the exponential model needs theta > 0; got %s.", theta))
    f <- function(x, th) ifelse(x >= 0, exp(-x / th) / th, 0)
  }
  dens <- as.numeric(f(data, theta))
  .morie_wsm_need(all(dens >= 0), "a density cannot be negative.")
  ll <- suppressWarnings(sum(log(dens)))
  list(estimate = if (is.finite(ll)) exp(ll) else 0, log_likelihood = ll,
       theta = theta, n = length(data),
       method = "L(theta) = prod f(X_i;theta) via log domain")
}

#' Log-likelihood with per-observation terms (Ch 9, wsmllk)
#'
#' @param data Numeric sample, non-empty.
#' @param f Density f(x, theta); NULL = exponential(theta).
#' @param theta Parameter value.
#' @return List with `estimate` (l), `per_observation`, `likelihood`,
#'   `theta`, `n`, `method`.
#' @examples
#' morie_wasserman_log_likelihood(c(1, 2), NULL, 1)$per_observation
#' @export
morie_wasserman_log_likelihood <- function(data, f, theta) {
  core <- morie_wasserman_likelihood(data, f, theta)
  data <- as.numeric(data); theta <- as.numeric(theta)[1]
  if (is.null(f)) f <- function(x, th) ifelse(x >= 0, exp(-x / th) / th, 0)
  per <- suppressWarnings(log(as.numeric(f(data, theta))))
  list(estimate = core$log_likelihood, per_observation = per,
       likelihood = core$estimate, theta = theta, n = length(data),
       method = "l(theta) = sum log f(X_i;theta)")
}

#' Cramer-Rao lower bound (Ch 9 Thm 9.23, wsmcrl)
#'
#' @param theta Parameter value the information was evaluated at.
#' @param n Sample size, at least 1.
#' @param I Per-observation Fisher information, strictly positive.
#' @return List with `estimate` (bound), `se_bound`, `theta`, `n`,
#'   `information`, `method`.
#' @examples
#' morie_wasserman_cramer_rao(0, 25, 0.25)$estimate
#' @export
morie_wasserman_cramer_rao <- function(theta, n, I) {
  theta <- as.numeric(theta)[1]; n <- as.integer(n); I <- as.numeric(I)[1]
  .morie_wsm_need(n >= 1L, sprintf("the Cramer-Rao bound needs n >= 1; got %d.", n))
  .morie_wsm_need(I > 0,
                  sprintf("the Cramer-Rao bound needs I(theta) > 0; got %s.", I))
  bound <- 1 / (n * I)
  list(estimate = bound, se_bound = sqrt(bound), theta = theta, n = n,
       information = I, method = "Cramer-Rao Var(T) >= 1/(n I(theta))")
}

#' Fisher information by numeric curvature (Ch 9, wsmfis)
#'
#' I(theta) = -E\\[d^2 log f / d theta^2\\], the second derivative taken as
#' a central difference in theta and the expectation by trapezoid
#' quadrature on `x_grid`. `f = NULL` selects the exponential model
#' (exact information 1/theta^2) with a default grid over \\[0, 40 theta\\].
#'
#' @param f Density f(x, theta) vectorised in x; NULL = exponential.
#' @param theta Parameter value.
#' @param x_grid Support grid; REQUIRED when f is supplied.
#' @param h Finite-difference step in theta.
#' @return List with `estimate` (I), `se_one_obs`, `theta`, `h`,
#'   `grid_points`, `method`.
#' @examples
#' round(morie_wasserman_fisher_info(NULL, 2)$estimate, 4)
#' @export
morie_wasserman_fisher_info <- function(f, theta, x_grid = NULL, h = 1e-5) {
  theta <- as.numeric(theta)[1]; h <- as.numeric(h)[1]
  if (is.null(f)) {
    .morie_wsm_need(theta > 0,
                    sprintf("the exponential model needs theta > 0; got %s.", theta))
    f <- function(x, th) ifelse(x >= 0, exp(-x / th) / th, 0)
    if (is.null(x_grid)) x_grid <- seq(0, 40 * theta, length.out = 200001L)
  }
  .morie_wsm_need(!is.null(x_grid),
                  "a custom density needs an explicit x_grid for the expectation.")
  x <- as.numeric(x_grid)
  lp <- suppressWarnings(log(as.numeric(f(x, theta + h))))
  l0 <- suppressWarnings(log(as.numeric(f(x, theta))))
  lm <- suppressWarnings(log(as.numeric(f(x, theta - h))))
  d2 <- (lp - 2 * l0 + lm) / h^2
  w <- as.numeric(f(x, theta))
  good <- is.finite(d2) & is.finite(w) & w > 0
  xg <- x[good]; integ <- -d2[good] * w[good]
  dx <- diff(xg)
  info <- sum(0.5 * dx * (integ[-1] + integ[-length(integ)]))
  .morie_wsm_need(info > 0, sprintf(
    "numeric information came out non-positive (%s); check the model/grid.", info))
  list(estimate = info, se_one_obs = info^-0.5, theta = theta, h = h,
       grid_points = length(x),
       method = "I = -int f(x;th) d2 log f/dth2 dx (central diff + trapezoid)")
}

#' MLE asymptotic standard error and Wald interval (Ch 9 Thm 9.18, wsmasm)
#'
#' @param data Numeric sample; only its size enters the se.
#' @param f Density f(x, theta); NULL = exponential model.
#' @param theta_hat The MLE.
#' @param x_grid Support grid for a custom f.
#' @return List with `estimate` (theta_hat), `se`, `information`,
#'   `ci_lower`, `ci_upper`, `n`, `method`.
#' @examples
#' round(morie_wasserman_mle_asymptotic(1:100, NULL, 2)$se, 4)
#' @export
morie_wasserman_mle_asymptotic <- function(data, f, theta_hat, x_grid = NULL) {
  data <- as.numeric(data); n <- length(data)
  .morie_wsm_need(n > 0, "asymptotics for an empty sample are undefined.")
  theta_hat <- as.numeric(theta_hat)[1]
  info <- morie_wasserman_fisher_info(f, theta_hat, x_grid = x_grid)$estimate
  se <- 1 / sqrt(n * info)
  z <- 1.959963984540054
  list(estimate = theta_hat, se = se, information = info,
       ci_lower = theta_hat - z * se, ci_upper = theta_hat + z * se, n = n,
       method = "MLE se = 1/sqrt(n I(theta_hat)), Wald 95 CI")
}

# --- sandwich, EM, chi-square -----------------------------------------

#' White-Huber sandwich covariance (Ch 9, wsmwhz)
#'
#' V = A^-1 B A^-1 / n with A = X'X/n and B = X' diag(e^2) X / n, the
#' HC0 form. Matrices come back ROW-MAJOR to match the Python payload.
#'
#' @param X Design matrix (n x p), n > p; add your own intercept.
#' @param y Response of length n.
#' @param f Optional mean function f(X, beta); NULL = linear.
#' @return List with `estimate` (robust se of the first coefficient),
#'   `beta`, `robust_se`, `covariance`, `bread`, `meat`, `n`, `p`, `method`.
#' @examples
#' X <- cbind(1, rep(c(-1, 1), 4)); y <- X %*% c(1, 2) + rep(c(0.5, -0.5, -0.5, 0.5), 2)
#' round(morie_wasserman_white_huber(X, y)$beta, 6)
#' @export
morie_wasserman_white_huber <- function(X, y, f = NULL) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(n > p, sprintf("sandwich needs n > p; got n=%d, p=%d.", n, p))
  beta <- as.numeric(qr.solve(X, y))
  fitted <- if (is.null(f)) as.numeric(X %*% beta) else as.numeric(f(X, beta))
  e <- y - fitted
  A <- crossprod(X) / n
  B <- crossprod(X * e^2, X) / n
  Ainv <- solve(A)
  V <- Ainv %*% B %*% Ainv / n
  rse <- sqrt(diag(V))
  list(estimate = rse[1], beta = beta, robust_se = rse,
       covariance = as.numeric(t(V)), bread = as.numeric(t(A)),
       meat = as.numeric(t(B)), n = n, p = p,
       method = "White-Huber HC0 sandwich A^-1 B A^-1 / n")
}

#' EM for a two-component normal mixture (Ch 9, wsmemt)
#'
#' The log-likelihood is monotone non-decreasing by construction and is
#' checked every iteration; a decrease signals a numerical fault and
#' stops rather than returning a quietly wrong fit.
#'
#' @param X Numeric sample, at least 2 points.
#' @param theta0 Length-5 start (pi, mu1, mu2, sd1, sd2), 0 < pi < 1, sds > 0.
#' @param max_iter Iteration cap.
#' @param tol Log-likelihood convergence gain.
#' @return List with `estimate` (final pi), `pi`, `mu1`, `mu2`, `sd1`,
#'   `sd2`, `log_likelihood`, `iterations`, `converged`, `n`, `method`.
#' @examples
#' x <- c(0, 0.1, -0.1, 0.05, 10, 10.1, 9.9, 10.05)
#' round(morie_wasserman_em_algorithm(x, c(0.5, -1, 11, 1, 1))$mu1, 4)
#' @export
morie_wasserman_em_algorithm <- function(X, theta0, max_iter = 200L, tol = 1e-8) {
  X <- as.numeric(X); n <- length(X)
  .morie_wsm_need(n >= 2L, "EM on fewer than 2 points is undefined.")
  th <- as.numeric(theta0)
  pi_ <- th[1]; mu1 <- th[2]; mu2 <- th[3]; sd1 <- th[4]; sd2 <- th[5]
  .morie_wsm_need(pi_ > 0 && pi_ < 1, sprintf(
    "the mixing weight must lie in (0, 1); got %s.", pi_))
  .morie_wsm_need(sd1 > 0 && sd2 > 0,
                  "initial standard deviations must be positive.")
  dnorm_ <- function(x, m, s) exp(-0.5 * ((x - m) / s)^2) / (s * sqrt(2 * pi))
  ll_old <- -Inf; converged <- FALSE; it <- 0L; ll <- NA_real_
  for (it in seq_len(as.integer(max_iter))) {
    d1 <- (1 - pi_) * dnorm_(X, mu1, sd1)
    d2 <- pi_ * dnorm_(X, mu2, sd2)
    tot <- d1 + d2
    tot[tot <= 0] <- .Machine$double.xmin
    ll <- sum(log(tot))
    .morie_wsm_need(ll >= ll_old - 1e-10, sprintf(
      "EM log-likelihood decreased (%s -> %s); numerical fault.", ll_old, ll))
    gamma <- d2 / tot
    if (abs(ll - ll_old) < tol) { converged <- TRUE; break }
    ll_old <- ll
    w2 <- sum(gamma); w1 <- n - w2
    pi_ <- w2 / n
    mu1 <- sum((1 - gamma) * X) / w1
    mu2 <- sum(gamma * X) / w2
    sd1 <- max(sqrt(sum((1 - gamma) * (X - mu1)^2) / w1), 1e-12)
    sd2 <- max(sqrt(sum(gamma * (X - mu2)^2) / w2), 1e-12)
  }
  list(estimate = pi_, pi = pi_, mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2,
       log_likelihood = ll, iterations = it, converged = converged, n = n,
       method = "EM 2-component normal mixture, closed-form M-step")
}

#' Chi-square goodness-of-fit (Ch 10, wsmchi)
#'
#' @param observed Non-negative observed counts, at least 2 cells.
#' @param expected Strictly positive expected counts, same length.
#' @return List with `estimate` (X^2), `p_value`, `df`, `per_cell`,
#'   `total_observed`, `total_expected`, `k`, `method`.
#' @examples
#' morie_wasserman_chi_sq_gof(c(10, 20, 30), c(20, 20, 20))$estimate
#' @export
morie_wasserman_chi_sq_gof <- function(observed, expected) {
  obs <- as.numeric(observed); exp_ <- as.numeric(expected)
  .morie_wsm_need(length(obs) == length(exp_), sprintf(
    "observed (%d) and expected (%d) lengths differ.", length(obs), length(exp_)))
  k <- length(obs)
  .morie_wsm_need(k >= 2L, "a goodness-of-fit test needs at least 2 cells.")
  .morie_wsm_need(all(obs >= 0), "observed counts cannot be negative.")
  .morie_wsm_need(all(exp_ > 0), "expected counts must be strictly positive.")
  per <- (obs - exp_)^2 / exp_
  stat <- sum(per); df <- k - 1L
  list(estimate = stat, p_value = stats::pchisq(stat, df, lower.tail = FALSE),
       df = df, per_cell = per, total_observed = sum(obs),
       total_expected = sum(exp_), k = k,
       method = "chi-square GOF sum (O-E)^2/E vs Chi2_{k-1}")
}

# --- Bayesian grid inference ------------------------------------------

.morie_wsm_trapz <- function(y, x) {
  dx <- diff(x)
  sum(0.5 * dx * (y[-1] + y[-length(y)]))
}

#' Grid posterior p(theta | x) (Ch 11, wsmbay)
#'
#' The likelihood is accumulated in the log domain at each grid point
#' and the posterior normalised by trapezoid quadrature, so the
#' evidence p(x) is reported rather than divided away invisibly.
#'
#' @param data Numeric sample, non-empty.
#' @param f Density f(x, theta) vectorised in x; NULL = N(theta, 1).
#' @param prior List/pair (theta_grid, prior_density); grid strictly
#'   increasing with at least 2 points, density non-negative.
#' @return List with `estimate` (posterior mean), `posterior`,
#'   `theta_grid`, `evidence`, `map_theta`, `n`, `method`.
#' @examples
#' g <- seq(-5, 5, length.out = 2001)
#' round(morie_wasserman_posterior(c(1, 0.5, 1.5), NULL, list(g, rep(1, 2001)))$estimate, 6)
#' @export
morie_wasserman_posterior <- function(data, f, prior) {
  data <- as.numeric(data)
  .morie_wsm_need(length(data) > 0, "a posterior needs data.")
  grid <- as.numeric(prior[[1]]); pd <- as.numeric(prior[[2]])
  .morie_wsm_need(length(grid) == length(pd) && length(grid) >= 2L,
                  "the prior needs matching grid/density arrays with >= 2 points.")
  .morie_wsm_need(all(diff(grid) > 0), "the parameter grid must be strictly increasing.")
  .morie_wsm_need(all(pd >= 0), "a prior density cannot be negative.")
  if (is.null(f)) f <- function(x, th) exp(-0.5 * (x - th)^2) / sqrt(2 * pi)
  ll <- vapply(grid, function(th) sum(log(as.numeric(f(data, th)))), numeric(1))
  logpost <- suppressWarnings(ll + log(pd))
  m <- max(logpost[is.finite(logpost)])
  unnorm <- ifelse(is.finite(logpost), exp(logpost - m), 0)
  Z <- .morie_wsm_trapz(unnorm, grid)
  .morie_wsm_need(Z > 0, paste("the posterior normalising integral is zero;",
                               "prior and likelihood do not overlap."))
  post <- unnorm / Z
  list(estimate = .morie_wsm_trapz(grid * post, grid), posterior = post,
       theta_grid = grid, evidence = Z * exp(m),
       map_theta = grid[which.max(post)], n = length(data),
       method = "grid posterior, log-domain likelihood, trapezoid normalisation")
}

#' Equal-tail credible interval (Ch 11, wsmbcr)
#'
#' @param posterior List/pair (theta_grid, density) as from
#'   [morie_wasserman_posterior()].
#' @param alpha Level in (0, 1).
#' @return List with `estimate` (interval length), `lower`, `upper`,
#'   `mass_drift`, `alpha`, `method`.
#' @examples
#' g <- seq(0, 1, length.out = 10001)
#' round(morie_wasserman_credible_interval(list(g, rep(1, 10001)), 0.1)$lower, 4)
#' @export
morie_wasserman_credible_interval <- function(posterior, alpha) {
  grid <- as.numeric(posterior[[1]]); dens <- as.numeric(posterior[[2]])
  alpha <- as.numeric(alpha)[1]
  .morie_wsm_need(alpha > 0 && alpha < 1,
                  sprintf("alpha must lie in (0, 1); got %s.", alpha))
  .morie_wsm_need(length(grid) == length(dens) && length(grid) >= 2L,
                  "the posterior needs matching grid/density arrays with >= 2 points.")
  dx <- diff(grid)
  seg <- 0.5 * dx * (dens[-1] + dens[-length(dens)])
  total <- sum(seg)
  .morie_wsm_need(total > 0, "the posterior has zero mass.")
  cdf <- c(0, cumsum(seg)) / total
  lo <- stats::approx(cdf, grid, xout = alpha / 2)$y
  hi <- stats::approx(cdf, grid, xout = 1 - alpha / 2)$y
  list(estimate = hi - lo, lower = lo, upper = hi,
       mass_drift = abs(total - 1), alpha = alpha,
       method = "equal-tail credible interval via trapezoid CDF inversion")
}

#' Posterior mean from a grid posterior (Ch 11, wsmpst1)
#'
#' @param posterior List/pair (theta_grid, density).
#' @return List with `estimate` (posterior mean), `posterior_sd`,
#'   `mass_drift`, `method`.
#' @examples
#' g <- seq(0, 1, length.out = 10001)
#' round(morie_wasserman_posterior_mean(list(g, rep(1, 10001)))$estimate, 6)
#' @export
morie_wasserman_posterior_mean <- function(posterior) {
  grid <- as.numeric(posterior[[1]]); dens <- as.numeric(posterior[[2]])
  .morie_wsm_need(length(grid) == length(dens) && length(grid) >= 2L,
                  "the posterior needs matching grid/density arrays with >= 2 points.")
  Z <- .morie_wsm_trapz(dens, grid)
  .morie_wsm_need(Z > 0, "the posterior has zero mass.")
  m1 <- .morie_wsm_trapz(grid * dens, grid) / Z
  m2 <- .morie_wsm_trapz(grid^2 * dens, grid) / Z
  list(estimate = m1, posterior_sd = sqrt(max(m2 - m1^2, 0)),
       mass_drift = abs(Z - 1),
       method = "posterior mean by trapezoid quadrature (self-normalising)")
}

#' Savage-Dickey Bayes factor (Verdinelli & Wasserman 1995, bfsd)
#'
#' BF_01 = p(theta_0 | y) / p(theta_0), the posterior density at the
#' null over the prior density there. The posterior density comes from
#' a Gaussian KDE with Silverman's bandwidth unless one is supplied.
#'
#' @param samples Posterior draws, at least 10.
#' @param prior Prior density function, or its value at theta0 (> 0).
#' @param theta0 The null value.
#' @param bandwidth Optional KDE bandwidth override (> 0).
#' @return List with `estimate` (BF_01), `bf10`,
#'   `posterior_density_at_null`, `prior_density_at_null`, `bandwidth`,
#'   `n`, `method`.
#' @examples
#' d <- seq(-1, 1, length.out = 2001)
#' round(morie_bayes_factor_savage_dickey(d, 1 / 6, 0)$estimate, 3)
#' @export
morie_bayes_factor_savage_dickey <- function(samples, prior, theta0 = 0,
                                             bandwidth = NULL) {
  samples <- as.numeric(samples); n <- length(samples)
  .morie_wsm_need(n >= 10L,
                  "the Savage-Dickey KDE needs at least 10 posterior draws.")
  theta0 <- as.numeric(theta0)[1]
  p0 <- if (is.function(prior)) as.numeric(prior(theta0)) else as.numeric(prior)[1]
  .morie_wsm_need(p0 > 0, sprintf(
    "the prior density at theta0 must be positive; got %s.", p0))
  if (is.null(bandwidth)) {
    s <- stats::sd(samples)
    q <- stats::quantile(samples, c(0.25, 0.75), type = 7, names = FALSE)
    spread <- if (q[2] > q[1]) min(s, (q[2] - q[1]) / 1.34) else s
    .morie_wsm_need(spread > 0, "degenerate posterior draws; KDE bandwidth is zero.")
    bandwidth <- 0.9 * spread * n^(-0.2)
  }
  bandwidth <- as.numeric(bandwidth)[1]
  .morie_wsm_need(bandwidth > 0, sprintf(
    "the KDE bandwidth must be positive; got %s.", bandwidth))
  z <- (theta0 - samples) / bandwidth
  post0 <- mean(exp(-0.5 * z^2)) / (bandwidth * sqrt(2 * pi))
  bf01 <- post0 / p0
  list(estimate = bf01, bf10 = if (bf01 > 0) 1 / bf01 else Inf,
       posterior_density_at_null = post0, prior_density_at_null = p0,
       bandwidth = bandwidth, n = n,
       method = "Savage-Dickey BF01 = KDE posterior(theta0) / prior(theta0)")
}

# --- information theory, epidemiology, networks ------------------------

#' Entropy in nats (Ch 23, wsment)
#'
#' Discrete when `x_grid` is absent (a probability vector summing to 1),
#' differential when it is supplied. 0 log 0 = 0 by convention.
#'
#' @param p Probability vector, or density values on `x_grid`.
#' @param x_grid Optional support grid; its presence selects the
#'   differential form.
#' @return List with `estimate` (nats), `bits`, `form`, `n`, `method`.
#' @examples
#' round(morie_wasserman_entropy(c(0.5, 0.5))$bits, 12)
#' @export
morie_wasserman_entropy <- function(p, x_grid = NULL) {
  p <- as.numeric(p)
  .morie_wsm_need(all(p >= 0), "probabilities/densities cannot be negative.")
  if (is.null(x_grid)) {
    s <- sum(p)
    .morie_wsm_need(abs(s - 1) <= 1e-8,
                    sprintf("a probability vector must sum to 1; got %s.", s))
    nz <- p[p > 0]
    H <- -sum(nz * log(nz)) + 0
    form <- "discrete"; n <- length(p)
  } else {
    x <- as.numeric(x_grid)
    .morie_wsm_need(length(x) == length(p) && length(x) >= 2L,
                    "density and grid must match with >= 2 points.")
    integ <- ifelse(p > 0, -p * log(ifelse(p > 0, p, 1)), 0)
    H <- .morie_wsm_trapz(integ, x)
    form <- "differential"; n <- length(x)
  }
  list(estimate = H, bits = H / log(2), form = form, n = n,
       method = sprintf("%s entropy, nats, 0 log 0 = 0", form))
}

#' Kullback-Leibler divergence (Ch 23, wsmkbk)
#'
#' D(p||q) in nats. Terms with p_i = 0 contribute 0; any point with
#' p_i > 0 and q_i = 0 makes D infinite -- reported, not signalled.
#'
#' @param p,q Probability vectors (each summing to 1), or densities on
#'   `x_grid`.
#' @param x_grid Optional support grid for the continuous form.
#' @return List with `estimate` (nats), `bits`, `reverse` (D(q||p)),
#'   `form`, `n`, `method`.
#' @examples
#' round(morie_wasserman_kullback_leibler(c(0.5, 0.5), c(0.25, 0.75))$estimate, 12)
#' @export
morie_wasserman_kullback_leibler <- function(p, q, x_grid = NULL) {
  p <- as.numeric(p); q <- as.numeric(q)
  .morie_wsm_need(length(p) == length(q),
                  sprintf("p (%d) and q (%d) lengths differ.", length(p), length(q)))
  .morie_wsm_need(all(p >= 0) && all(q >= 0),
                  "probabilities/densities cannot be negative.")
  kl <- function(a, b) {
    if (any(a > 0 & b == 0)) return(Inf)
    mask <- a > 0
    if (is.null(x_grid)) return(sum(a[mask] * log(a[mask] / b[mask])))
    x <- as.numeric(x_grid)
    full <- numeric(length(a))
    full[mask] <- a[mask] * log(a[mask] / b[mask])
    .morie_wsm_trapz(full, x)
  }
  if (is.null(x_grid)) {
    for (nm in c("p", "q")) {
      v <- if (nm == "p") p else q
      .morie_wsm_need(abs(sum(v) - 1) <= 1e-8,
                      sprintf("%s must sum to 1; got %s.", nm, sum(v)))
    }
    form <- "discrete"; n <- length(p)
  } else {
    form <- "continuous"; n <- length(as.numeric(x_grid))
  }
  D <- kl(p, q)
  list(estimate = D, bits = if (is.finite(D)) D / log(2) else Inf,
       reverse = kl(q, p), form = form, n = n,
       method = "KL D(p||q); 0 log 0 = 0, p>0 & q=0 -> inf")
}

#' Mutual information of two discrete samples (Ch 23, wsmmtl)
#'
#' @param x,y Paired discrete observations of equal length.
#' @return List with `estimate` (nats), `bits`, `levels_x`, `levels_y`,
#'   `n`, `method`.
#' @examples
#' round(morie_wasserman_mutual_info(c(0, 0, 1, 1), c(0, 0, 1, 1))$bits, 12)
#' @export
morie_wasserman_mutual_info <- function(x, y) {
  .morie_wsm_need(length(x) == length(y), sprintf(
    "paired samples must have equal length; got %d and %d.", length(x), length(y)))
  n <- length(x)
  .morie_wsm_need(n > 0, "mutual information of an empty sample is undefined.")
  lx <- sort(unique(as.character(x))); ly <- sort(unique(as.character(y)))
  joint <- matrix(0, length(lx), length(ly))
  for (i in seq_len(n)) {
    joint[match(as.character(x[i]), lx), match(as.character(y[i]), ly)] <-
      joint[match(as.character(x[i]), lx), match(as.character(y[i]), ly)] + 1
  }
  joint <- joint / n
  prod_m <- outer(rowSums(joint), colSums(joint))
  kl <- morie_wasserman_kullback_leibler(as.numeric(t(joint)), as.numeric(t(prod_m)))
  list(estimate = kl$estimate, bits = kl$bits, levels_x = length(lx),
       levels_y = length(ly), n = n,
       method = "I(X;Y) = KL(joint || product of marginals), empirical")
}

#' Odds ratio with Woolf interval (Ch 16, wsmodd)
#'
#' Table layout \\[\\[n11, n10\\], \\[n01, n00\\]\\]: row = exposure, column =
#' outcome. A zero cell is refused; a continuity correction is the
#' caller's explicit decision, never a silent default.
#'
#' @param table 2x2 matrix of strictly positive counts.
#' @return List with `estimate` (OR), `log_or`, `se`, `ci_lower`,
#'   `ci_upper`, `n`, `method`.
#' @examples
#' morie_wasserman_odds_ratio(rbind(c(30, 10), c(15, 45)))$estimate
#' @export
morie_wasserman_odds_ratio <- function(table) {
  T <- as.matrix(table)
  .morie_wsm_need(all(dim(T) == c(2L, 2L)),
                  sprintf("the table must be 2x2; got %dx%d.", nrow(T), ncol(T)))
  .morie_wsm_need(all(T >= 0), "counts cannot be negative.")
  .morie_wsm_need(all(T > 0), paste("a zero cell makes the odds ratio degenerate;",
                                    "apply a continuity correction explicitly if intended."))
  or_ <- (T[1, 1] * T[2, 2]) / (T[1, 2] * T[2, 1])
  log_or <- log(or_); se <- sqrt(sum(1 / T))
  z <- 1.959963984540054
  list(estimate = or_, log_or = log_or, se = se,
       ci_lower = exp(log_or - z * se), ci_upper = exp(log_or + z * se),
       n = sum(T), method = "OR = n11 n00 / (n10 n01), Woolf log-scale CI")
}

#' Relative risk with Katz interval (Ch 16, wsmrrr)
#'
#' @param table 2x2 matrix \\[\\[n11, n10\\], \\[n01, n00\\]\\] with positive rows
#'   and event counts.
#' @return List with `estimate` (RR), `risk_exposed`, `risk_unexposed`,
#'   `log_rr`, `se`, `ci_lower`, `ci_upper`, `n`, `method`.
#' @examples
#' round(morie_wasserman_relative_risk(rbind(c(30, 70), c(10, 90)))$estimate, 12)
#' @export
morie_wasserman_relative_risk <- function(table) {
  T <- as.matrix(table)
  .morie_wsm_need(all(dim(T) == c(2L, 2L)),
                  sprintf("the table must be 2x2; got %dx%d.", nrow(T), ncol(T)))
  .morie_wsm_need(all(T >= 0), "counts cannot be negative.")
  r1 <- T[1, 1] + T[1, 2]; r0 <- T[2, 1] + T[2, 2]
  .morie_wsm_need(r1 > 0 && r0 > 0, "both exposure rows need at least one subject.")
  .morie_wsm_need(T[1, 1] > 0 && T[2, 1] > 0,
                  "zero event counts make the relative risk degenerate.")
  p1 <- T[1, 1] / r1; p0 <- T[2, 1] / r0
  rr <- p1 / p0; log_rr <- log(rr)
  se <- sqrt((1 - p1) / T[1, 1] + (1 - p0) / T[2, 1])
  z <- 1.959963984540054
  list(estimate = rr, risk_exposed = p1, risk_unexposed = p0, log_rr = log_rr,
       se = se, ci_lower = exp(log_rr - z * se), ci_upper = exp(log_rr + z * se),
       n = sum(T), method = "RR = p_exposed/p_unexposed, Katz log-scale CI")
}

#' Density of an undirected simple graph (Wasserman & Faust Ch 4, densty)
#'
#' @param G Binary symmetric hollow adjacency matrix, at least 2 nodes.
#' @return List with `estimate` (density), `n_edges`, `n_possible`,
#'   `n`, `method`.
#' @examples
#' morie_density(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))$n_edges
#' @export
morie_density <- function(G) {
  A <- as.matrix(G)
  .morie_wsm_need(nrow(A) == ncol(A), sprintf(
    "the adjacency matrix must be square; got %dx%d.", nrow(A), ncol(A)))
  n <- nrow(A)
  .morie_wsm_need(n >= 2L, "density needs at least 2 vertices.")
  .morie_wsm_need(identical(as.numeric(A), as.numeric(t(A))),
                  "the adjacency matrix must be symmetric (undirected graph).")
  .morie_wsm_need(all(diag(A) == 0), "self-loops are not allowed (nonzero diagonal).")
  .morie_wsm_need(all(A %in% c(0, 1)), "the adjacency matrix must be binary.")
  edges <- sum(A) %/% 2
  possible <- n * (n - 1) %/% 2
  list(estimate = edges / possible, n_edges = edges, n_possible = possible,
       n = n, method = "density |E| / C(n,2), undirected simple graph")
}

#' Closeness centrality (Wasserman & Faust Ch 5.3.2, sgtclo)
#'
#' C(v) = (n - 1) / sum_u d(v, u) with d the unweighted shortest-path
#' distance by breadth-first search. A disconnected graph makes the
#' Wasserman-Faust form undefined and is refused rather than silently
#' switched to a harmonic variant.
#'
#' @param A Binary symmetric adjacency matrix of a connected graph.
#' @return List with `estimate` (max closeness), `closeness`,
#'   `argmax` (0-based), `n`, `method`.
#' @examples
#' morie_sgt_closeness_centrality(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))$closeness
#' @export
morie_sgt_closeness_centrality <- function(A) {
  A <- as.matrix(A)
  .morie_wsm_need(nrow(A) == ncol(A), sprintf(
    "the adjacency matrix must be square; got %dx%d.", nrow(A), ncol(A)))
  n <- nrow(A)
  .morie_wsm_need(n >= 2L, "closeness needs at least 2 vertices.")
  .morie_wsm_need(identical(as.numeric(A), as.numeric(t(A))),
                  "the adjacency matrix must be symmetric (undirected graph).")
  nbrs <- lapply(seq_len(n), function(i) which(A[i, ] != 0))
  clos <- numeric(n)
  for (v in seq_len(n)) {
    dist <- rep(-1L, n); dist[v] <- 0L; queue <- v
    while (length(queue)) {
      nxt <- integer(0)
      for (u in queue) for (w in nbrs[[u]]) if (dist[w] < 0L) {
        dist[w] <- dist[u] + 1L; nxt <- c(nxt, w)
      }
      queue <- nxt
    }
    far <- which(dist < 0L)
    .morie_wsm_need(length(far) == 0L, sprintf(
      "closeness needs a connected graph; vertex %d cannot reach vertex %d.",
      v - 1L, far[1] - 1L))
    clos[v] <- (n - 1) / sum(dist)
  }
  arg <- which.max(clos)
  list(estimate = clos[arg], closeness = clos, argmax = arg - 1L, n = n,
       method = "closeness (n-1)/sum BFS distances; connected required")
}

# --- regression and model selection -----------------------------------

#' Ordinary least squares with classical errors (Ch 13 Thm 13.4, wsmlsr)
#'
#' Solved by QR rather than the normal equations; a rank-deficient
#' design is refused rather than silently pseudo-inverted.
#'
#' @param X Design matrix (n x p) with n > p; add your own intercept.
#' @param y Response of length n.
#' @return List with `estimate` (first coefficient), `beta`, `se`,
#'   `sigma2`, `rss`, `r_squared`, `n`, `p`, `method`.
#' @examples
#' X <- cbind(1, c(0, 1, 2)); morie_wasserman_least_squares(X, c(1, 3, 5))$beta
#' @export
morie_wasserman_least_squares <- function(X, y) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(n > p, sprintf("OLS needs n > p; got n=%d, p=%d.", n, p))
  qrx <- qr(X)
  .morie_wsm_need(qrx$rank == p, sprintf(
    "the design matrix is rank deficient (rank %d < p = %d).", qrx$rank, p))
  beta <- as.numeric(qr.coef(qrx, y))
  resid <- y - as.numeric(X %*% beta)
  rss <- sum(resid^2); sigma2 <- rss / (n - p)
  se <- sqrt(diag(sigma2 * solve(crossprod(X))))
  tss <- sum((y - mean(y))^2)
  list(estimate = beta[1], beta = beta, se = se, sigma2 = sigma2, rss = rss,
       r_squared = if (tss > 0) 1 - rss / tss else NaN, n = n, p = p,
       method = "OLS via QR; classical se sigma2 (X'X)^-1")
}

#' Ridge regression (Ch 13, wsmrgr)
#'
#' @param X Design matrix (n x p).
#' @param y Response of length n.
#' @param lambda_ Non-negative penalty.
#' @return List with `estimate`, `beta`, `effective_df`, `rss`,
#'   `lambda`, `n`, `p`, `method`.
#' @examples
#' X <- cbind(1, c(0, 1, 2)); morie_wasserman_ridge(X, c(1, 3, 5), 0.5)$beta
#' @export
morie_wasserman_ridge <- function(X, y, lambda_) {
  X <- as.matrix(X); y <- as.numeric(y); lam <- as.numeric(lambda_)[1]
  n <- nrow(X); p <- ncol(X)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(lam >= 0,
                  sprintf("the ridge penalty must be non-negative; got %s.", lam))
  G <- crossprod(X) + lam * diag(p)
  Ginv <- tryCatch(solve(G), error = function(e)
    stop("X'X + lambda I is singular; increase lambda or fix the design.",
         call. = FALSE))
  beta <- as.numeric(Ginv %*% crossprod(X, y))
  H <- X %*% Ginv %*% t(X)
  resid <- y - as.numeric(X %*% beta)
  list(estimate = beta[1], beta = beta, effective_df = sum(diag(H)),
       rss = sum(resid^2), lambda = lam, n = n, p = p,
       method = "ridge (X'X + lambda I)^-1 X'y; edf = tr(H)")
}

#' Lasso by cyclic coordinate descent (Ch 13, wsmlas)
#'
#' Minimises (1/2)|y - X beta|^2 + lambda |beta|_1, so this lambda is
#' n times glmnet's convention -- stated because the two differ.
#'
#' @param X Design matrix with no all-zero column.
#' @param y Response.
#' @param lambda_ Non-negative penalty.
#' @param max_iter,tol Coordinate-descent controls.
#' @return List with `estimate`, `beta`, `n_nonzero`, `objective`,
#'   `iterations`, `converged`, `lambda`, `n`, `p`, `method`.
#' @examples
#' morie_wasserman_lasso(diag(2), c(3, -1), 0.5)$beta
#' @export
morie_wasserman_lasso <- function(X, y, lambda_, max_iter = 10000L, tol = 1e-12) {
  X <- as.matrix(X); y <- as.numeric(y); lam <- as.numeric(lambda_)[1]
  n <- nrow(X); p <- ncol(X)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(lam >= 0,
                  sprintf("the lasso penalty must be non-negative; got %s.", lam))
  colsq <- colSums(X^2)
  .morie_wsm_need(all(colsq > 0),
                  "an all-zero column cannot be penalised meaningfully.")
  beta <- numeric(p); r <- y; converged <- FALSE; it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    delta <- 0
    for (j in seq_len(p)) {
      old <- beta[j]
      rho <- sum(X[, j] * r) + colsq[j] * old
      new <- sign(rho) * max(abs(rho) - lam, 0) / colsq[j]
      if (new != old) {
        r <- r + X[, j] * (old - new); beta[j] <- new
        delta <- max(delta, abs(new - old))
      }
    }
    if (delta < tol) { converged <- TRUE; break }
  }
  list(estimate = beta[1], beta = beta, n_nonzero = sum(beta != 0),
       objective = 0.5 * sum(r^2) + lam * sum(abs(beta)),
       iterations = it, converged = converged, lambda = lam, n = n, p = p,
       method = "lasso cyclic coordinate descent, soft threshold")
}

#' Logistic regression MLE by Newton-Raphson (Ch 13.7, wsmlgr)
#'
#' Perfect separation drives the MLE to infinity; it is detected via
#' exploding coefficients and refused with that diagnosis rather than
#' returning a meaningless fit.
#'
#' @param X Design matrix; add your own intercept.
#' @param y Binary response in {0, 1}.
#' @param max_iter,tol Newton controls.
#' @return List with `estimate`, `beta`, `se`, `log_likelihood`,
#'   `iterations`, `converged`, `n`, `p`, `method`.
#' @examples
#' round(morie_wasserman_logistic_regression(matrix(1, 4, 1), c(1, 1, 1, 0))$beta, 6)
#' @export
morie_wasserman_logistic_regression <- function(X, y, max_iter = 100L, tol = 1e-10) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(all(y %in% c(0, 1)), "the response must be binary 0/1.")
  sep <- "perfect separation: the MLE is infinite; regularise or change the model."
  beta <- numeric(p); converged <- FALSE; it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    mu <- 1 / (1 + exp(-as.numeric(X %*% beta)))
    Wv <- mu * (1 - mu)
    H <- crossprod(X * Wv, X)
    step <- tryCatch(as.numeric(solve(H, crossprod(X, y - mu))),
                     error = function(e) stop(sep, call. = FALSE))
    beta <- beta + step
    .morie_wsm_need(max(abs(beta)) <= 30, sep)
    if (max(abs(step)) < tol) { converged <- TRUE; break }
  }
  mu <- 1 / (1 + exp(-as.numeric(X %*% beta)))
  ll <- sum(y * log(mu) + (1 - y) * log(1 - mu))
  se <- sqrt(diag(solve(crossprod(X * (mu * (1 - mu)), X))))
  list(estimate = beta[1], beta = beta, se = se, log_likelihood = ll,
       iterations = it, converged = converged, n = n, p = p,
       method = "logistic MLE by Newton-Raphson; separation refused")
}

#' Poisson log-linear regression MLE (Ch 13, wsmpsr)
#'
#' The log-likelihood INCLUDES the -sum log(y_i!) constant, so values
#' are comparable across models.
#'
#' @param X Design matrix.
#' @param y Non-negative integer counts.
#' @param max_iter,tol Newton controls.
#' @return List with `estimate`, `beta`, `se`, `log_likelihood`,
#'   `deviance`, `iterations`, `converged`, `n`, `p`, `method`.
#' @examples
#' round(morie_wasserman_poisson_regression(matrix(1, 4, 1), c(1, 2, 3, 2))$beta, 6)
#' @export
morie_wasserman_poisson_regression <- function(X, y, max_iter = 100L, tol = 1e-10) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(all(y >= 0) && all(y == round(y)),
                  "Poisson counts must be non-negative integers.")
  beta <- numeric(p)
  beta[1] <- if (mean(y) > 0) log(mean(y)) else 0
  converged <- FALSE; it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    mu <- exp(pmin(pmax(as.numeric(X %*% beta), -30), 30))
    step <- tryCatch(as.numeric(solve(crossprod(X * mu, X), crossprod(X, y - mu))),
                     error = function(e)
                       stop("the information matrix is singular; check the design.",
                            call. = FALSE))
    beta <- beta + step
    if (max(abs(step)) < tol) { converged <- TRUE; break }
  }
  mu <- exp(as.numeric(X %*% beta))
  ll <- sum(y * log(mu) - mu) - sum(lgamma(y + 1))
  dev_terms <- ifelse(y > 0, y * log(y / mu), 0) - (y - mu)
  se <- sqrt(diag(solve(crossprod(X * mu, X))))
  list(estimate = beta[1], beta = beta, se = se, log_likelihood = ll,
       deviance = 2 * sum(dev_terms), iterations = it, converged = converged,
       n = n, p = p, method = "Poisson GLM Newton; ll includes lgamma constant")
}

#' AIC of a fitted model (Ch 13.6, wsmaic)
#'
#' @param loglik Maximised log-likelihood.
#' @param k Number of estimated parameters.
#' @return List with `estimate` (classical AIC, lower better),
#'   `aic_wasserman` (Ch 13's log L - k form, higher better),
#'   `loglik`, `k`, `method`.
#' @examples
#' morie_wasserman_aic(-100, 3)$estimate
#' @export
morie_wasserman_aic <- function(loglik, k) {
  loglik <- as.numeric(loglik)[1]; k <- as.integer(k)
  .morie_wsm_need(k >= 0L,
                  sprintf("the parameter count cannot be negative; got %d.", k))
  list(estimate = -2 * loglik + 2 * k, aic_wasserman = loglik - k,
       loglik = loglik, k = k,
       method = "AIC = -2 log L + 2k (classical); Wasserman form alongside")
}

#' BIC of a fitted model (Ch 13.6, wsmbic)
#'
#' @param loglik Maximised log-likelihood.
#' @param k Number of estimated parameters.
#' @param n Sample size, at least 2.
#' @return List with `estimate` (classical BIC), `bic_wasserman`,
#'   `loglik`, `k`, `n`, `method`.
#' @examples
#' round(morie_wasserman_bic(-100, 3, 50)$estimate, 6)
#' @export
morie_wasserman_bic <- function(loglik, k, n) {
  loglik <- as.numeric(loglik)[1]; k <- as.integer(k); n <- as.integer(n)
  .morie_wsm_need(k >= 0L,
                  sprintf("the parameter count cannot be negative; got %d.", k))
  .morie_wsm_need(n >= 2L, sprintf("BIC needs n >= 2; got %d.", n))
  list(estimate = -2 * loglik + k * log(n),
       bic_wasserman = loglik - 0.5 * k * log(n),
       loglik = loglik, k = k, n = n,
       method = "BIC = -2 log L + k log n; Wasserman form alongside")
}

#' K-fold cross-validated squared error (Ch 13.6, wsmcvr)
#'
#' Folds are CONTIGUOUS blocks in the given order -- deterministic, no
#' hidden RNG; shuffle beforehand if the order carries information.
#'
#' @param X Design matrix.
#' @param y Response.
#' @param model Fit-and-predict function (Xtr, ytr, Xte); NULL = OLS.
#' @param k Number of folds, 2 <= k <= n.
#' @return List with `estimate` (CV mean squared error), `fold_mse`,
#'   `fold_sizes`, `k`, `n`, `method`.
#' @examples
#' X <- cbind(1, 0:7); round(morie_wasserman_kfold_cv(X, 1 + 2 * (0:7), NULL, 4)$estimate, 10)
#' @export
morie_wasserman_kfold_cv <- function(X, y, model, k) {
  X <- as.matrix(X); y <- as.numeric(y); n <- nrow(X); k <- as.integer(k)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d entries.", n, length(y)))
  .morie_wsm_need(k >= 2L && k <= n, sprintf(
    "cross-validation needs 2 <= k <= n; got k=%d, n=%d.", k, n))
  if (is.null(model)) {
    model <- function(Xtr, ytr, Xte) as.numeric(Xte %*% qr.solve(Xtr, ytr))
  }
  bounds <- as.integer(round(seq(0, n, length.out = k + 1L)))
  fold_mse <- numeric(k); fold_sizes <- integer(k); sq_sum <- 0
  for (f in seq_len(k)) {
    lo <- bounds[f]; hi <- bounds[f + 1L]
    te <- seq.int(lo + 1L, hi); tr <- setdiff(seq_len(n), te)
    pred <- as.numeric(model(X[tr, , drop = FALSE], y[tr], X[te, , drop = FALSE]))
    sq <- (y[te] - pred)^2
    fold_mse[f] <- mean(sq); fold_sizes[f] <- length(te); sq_sum <- sq_sum + sum(sq)
  }
  list(estimate = sq_sum / n, fold_mse = fold_mse, fold_sizes = fold_sizes,
       k = k, n = n,
       method = "k-fold CV, contiguous deterministic folds, squared error")
}

# --- smoothing and multivariate ---------------------------------------

#' Nadaraya-Watson kernel regression (Ch 20 Def 20.21, wsmcrk)
#'
#' An evaluation point whose kernel weights all underflow to zero is
#' reported as NaN rather than fabricated.
#'
#' @param x Evaluation point(s).
#' @param x_data,y_data Paired training sample of equal length.
#' @param h Positive bandwidth.
#' @return List with `estimate`, `values`, `effective_n`, `h`, `n`, `method`.
#' @examples
#' round(morie_wasserman_kernel_regression(0, c(-1, 1), c(2, 4), 1e6)$estimate, 6)
#' @export
morie_wasserman_kernel_regression <- function(x, x_data, y_data, h) {
  x <- as.numeric(x); xd <- as.numeric(x_data); yd <- as.numeric(y_data)
  h <- as.numeric(h)[1]
  .morie_wsm_need(length(xd) == length(yd), sprintf(
    "x_data (%d) and y_data (%d) lengths differ.", length(xd), length(yd)))
  .morie_wsm_need(length(xd) > 0, "kernel regression needs data.")
  .morie_wsm_need(h > 0, sprintf("the bandwidth must be positive; got %s.", h))
  eff <- numeric(length(x)); vals <- numeric(length(x))
  for (i in seq_along(x)) {
    w <- exp(-0.5 * ((x[i] - xd) / h)^2)
    s <- sum(w); eff[i] <- s
    vals[i] <- if (s > 0) sum(w * yd) / s else NaN
  }
  list(estimate = vals[1], values = vals, effective_n = eff, h = h,
       n = length(xd),
       method = "Nadaraya-Watson, Gaussian kernel; zero-weight -> nan")
}

#' Local polynomial regression (Ch 20, wsmlpr)
#'
#' Degree 0 recovers Nadaraya-Watson exactly; degree 1 is the local
#' linear smoother, whose b_1 estimates the derivative.
#'
#' @param x Evaluation point(s).
#' @param x_data,y_data Paired training sample.
#' @param h Positive bandwidth.
#' @param p Polynomial degree (>= 0) with n > p.
#' @return List with `estimate`, `values`, `derivatives`, `h`, `p`,
#'   `n`, `method`.
#' @examples
#' round(morie_wasserman_local_polynomial(1.5, 0:3, c(1, 3, 5, 7), 0.7, 1)$estimate, 8)
#' @export
morie_wasserman_local_polynomial <- function(x, x_data, y_data, h, p = 1L) {
  x <- as.numeric(x); xd <- as.numeric(x_data); yd <- as.numeric(y_data)
  h <- as.numeric(h)[1]; p <- as.integer(p)
  .morie_wsm_need(length(xd) == length(yd), sprintf(
    "x_data (%d) and y_data (%d) lengths differ.", length(xd), length(yd)))
  .morie_wsm_need(h > 0, sprintf("the bandwidth must be positive; got %s.", h))
  .morie_wsm_need(p >= 0L, sprintf("the degree must be >= 0; got %d.", p))
  .morie_wsm_need(length(xd) > p, sprintf(
    "local degree-%d fitting needs n > p; got n=%d.", p, length(xd)))
  vals <- numeric(length(x)); ders <- numeric(length(x))
  for (i in seq_along(x)) {
    w <- exp(-0.5 * ((x[i] - xd) / h)^2)
    D <- outer(xd - x[i], 0:p, `^`)
    sw <- sqrt(w)
    qrx <- qr(D * sw)
    if (qrx$rank < p + 1L) { vals[i] <- NaN; ders[i] <- NaN; next }
    b <- as.numeric(qr.coef(qrx, yd * sw))
    vals[i] <- b[1]
    ders[i] <- if (p >= 1L) b[2] else NaN
  }
  list(estimate = vals[1], values = vals, derivatives = ders, h = h, p = p,
       n = length(xd),
       method = "local polynomial WLS, Gaussian kernel; b0 = fit, b1 = slope")
}

#' Discrete smoothing spline (Ch 20.5, wsmsmp)
#'
#' m_hat = (I + lambda D'D)^-1 y with D the second-difference operator
#' scaled by the (possibly uneven) spacings: the Whittaker-Henderson
#' estimator whose limits are interpolation (lambda -> 0) and the OLS
#' LINE (lambda -> inf, since D annihilates linear trends).
#'
#' @param x Strictly increasing design points, at least 3.
#' @param y Responses of the same length.
#' @param lambda_ Non-negative roughness penalty.
#' @return List with `estimate` (fitted values), `effective_df`, `rss`,
#'   `lambda`, `n`, `method`.
#' @examples
#' round(morie_wasserman_smoothing_spline(0:3, c(0, 2, 0, 2), 1)$effective_df, 6)
#' @export
morie_wasserman_smoothing_spline <- function(x, y, lambda_) {
  x <- as.numeric(x); y <- as.numeric(y); lam <- as.numeric(lambda_)[1]
  n <- length(x)
  .morie_wsm_need(length(y) == n,
                  sprintf("x (%d) and y (%d) lengths differ.", n, length(y)))
  .morie_wsm_need(n >= 3L, "a smoothing spline needs at least 3 points.")
  .morie_wsm_need(all(diff(x) > 0), "the design points must be strictly increasing.")
  .morie_wsm_need(lam >= 0, sprintf("the penalty must be non-negative; got %s.", lam))
  D <- matrix(0, n - 2L, n)
  for (i in seq_len(n - 2L)) {
    h1 <- x[i + 1L] - x[i]; h2 <- x[i + 2L] - x[i + 1L]
    D[i, i] <- 2 / (h1 * (h1 + h2))
    D[i, i + 1L] <- -2 / (h1 * h2)
    D[i, i + 2L] <- 2 / (h2 * (h1 + h2))
  }
  S <- solve(diag(n) + lam * crossprod(D))
  fit <- as.numeric(S %*% y)
  list(estimate = fit, effective_df = sum(diag(S)), rss = sum((y - fit)^2),
       lambda = lam, n = n,
       method = "discrete smoothing spline (I + lam D'D)^-1 y, uneven spacings")
}

#' Principal component analysis (Ch 14, wsmpca)
#'
#' Eigenvectors carry a sign freedom; each component's
#' largest-magnitude coordinate is made positive so results are
#' identical across LAPACK builds. Components come back ROW-MAJOR.
#'
#' @param X Data matrix (n x d) with n >= 2.
#' @param k Components to keep, 1 <= k <= d.
#' @return List with `estimate` (first eigenvalue), `eigenvalues`,
#'   `components`, `explained_ratio`, `scores`, `n`, `d`, `k`, `method`.
#' @examples
#' round(morie_wasserman_pca(rbind(c(-1, -1), c(0, 0), c(1, 1)), 1)$estimate, 12)
#' @export
morie_wasserman_pca <- function(X, k) {
  X <- as.matrix(X); n <- nrow(X); d <- ncol(X); k <- as.integer(k)
  .morie_wsm_need(n >= 2L, "PCA needs at least 2 observations.")
  .morie_wsm_need(k >= 1L && k <= d,
                  sprintf("k must lie in [1, d]; got k=%d, d=%d.", k, d))
  Xc <- sweep(X, 2, colMeans(X))
  S <- crossprod(Xc) / (n - 1)
  eg <- eigen(S, symmetric = TRUE)
  vals <- eg$values[seq_len(k)]
  vecs <- eg$vectors[, seq_len(k), drop = FALSE]
  for (j in seq_len(k)) {
    i <- which.max(abs(vecs[, j]))
    if (vecs[i, j] < 0) vecs[, j] <- -vecs[, j]
  }
  scores <- Xc %*% vecs
  total <- sum(diag(S))
  list(estimate = vals[1], eigenvalues = vals,
       components = as.numeric(t(vecs)),
       explained_ratio = if (total > 0) sum(vals) / total else NaN,
       scores = as.numeric(t(scores)), n = n, d = d, k = k,
       method = "eigh of (n-1)-covariance; sign fixed by max-|coord| positive")
}

#' K-means by Lloyd's algorithm (Ch 19, wsmkmn)
#'
#' Seeded by farthest-first traversal from the point nearest the grand
#' mean -- fully deterministic, no RNG. Centres come back ROW-MAJOR and
#' labels are 0-based to match the Python payload.
#'
#' @param X Data matrix (n x d) with n >= k.
#' @param k Number of clusters, at least 1.
#' @param max_iter Lloyd iteration cap.
#' @return List with `estimate` (within-cluster sum of squares),
#'   `centers`, `labels`, `iterations`, `converged`, `n`, `d`, `k`, `method`.
#' @examples
#' morie_wasserman_kmeans(matrix(c(0, 0.2, -0.2, 10, 10.2, 9.8), ncol = 1), 2)$labels
#' @export
morie_wasserman_kmeans <- function(X, k, max_iter = 300L) {
  X <- as.matrix(X); n <- nrow(X); d <- ncol(X); k <- as.integer(k)
  .morie_wsm_need(k >= 1L, sprintf("k must be >= 1; got %d.", k))
  .morie_wsm_need(n >= k, sprintf("k-means needs n >= k; got n=%d, k=%d.", n, k))
  grand <- colMeans(X)
  first <- which.min(rowSums(sweep(X, 2, grand)^2))
  centre_idx <- first
  while (length(centre_idx) < k) {
    dmin <- apply(vapply(centre_idx,
                         function(i) rowSums(sweep(X, 2, X[i, ])^2), numeric(n)),
                  1, min)
    centre_idx <- c(centre_idx, which.max(dmin))
  }
  C <- X[centre_idx, , drop = FALSE]
  labels <- rep(-1L, n); converged <- FALSE; it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    dist <- vapply(seq_len(k), function(j) rowSums(sweep(X, 2, C[j, ])^2), numeric(n))
    if (is.null(dim(dist))) dist <- matrix(dist, nrow = n)
    new <- max.col(-dist, ties.method = "first")
    if (identical(new, labels)) { converged <- TRUE; break }
    labels <- new
    for (j in seq_len(k)) {
      members <- X[labels == j, , drop = FALSE]
      if (nrow(members)) C[j, ] <- colMeans(members)
    }
  }
  wcss <- sum((X - C[labels, , drop = FALSE])^2)
  list(estimate = wcss, centers = as.numeric(t(C)), labels = labels - 1L,
       iterations = it, converged = converged, n = n, d = d, k = k,
       method = "Lloyd k-means, farthest-first deterministic seeding")
}

#' Two-component Gaussian mixture via EM (Ch 19, wsmgmm)
#'
#' Delegates the optimisation to [morie_wasserman_em_algorithm()] after
#' a deterministic quartile-and-pooled-sd initialisation. Only k = 2 is
#' implemented; a larger k raises rather than fitting something else.
#'
#' @param X Numeric sample, at least 4 points.
#' @param k Number of components; must be 2.
#' @return List with `estimate` (log-likelihood), `weights`, `means`,
#'   `sds`, `iterations`, `converged`, `n`, `k`, `method`.
#' @examples
#' x <- c(0, 0.1, -0.1, 0.05, 10, 10.1, 9.9, 10.05)
#' round(morie_wasserman_gmm_em(x, 2)$weights, 6)
#' @export
morie_wasserman_gmm_em <- function(X, k) {
  X <- as.numeric(X); n <- length(X)
  .morie_wsm_need(as.integer(k) == 2L, sprintf(
    "only the 2-component mixture is implemented; got k=%d.", as.integer(k)))
  .morie_wsm_need(n >= 4L, "a 2-component mixture needs at least 4 points.")
  xs <- sort(X)
  q1 <- xs[ceiling(0.25 * n)]; q3 <- xs[ceiling(0.75 * n)]
  s <- stats::sd(X)
  .morie_wsm_need(s > 0, "a constant sample cannot support a mixture fit.")
  core <- morie_wasserman_em_algorithm(X, c(0.5, q1, q3, s, s))
  list(estimate = core$log_likelihood, weights = c(1 - core$pi, core$pi),
       means = c(core$mu1, core$mu2), sds = c(core$sd1, core$sd2),
       iterations = core$iterations, converged = core$converged, n = n, k = 2L,
       method = "GMM k=2 via EM core; quartile+pooled-sd deterministic init")
}

# --- HMMs, MCMC, graphical models, learners ---------------------------

#' HMM forward algorithm with per-step scaling (Rabiner 1989; Wasserman Ch 23 covers the underlying Markov chains) (wsmhmm)
#'
#' Each step is normalised and the log of the scale factors accumulates
#' the exact log-likelihood, so long sequences cannot underflow.
#'
#' @param obs 0-based observation indices, at least one.
#' @param A Transition matrix (S x S), rows summing to 1.
#' @param B Emission matrix (S x M), rows summing to 1.
#' @param pi Initial distribution of length S.
#' @return List with `estimate` (log-likelihood), `filtered`, `T`, `S`, `method`.
#' @examples
#' A <- rbind(c(0.7, 0.3), c(0.4, 0.6)); B <- rbind(c(0.9, 0.1), c(0.2, 0.8))
#' round(morie_wasserman_hmm_forward(c(0, 1, 0), A, B, c(0.6, 0.4))$estimate, 6)
#' @export
morie_wasserman_hmm_forward <- function(obs, A, B, pi) {
  obs <- as.integer(obs); A <- as.matrix(A); B <- as.matrix(B)
  pi <- as.numeric(pi)
  S <- nrow(B); M <- ncol(B)
  .morie_wsm_need(all(dim(A) == c(S, S)), sprintf(
    "A must be %dx%d to match B's rows; got %dx%d.", S, S, nrow(A), ncol(A)))
  .morie_wsm_need(length(pi) == S,
                  sprintf("pi must have %d entries; got %d.", S, length(pi)))
  .morie_wsm_need(all(abs(rowSums(A) - 1) <= 1e-8), "rows of A must sum to 1.")
  .morie_wsm_need(all(abs(rowSums(B) - 1) <= 1e-8), "rows of B must sum to 1.")
  .morie_wsm_need(abs(sum(pi) - 1) <= 1e-8, "pi must sum to 1.")
  .morie_wsm_need(length(obs) > 0,
                  "the forward algorithm needs at least one observation.")
  .morie_wsm_need(all(obs >= 0 & obs < M), sprintf(
    "observation index %d is outside the emission alphabet of size %d.",
    obs[obs < 0 | obs >= M][1], M))
  alpha <- pi * B[, obs[1] + 1L]
  ll <- 0
  c1 <- sum(alpha)
  if (c1 == 0) return(list(estimate = -Inf, filtered = numeric(S),
                           T = length(obs), S = S,
                           method = "forward (impossible sequence)"))
  alpha <- alpha / c1; ll <- log(c1)
  for (o in obs[-1]) {
    alpha <- as.numeric(alpha %*% A) * B[, o + 1L]
    cc <- sum(alpha)
    if (cc == 0) return(list(estimate = -Inf, filtered = numeric(S),
                             T = length(obs), S = S,
                             method = "forward (impossible sequence)"))
    alpha <- alpha / cc; ll <- ll + log(cc)
  }
  list(estimate = ll, filtered = alpha, T = length(obs), S = S,
       method = "scaled forward algorithm; exact log-likelihood")
}

#' Viterbi decoding in log space (Ch 23, wsmvit)
#'
#' Ties are broken toward the LOWER state index, which is what makes
#' the path deterministic across implementations.
#'
#' @param obs 0-based observation indices.
#' @param A,B,pi As in [morie_wasserman_hmm_forward()].
#' @return List with `estimate` (log probability of the best path),
#'   `path` (0-based states), `T`, `S`, `method`.
#' @examples
#' A <- rbind(c(0.7, 0.3), c(0.4, 0.6)); B <- rbind(c(0.9, 0.1), c(0.2, 0.8))
#' morie_wasserman_viterbi(c(0, 1, 0), A, B, c(0.6, 0.4))$path
#' @export
morie_wasserman_viterbi <- function(obs, A, B, pi) {
  obs <- as.integer(obs); A <- as.matrix(A); B <- as.matrix(B)
  pi <- as.numeric(pi)
  S <- nrow(B); M <- ncol(B); Tn <- length(obs)
  .morie_wsm_need(all(dim(A) == c(S, S)), sprintf(
    "A must be %dx%d to match B's rows; got %dx%d.", S, S, nrow(A), ncol(A)))
  .morie_wsm_need(length(pi) == S,
                  sprintf("pi must have %d entries; got %d.", S, length(pi)))
  .morie_wsm_need(Tn > 0, "Viterbi needs at least one observation.")
  .morie_wsm_need(all(obs >= 0 & obs < M), sprintf(
    "observation index %d is outside the emission alphabet of size %d.",
    obs[obs < 0 | obs >= M][1], M))
  lA <- suppressWarnings(log(A)); lB <- suppressWarnings(log(B))
  lpi <- suppressWarnings(log(pi))
  delta <- lpi + lB[, obs[1] + 1L]
  back <- matrix(0L, Tn, S)
  if (Tn > 1L) for (t in 2:Tn) {
    cand <- sweep(lA, 1, delta, `+`)
    back[t, ] <- max.col(t(cand), ties.method = "first")
    delta <- cand[cbind(back[t, ], seq_len(S))] + lB[, obs[t] + 1L]
  }
  end <- which.max(delta)
  path <- integer(Tn); path[Tn] <- end
  if (Tn > 1L) for (t in Tn:2) path[t - 1L] <- back[t, path[t]]
  list(estimate = delta[end], path = path - 1L, T = Tn, S = S,
       method = "log-space Viterbi, ties to lower state index")
}

# Acklam's rational approximation of the standard normal quantile
# (|error| < 1.15e-9). The PYTHON side uses this too: stats::qnorm is
# more accurate, but parity requires the same approximation in both
# languages, so LCG-driven chains agree draw for draw.
.morie_wsm_norm_inv <- function(u) {
  a <- c(-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
         1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00)
  b <- c(-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
         6.680131188771972e+01, -1.328068155288572e+01)
  cc <- c(-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
          -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00)
  d <- c(7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
         3.754408661907416e+00)
  plow <- 0.02425; phigh <- 1 - plow
  vapply(u, function(ui) {
    if (ui < plow) {
      q <- sqrt(-2 * log(ui))
      (((((cc[1]*q+cc[2])*q+cc[3])*q+cc[4])*q+cc[5])*q+cc[6]) /
        ((((d[1]*q+d[2])*q+d[3])*q+d[4])*q+1)
    } else if (ui > phigh) {
      q <- sqrt(-2 * log(1 - ui))
      -(((((cc[1]*q+cc[2])*q+cc[3])*q+cc[4])*q+cc[5])*q+cc[6]) /
        ((((d[1]*q+d[2])*q+d[3])*q+d[4])*q+1)
    } else {
      q <- ui - 0.5; r <- q * q
      (((((a[1]*r+a[2])*r+a[3])*r+a[4])*r+a[5])*r+a[6])*q /
        (((((b[1]*r+b[2])*r+b[3])*r+b[4])*r+b[5])*r+1)
    }
  }, numeric(1))
}

#' Gibbs sampler for a bivariate normal (Ch 24.4, wsmgib)
#'
#' Full conditionals X | Y = y ~ N(rho y, 1 - rho^2), sampled by
#' inversion on the shared exact-integer LCG so the chain reproduces
#' the Python one draw for draw.
#'
#' @param target Correlation rho with |rho| < 1.
#' @param x0 Length-2 starting point.
#' @param n Sweeps, at least 1.
#' @param seed LCG seed.
#' @return List with `estimate` (sample correlation), `samples_x`,
#'   `samples_y`, `mean_x`, `mean_y`, `n`, `method`.
#' @examples
#' round(morie_wasserman_gibbs_sampler(0.9, c(0, 0), 200)$estimate, 3)
#' @export
morie_wasserman_gibbs_sampler <- function(target, x0, n, seed = 13) {
  rho <- as.numeric(target)[1]; n <- as.integer(n)
  .morie_wsm_need(rho > -1 && rho < 1, sprintf(
    "the correlation must satisfy |rho| < 1; got %s.", rho))
  .morie_wsm_need(n >= 1L, sprintf("the sampler needs n >= 1 sweeps; got %d.", n))
  x <- as.numeric(x0)[1]; y <- as.numeric(x0)[2]
  s <- sqrt(1 - rho^2)
  u <- .morie_wsm_lcg_u(2 * n, seed)
  xs <- numeric(n); ys <- numeric(n)
  for (t in seq_len(n)) {
    x <- rho * y + s * .morie_wsm_norm_inv(u[2 * t - 1L])
    y <- rho * x + s * .morie_wsm_norm_inv(u[2 * t])
    xs[t] <- x; ys[t] <- y
  }
  list(estimate = stats::cor(xs, ys), samples_x = xs, samples_y = ys,
       mean_x = mean(xs), mean_y = mean(ys), n = n,
       method = "Gibbs on bivariate normal conditionals, LCG inversion")
}

#' Random-walk Metropolis sampler (Ch 24.3, wsmmcm)
#'
#' The proposal is symmetric so q cancels and alpha = min(1, p(x')/p(x)).
#' Both the step and the accept decision come from the shared LCG.
#'
#' @param target Unnormalised density function, non-negative.
#' @param proposal Positive random-walk standard deviation.
#' @param x0 Starting point with target(x0) > 0.
#' @param n Iterations, at least 1.
#' @param seed LCG seed.
#' @return List with `estimate` (chain mean), `samples`,
#'   `acceptance_rate`, `n`, `method`.
#' @examples
#' p <- function(x) exp(-0.5 * x^2)
#' round(morie_wasserman_mcmc_metropolis(p, 1, 0, 200)$acceptance_rate, 3)
#' @export
morie_wasserman_mcmc_metropolis <- function(target, proposal, x0, n, seed = 13) {
  step <- as.numeric(proposal)[1]; n <- as.integer(n)
  .morie_wsm_need(step > 0, sprintf("the proposal sd must be positive; got %s.", step))
  .morie_wsm_need(n >= 1L, sprintf("the sampler needs n >= 1; got %d.", n))
  x <- as.numeric(x0)[1]; px <- as.numeric(target(x))
  .morie_wsm_need(px > 0, "the chain must start where the target is positive.")
  u <- .morie_wsm_lcg_u(2 * n, seed)
  samples <- numeric(n); accepted <- 0L
  for (t in seq_len(n)) {
    prop <- x + step * .morie_wsm_norm_inv(u[2 * t - 1L])
    pp <- as.numeric(target(prop))
    .morie_wsm_need(pp >= 0, "a density cannot be negative.")
    if (u[2 * t] < min(1, if (px > 0) pp / px else 0)) {
      x <- prop; px <- pp; accepted <- accepted + 1L
    }
    samples[t] <- x
  }
  list(estimate = mean(samples), samples = samples,
       acceptance_rate = accepted / n, n = n,
       method = "random-walk Metropolis, LCG-driven, symmetric q cancels")
}

#' Directed graphical model factorisation (Ch 17, wsmdir)
#'
#' p(x) = prod_i p(x_i | pa(x_i)) for a binary DAG given in topological
#' order; a parent index at or after its child is refused, which is
#' also what rules out cycles.
#'
#' @param dag List of nodes, each `list(parents = <indices>, cpt = <list>)`
#'   where cpt maps a parent bit-string key to P(X_i = 1).
#' @param x Binary configuration of the same length.
#' @return List with `estimate` (joint probability), `log_joint`,
#'   `factors`, `n_nodes`, `method`.
#' @examples
#' dag <- list(list(parents = integer(0), cpt = list(0.3)),
#'             list(parents = 0L, cpt = list(`0` = 0.2, `1` = 0.9)))
#' round(morie_wasserman_directed_graph(dag, c(1, 1))$estimate, 12)
#' @export
morie_wasserman_directed_graph <- function(dag, x) {
  x <- as.integer(x)
  .morie_wsm_need(length(dag) == length(x), sprintf(
    "dag has %d nodes but x has %d entries.", length(dag), length(x)))
  .morie_wsm_need(all(x %in% c(0L, 1L)), "configurations must be binary 0/1.")
  factors <- numeric(length(dag))
  for (i in seq_along(dag)) {
    parents <- as.integer(dag[[i]]$parents)
    bad <- parents[parents >= i - 1L]
    .morie_wsm_need(length(bad) == 0L, sprintf(
      paste("node %d has parent %d not earlier in the ordering;",
            "supply a topological order."), i - 1L, bad[1]))
    # A root node has no parent key to look up -- R cannot name a list
    # element "" -- so it takes the single CPT entry.
    if (length(parents) == 0L) {
      .morie_wsm_need(length(dag[[i]]$cpt) == 1L,
                      sprintf("root node %d needs exactly one CPT entry.", i - 1L))
      p1 <- as.numeric(dag[[i]]$cpt[[1]])
    } else {
      key <- paste(x[parents + 1L], collapse = "")
      .morie_wsm_need(!is.null(dag[[i]]$cpt[[key]]), sprintf(
        "node %d's CPT lacks the parent configuration (%s).", i - 1L, key))
      p1 <- as.numeric(dag[[i]]$cpt[[key]])
    }
    .morie_wsm_need(p1 >= 0 && p1 <= 1, sprintf(
      "node %d's CPT value %s is not a probability.", i - 1L, p1))
    factors[i] <- if (x[i] == 1L) p1 else 1 - p1
  }
  joint <- prod(factors)
  list(estimate = joint,
       log_joint = if (joint > 0) sum(log(factors)) else -Inf,
       factors = factors, n_nodes = length(dag),
       method = "DAG factorisation prod p(x_i | pa_i), binary CPTs")
}

#' Exact undirected clique model (Ch 17, wsmund)
#'
#' p(x) = prod_C psi_C(x_C) / Z with Z summed over all 2^n binary
#' configurations -- the didactic exact version, capped at 20 nodes.
#' Configuration index is the binary number with x_0 as the most
#' significant bit.
#'
#' @param graph List(n_nodes, cliques) with cliques as 0-based index vectors.
#' @param psi List of clique potentials, each returning a positive value.
#' @return List with `estimate` (Z), `probabilities`, `n_nodes`,
#'   `n_cliques`, `method`.
#' @examples
#' agree <- function(t) if (t[1] == t[2]) 2 else 1
#' morie_wasserman_undirected_graph(list(2, list(c(0, 1))), list(agree))$estimate
#' @export
morie_wasserman_undirected_graph <- function(graph, psi) {
  n <- as.integer(graph[[1]]); cliques <- graph[[2]]
  .morie_wsm_need(n >= 1L && n <= 20L, sprintf(
    "the exact version handles 1 <= n <= 20 nodes; got %d.", n))
  .morie_wsm_need(length(cliques) == length(psi), sprintf(
    "%d cliques but %d potentials.", length(cliques), length(psi)))
  for (cl in cliques) .morie_wsm_need(all(cl >= 0 & cl < n),
                                      "a clique references a node outside the graph.")
  configs <- as.matrix(expand.grid(rep(list(c(0L, 1L)), n)))
  configs <- configs[, rev(seq_len(n)), drop = FALSE]   # x_0 most significant
  ord <- do.call(order, lapply(seq_len(n), function(j) configs[, j]))
  configs <- configs[ord, , drop = FALSE]
  weights <- numeric(nrow(configs))
  for (r in seq_len(nrow(configs))) {
    w <- 1
    for (ci in seq_along(cliques)) {
      val <- as.numeric(psi[[ci]](configs[r, as.integer(cliques[[ci]]) + 1L]))
      .morie_wsm_need(val > 0, "clique potentials must be strictly positive.")
      w <- w * val
    }
    weights[r] <- w
  }
  Z <- sum(weights)
  list(estimate = Z, probabilities = weights / Z, n_nodes = n,
       n_cliques = length(cliques),
       method = "exact undirected model; brute-force Z over 2^n configs")
}

#' Normalised clique-potential model with its mode (Ch 17, wsmgrp)
#'
#' @param graph,psi As in [morie_wasserman_undirected_graph()].
#' @return List with `estimate` (probability of the mode), `mode`
#'   (bit vector), `partition_function`, `probabilities`, `n_nodes`, `method`.
#' @examples
#' agree <- function(t) if (t[1] == t[2]) 2 else 1
#' morie_wasserman_graphical_model(list(2, list(c(0, 1))), list(agree))$mode
#' @export
morie_wasserman_graphical_model <- function(graph, psi) {
  core <- morie_wasserman_undirected_graph(graph, psi)
  probs <- core$probabilities; n <- core$n_nodes
  best <- which.max(probs) - 1L
  mode <- vapply(seq_len(n), function(j) bitwAnd(bitwShiftR(best, n - j), 1L),
                 integer(1))
  list(estimate = probs[best + 1L], mode = mode,
       partition_function = core$estimate, probabilities = probs, n_nodes = n,
       method = "clique-potential joint; mode decoded (ties -> lowest index)")
}

#' Saturated log-linear model for a two-way table (Ch 17, wsmlgc)
#'
#' The lambdas are the zero-sum ANOVA decomposition of log n_ij (the
#' saturated model fits exactly); the independence fit and its G^2
#' likelihood-ratio statistic come along. Interaction terms are
#' ROW-MAJOR.
#'
#' @param table I x J matrix of strictly positive counts, at least 2x2.
#' @return List with `estimate` (G^2 vs independence), `lambda0`,
#'   `lambda_row`, `lambda_col`, `lambda_int`, `independence_fit`,
#'   `df`, `n`, `method`.
#' @examples
#' round(morie_wasserman_log_linear(rbind(c(30, 10), c(15, 45)))$estimate, 6)
#' @export
morie_wasserman_log_linear <- function(table) {
  T <- as.matrix(table); I <- nrow(T); J <- ncol(T)
  .morie_wsm_need(I >= 2L && J >= 2L, sprintf(
    "a two-way table needs at least 2x2 cells; got %dx%d.", I, J))
  .morie_wsm_need(all(T > 0),
                  "the saturated log-linear model needs strictly positive counts.")
  L <- log(T)
  lam0 <- mean(L)
  lr <- rowMeans(L) - lam0
  lc <- colMeans(L) - lam0
  lint <- L - lam0 - outer(lr, rep(1, J)) - outer(rep(1, I), lc)
  n <- sum(T)
  mu_ind <- outer(rowSums(T), colSums(T)) / n
  list(estimate = 2 * sum(T * log(T / mu_ind)), lambda0 = lam0,
       lambda_row = lr, lambda_col = lc, lambda_int = as.numeric(t(lint)),
       independence_fit = as.numeric(t(mu_ind)), df = (I - 1L) * (J - 1L), n = n,
       method = "saturated log-linear (zero-sum ANOVA of log counts) + G^2")
}

#' AdaBoost.M1 with decision stumps (Ch 22, wsmbst)
#'
#' The weak learner is an exhaustive axis-aligned stump search with
#' deterministic tie-breaks; a perfect stump gets a capped alpha and
#' ends the run, and a stump no better than chance stops it.
#'
#' @param X Feature matrix.
#' @param y Labels in {-1, +1}.
#' @param model Weak-learner factory (X, y, w) -> predict function;
#'   NULL = built-in stumps.
#' @param T Boosting rounds, at least 1.
#' @return List with `estimate` (training error rate), `prediction`,
#'   `alphas`, `rounds_used`, `n`, `method`.
#' @examples
#' morie_wasserman_boosting(matrix(0:3, ncol = 1), c(1, -1, 1, -1), NULL, 5)$rounds_used
#' @export
morie_wasserman_boosting <- function(X, y, model, T) {
  X <- as.matrix(X); y <- as.numeric(y); n <- nrow(X); T <- as.integer(T)
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d labels.", n, length(y)))
  .morie_wsm_need(all(y %in% c(-1, 1)), "labels must lie in {-1, +1}.")
  .morie_wsm_need(T >= 1L, sprintf("boosting needs T >= 1 rounds; got %d.", T))
  best_stump <- function(w) {
    best <- list(err = Inf, j = 1L, thr = 0, sign = 1L)
    for (j in seq_len(ncol(X))) for (thr in sort(unique(X[, j]))) for (sg in c(1L, -1L)) {
      pred <- ifelse(X[, j] <= thr, sg, -sg)
      err <- sum(w[pred != y])
      if (err < best$err - 1e-15) best <- list(err = err, j = j, thr = thr, sign = sg)
    }
    best
  }
  w <- rep(1 / n, n); F <- numeric(n); alphas <- numeric(0); rounds <- 0L
  for (round in seq_len(T)) {
    if (is.null(model)) {
      st <- best_stump(w)
      err <- st$err
      pred <- ifelse(X[, st$j] <= st$thr, st$sign, -st$sign)
    } else {
      predict_fn <- model(X, y, w)
      pred <- as.numeric(predict_fn(X))
      err <- sum(w[pred != y])
    }
    if (err >= 0.5) break
    rounds <- rounds + 1L
    alpha <- if (err == 0) 10 else 0.5 * log((1 - err) / err)
    alphas <- c(alphas, alpha)
    F <- F + alpha * pred
    if (err == 0) break
    w <- w * exp(-alpha * y * pred); w <- w / sum(w)
  }
  committee <- ifelse(F >= 0, 1L, -1L)
  list(estimate = mean(committee != y), prediction = committee, alphas = alphas,
       rounds_used = rounds, n = n,
       method = paste("AdaBoost.M1, exhaustive stumps, deterministic ties;",
                      "perfect-stump alpha capped at 10"))
}

#' Linear SVM by pairwise dual ascent (Ch 22, wsmsvm)
#'
#' Solves the box-constrained dual with pair updates that keep
#' sum a_i y_i = 0 exact; the default C = 1e6 approximates the hard
#' margin. The margin, support-vector set and KKT residual are
#' reported so optimality is inspectable rather than asserted.
#'
#' @param X Feature matrix.
#' @param y Labels in {-1, +1}, both classes present.
#' @param C Box constraint.
#' @param max_iter,tol Solver controls.
#' @return List with `estimate` (margin 2/|w|), `w`, `b`,
#'   `support_vectors` (0-based), `alphas`, `kkt_violation`, `n`, `d`, `method`.
#' @examples
#' round(morie_wasserman_svm(rbind(c(-1, -1), c(1, 1)), c(-1, 1))$w, 8)
#' @export
morie_wasserman_svm <- function(X, y, C = 1e6, max_iter = 1000L, tol = 1e-12) {
  X <- as.matrix(X); y <- as.numeric(y); n <- nrow(X); d <- ncol(X)
  C <- as.numeric(C)[1]
  .morie_wsm_need(length(y) == n,
                  sprintf("X has %d rows but y has %d labels.", n, length(y)))
  .morie_wsm_need(all(y %in% c(-1, 1)), "labels must lie in {-1, +1}.")
  .morie_wsm_need(length(unique(y)) == 2L, "the SVM needs both classes present.")
  K <- tcrossprod(X)
  a <- numeric(n)
  for (sweep_i in seq_len(as.integer(max_iter))) {
    moved <- 0
    u <- as.numeric(K %*% (a * y))
    for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
      kappa <- K[i, i] - 2 * K[i, j] + K[j, j]
      if (kappa <= 1e-300) next
      tstar <- (y[i] - y[j] - u[i] + u[j]) / kappa
      b1 <- -a[i] * y[i]; b2 <- (C - a[i]) * y[i]
      lo <- min(b1, b2); hi <- max(b1, b2)
      b3 <- a[j] * y[j]; b4 <- (a[j] - C) * y[j]
      lo <- max(lo, min(b3, b4)); hi <- min(hi, max(b3, b4))
      tt <- min(max(tstar, lo), hi)
      if (tt == 0) next
      a[i] <- a[i] + y[i] * tt
      a[j] <- a[j] - y[j] * tt
      u <- u + K[, i] * tt - K[, j] * tt
      moved <- moved + abs(tt)
    }
    if (moved < tol) break
  }
  w <- as.numeric(crossprod(X, a * y))
  free <- which(a > 1e-8 & a < C - 1e-8)
  sv <- which(a > 1e-8)
  b <- if (length(free)) mean(y[free] - as.numeric(X[free, , drop = FALSE] %*% w))
       else if (length(sv)) mean(y[sv] - as.numeric(X[sv, , drop = FALSE] %*% w))
       else stop("the solver found no support vectors; data may be degenerate.",
                 call. = FALSE)
  margins <- y * (as.numeric(X %*% w) + b)
  nw <- sqrt(sum(w^2))
  list(estimate = if (nw > 0) 2 / nw else Inf, w = w, b = b,
       support_vectors = sv - 1L, alphas = a,
       kkt_violation = if (length(sv)) max(0, 1 - min(margins[sv])) else NaN,
       n = n, d = d,
       method = "linear SVM, cyclic pairwise dual ascent, C=1e6 ~ hard margin")
}

#' Minimax value of a finite decision problem (Ch 12, wsmmin)
#'
#' Reports inf_T sup_F R alongside the maximin value; weak duality
#' (maximin <= minimax) is checked, and equality flags a pure saddle.
#'
#' @param loss Risk matrix (m x k), R\\[i, j\\] = R(estimator_i, F_j).
#' @param estimator Labels for the m rows.
#' @param family Labels for the k columns.
#' @return List with `estimate` (minimax risk), `minimax_estimator`,
#'   `worst_case`, `maximin`, `has_pure_saddle`, `m`, `k`, `method`.
#' @examples
#' R <- rbind(c(1, 4), c(2, 2), c(3, 1))
#' morie_wasserman_minimax(R, c("T1", "T2", "T3"), c("F1", "F2"))$minimax_estimator
#' @export
morie_wasserman_minimax <- function(loss, estimator, family) {
  R <- as.matrix(loss); m <- nrow(R); k <- ncol(R)
  est <- as.character(estimator); fam <- as.character(family)
  .morie_wsm_need(length(est) == m, sprintf(
    "the risk matrix is %dx%d but there are %d estimator labels.", m, k, length(est)))
  .morie_wsm_need(length(fam) == k, sprintf(
    "the risk matrix is %dx%d but there are %d family labels.", m, k, length(fam)))
  worst <- apply(R, 1, max)
  i_star <- which.min(worst)
  minimax <- worst[i_star]
  maximin <- max(apply(R, 2, min))
  .morie_wsm_need(maximin <= minimax + 1e-12,
                  "weak duality violated -- impossible; numerical fault.")
  list(estimate = minimax, minimax_estimator = est[i_star], worst_case = worst,
       maximin = maximin, has_pure_saddle = abs(maximin - minimax) < 1e-12,
       m = m, k = k,
       method = "exact min over rows of max over columns; maximin duality check")
}
