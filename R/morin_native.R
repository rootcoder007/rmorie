# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Probability-theory mirrors for the Morin shelf.
#
# Mirrors the computational surface of the 174 morie.fn
# david_j_morin_probability_for_the_enthusiastic_beginner* modules
# (Morin, D. J. (2016), Probability: For the Enthusiastic Beginner,
# chapters 1-7). Counting uses exact arithmetic via choose()/
# factorial(); distribution tails use log-space forms mirroring the
# Python arm's lgamma implementations, so the two arms agree at
# machine precision.
#
# Text-verified traps carried over from the Python arm: the module
# named "10e6" is the literal 10.6 inside eq (6.76) (the real target
# is the model sigma_y of eq (6.5)); "19e2" is the 19.2 = <x^2>
# inside eq (6.89) (the target is the least-squares intercept,
# eq (6.49)); and eq (5.31) lost its square root to OCR -- 2.24 is
# the standard deviation, not the variance.

#' Partial permutations N_P_n
#'
#' Ordered subgroups of n from N: N(N-1)...(N-(n-1)) = N!/(N-n)!.
#' Mirrors morie.fn ...1e3/1e5.
#'
#' @param N,n pool size and subgroup size, 0 <= n <= N.
#' @return the exact count as a double.
#' @references Morin (2016), eqs. (1.3), (1.5)-(1.6).
#' @examples
#' morie_partial_permutations(10, 3)
#' @export
morie_partial_permutations <- function(N, n) {
  N <- as.integer(N)
  n <- as.integer(n)
  if (N < 0L || n < 0L || n > N) stop("need 0 <= n <= N.", call. = FALSE)
  if (n == 0L) return(1)
  prod(seq.int(N, N - n + 1L))
}

#' Multinomial coefficient with an implicit leftover committee
#'
#' N!/(n1! n2! ... nk!); when sum(ns) < N the leftover people form
#' one extra committee (Morin's remark below eq (1.35)). Mirrors
#' morie.fn ...1e35/1e37.
#'
#' @param ns committee sizes.
#' @param N total people (default sum(ns)).
#' @return the exact coefficient as a double.
#' @references Morin (2016), eqs. (1.35), (1.37).
#' @examples
#' morie_multinomial_coef(c(3, 2, 5))
#' @export
morie_multinomial_coef <- function(ns, N = NULL) {
  ns <- as.integer(ns)
  if (any(ns < 0L)) stop("committee sizes must be >= 0.", call. = FALSE)
  if (is.null(N)) N <- sum(ns)
  N <- as.integer(N)
  if (sum(ns) > N) stop("sum(ns) exceeds N.", call. = FALSE)
  if (sum(ns) < N) ns <- c(ns, N - sum(ns))
  prod(choose(cumsum(ns), ns))
}

#' Stars and bars count
#'
#' Unordered samples of n with repetition from N types:
#' C(n + N - 1, N - 1). Mirrors morie.fn ...1e57.
#'
#' @param n draws.
#' @param N types, >= 1.
#' @return the exact count.
#' @references Morin (2016), eqs. (1.16), (1.57).
#' @examples
#' morie_stars_bars(2, 3)
#' @export
morie_stars_bars <- function(n, N) {
  n <- as.integer(n)
  N <- as.integer(N)
  if (n < 0L || N < 1L) stop("need n >= 0 and N >= 1.", call. = FALSE)
  choose(n + N - 1, N - 1)
}

#' Hockey-stick identity
#'
#' sum_{j=k-1}^{n-1} C(j, k-1) = C(n, k). Mirrors morie.fn ...1e29.
#'
#' @param n,k identity parameters, 1 <= k <= n.
#' @return list(sum, binomial, holds).
#' @references Morin (2016), eq. (1.29).
#' @examples
#' morie_hockey_stick(6, 2)$holds
#' @export
morie_hockey_stick <- function(n, k) {
  n <- as.integer(n)
  k <- as.integer(k)
  if (k < 1L || k > n) stop("need 1 <= k <= n.", call. = FALSE)
  s <- sum(choose(seq.int(k - 1L, n - 1L), k - 1L))
  list(sum = s, binomial = choose(n, k), holds = isTRUE(all.equal(s, choose(n, k))))
}

#' Elementary probability rules for an event pair
#'
#' And/or rules, the general or-rule, and the independent/exclusive
#' classification. Mirrors morie.fn ...2e2/2e14/2e21/2e24/2e70.
#'
#' @param p_a,p_b marginal probabilities.
#' @param p_ab joint probability P(A and B).
#' @return list: and_independent, or_exclusive, or_general,
#'   independent, exclusive.
#' @references Morin (2016), eqs. (2.2), (2.14), (2.21), (2.24),
#'   (2.70)-(2.71).
#' @examples
#' morie_prob_rules(1 / 13, 1 / 4, 1 / 52)$or_general
#' @export
morie_prob_rules <- function(p_a, p_b, p_ab) {
  for (p in c(p_a, p_b, p_ab)) {
    if (p < 0 || p > 1) stop("probabilities must be in [0, 1].", call. = FALSE)
  }
  if (p_ab > min(p_a, p_b) + 1e-12) {
    stop("P(A and B) cannot exceed min(P(A), P(B)).", call. = FALSE)
  }
  list(
    and_independent = p_a * p_b,
    or_exclusive = p_a + p_b,
    or_general = p_a + p_b - p_ab,
    independent = abs(p_ab - p_a * p_b) <= 1e-12,
    exclusive = p_ab <= 1e-12
  )
}

#' Bayes' theorem over a complete hypothesis set
#'
#' Posteriors P(Ak | Z) = P(Z | Ak) P(Ak) / sum_i P(Z | Ai) P(Ai);
#' covers the simple, explicit and general forms plus the law of
#' total probability. Mirrors morie.fn ...2e29/2e51-2e62/2e74/2e86.
#'
#' @param priors prior probabilities, summing to 1.
#' @param likelihoods P(Z | Ai), same length.
#' @return list(posteriors, p_z).
#' @references Morin (2016), eqs. (2.51)-(2.53), (2.55), (2.74).
#' @examples
#' morie_bayes(c(0.02, 0.98), c(0.95, 0.10))$posteriors[1]
#' @export
morie_bayes <- function(priors, likelihoods) {
  priors <- as.numeric(priors)
  likelihoods <- as.numeric(likelihoods)
  if (length(priors) != length(likelihoods) || length(priors) == 0L) {
    stop("priors and likelihoods must be equal-length, non-empty.", call. = FALSE)
  }
  if (any(priors < 0) || any(priors > 1) || any(likelihoods < 0) ||
        any(likelihoods > 1)) {
    stop("all probabilities must be in [0, 1].", call. = FALSE)
  }
  if (abs(sum(priors) - 1) > 1e-9) {
    stop("priors must sum to 1.", call. = FALSE)
  }
  p_z <- sum(priors * likelihoods)
  if (p_z == 0) stop("P(Z) = 0: no hypothesis can produce Z.", call. = FALSE)
  list(posteriors = priors * likelihoods / p_z, p_z = p_z)
}

#' Moments of a discrete pmf
#'
#' Mean, variance (definition and computational forms cross-checked)
#' and standard deviation. Mirrors morie.fn ...3e19/3e34/3e35/5e31.
#'
#' @param values,probs the pmf; probs must sum to 1.
#' @return list(mean, variance, sd).
#' @references Morin (2016), eqs. (3.19), (3.34)-(3.35), (5.31).
#' @examples
#' morie_pmf_moments(1:6, rep(1 / 6, 6))$variance
#' @export
morie_pmf_moments <- function(values, probs) {
  values <- as.numeric(values)
  probs <- as.numeric(probs)
  if (length(values) != length(probs) || length(values) == 0L) {
    stop("values and probs must be equal-length, non-empty.", call. = FALSE)
  }
  if (any(probs < 0) || abs(sum(probs) - 1) > 1e-9) {
    stop("probs must be >= 0 and sum to 1.", call. = FALSE)
  }
  mu <- sum(values * probs)
  v_def <- sum(probs * (values - mu)^2)
  v_comp <- sum(probs * values^2) - mu^2
  if (abs(v_def - v_comp) > 1e-9 * max(1, abs(v_def))) {
    stop("variance forms disagree.", call. = FALSE)
  }
  list(mean = mu, variance = v_def, sd = sqrt(v_def))
}

#' pmf of the sum of two independent discrete variables
#'
#' Discrete convolution. Mirrors morie.fn ...3e11/3e12/3e28.
#'
#' @param values_x,probs_x,values_y,probs_y the two pmfs.
#' @return list(values, probs) sorted by value.
#' @references Morin (2016), Sec. 3.1, eq. (3.11) example.
#' @examples
#' morie_pmf_convolve(1:2, c(0.5, 0.5), 1:3, rep(1 / 3, 3))$probs
#' @export
morie_pmf_convolve <- function(values_x, probs_x, values_y, probs_y) {
  vx <- as.numeric(values_x)
  px <- as.numeric(probs_x)
  vy <- as.numeric(values_y)
  py <- as.numeric(probs_y)
  if (abs(sum(px) - 1) > 1e-9 || abs(sum(py) - 1) > 1e-9) {
    stop("each pmf must sum to 1.", call. = FALSE)
  }
  sums <- outer(vx, vy, "+")
  ps <- outer(px, py)
  agg <- tapply(as.vector(ps), as.vector(sums), sum)
  vals <- as.numeric(names(agg))
  ord <- order(vals)
  list(values = vals[ord], probs = as.numeric(agg)[ord])
}

#' Population and sample variance of a data vector
#'
#' s-tilde^2 with the 1/n divisor, the unbiased s^2 with 1/(n-1),
#' and the computational identity. Mirrors morie.fn
#' ...3e37/3e60/3e66/3e73.
#'
#' @param x numeric data, length >= 2.
#' @return list(mean, population_variance, sample_variance).
#' @references Morin (2016), eqs. (3.37), (3.60), (3.66), (3.73).
#' @examples
#' morie_data_variance(c(2, 4, 4, 4, 5, 5, 7, 9))$population_variance
#' @export
morie_data_variance <- function(x) {
  x <- as.numeric(x)
  if (length(x) < 2L) stop("need n >= 2.", call. = FALSE)
  m <- mean(x)
  pop <- mean((x - m)^2)
  comp <- mean(x^2) - m^2
  if (abs(pop - comp) > 1e-9 * max(1, abs(pop))) {
    stop("variance forms disagree.", call. = FALSE)
  }
  list(mean = m, population_variance = pop,
       sample_variance = sum((x - m)^2) / (length(x) - 1))
}

#' Standard deviations of sums and means
#'
#' sigma_aX, sigma of independent sums, sqrt(n) sigma for i.i.d.
#' sums, sigma/sqrt(n) for the mean, and sqrt(sum sigma_i^2)/n for
#' heterogeneous averages. Mirrors morie.fn ...3e41-3e55, 1e71.
#'
#' @param sigmas per-variable standard deviations.
#' @param a scale factor for the first sigma.
#' @return list: scaled, sd_sum, sd_iid_sum, sd_mean, sd_mean_hetero.
#' @references Morin (2016), eqs. (3.41)-(3.55).
#' @examples
#' morie_sd_forms(c(3, 4))$sd_sum
#' @export
morie_sd_forms <- function(sigmas, a = 1) {
  sigmas <- as.numeric(sigmas)
  if (any(sigmas < 0)) stop("sigmas must be >= 0.", call. = FALSE)
  n <- length(sigmas)
  list(
    scaled = abs(a) * sigmas[1],
    sd_sum = sqrt(sum(sigmas^2)),
    sd_iid_sum = sqrt(n) * sigmas[1],
    sd_mean = sigmas[1] / sqrt(n),
    sd_mean_hetero = sqrt(sum(sigmas^2)) / n
  )
}

#' Binomial distribution surface
#'
#' pmf (log-space above n = 1000, matching the Python arm), mean np,
#' second moment p^2 n(n-1) + pn, variance npq, and the p = 1/(n+1)
#' solving P(0) = P(1). Mirrors morie.fn ch4 binomial modules.
#'
#' @param n,p trial count and success probability.
#' @param k optional specific count.
#' @return list: pmf (if k given), mean, second_moment, variance,
#'   p_zero_equals_one.
#' @references Morin (2016), eqs. (4.6)-(4.10), (4.61), (4.66)-(4.67).
#' @examples
#' morie_binomial_dist(4, 0.5, k = 2)$pmf
#' @export
morie_binomial_dist <- function(n, p, k = NULL) {
  n <- as.integer(n)
  if (n < 0L || p < 0 || p > 1) stop("need n >= 0, p in [0, 1].", call. = FALSE)
  pmf <- NULL
  if (!is.null(k)) {
    k <- as.integer(k)
    if (k < 0L || k > n) {
      pmf <- 0
    } else if (p == 0 || p == 1) {
      pmf <- as.numeric(k == n * p)
    } else if (n <= 1000L) {
      pmf <- choose(n, k) * p^k * (1 - p)^(n - k)
    } else {
      pmf <- exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) +
                   k * log(p) + (n - k) * log1p(-p))
    }
  }
  list(
    pmf = pmf,
    mean = n * p,
    second_moment = p^2 * n * (n - 1) + p * n,
    variance = n * p * (1 - p),
    p_zero_equals_one = if (n >= 1L) 1 / (n + 1) else NA_real_
  )
}

#' Poisson distribution surface
#'
#' pmf in log space, series-checked mean and variance, mode
#' ceil(a) - 1, and the alternating e^-a series. Mirrors morie.fn
#' ch4 Poisson modules.
#'
#' @param a expected count, > 0 (>= 0 for the pmf).
#' @param k optional specific count.
#' @return list: pmf (if k given), mean, variance, mode, p_zero.
#' @references Morin (2016), eqs. (4.40), (4.53), (4.89), (4.92),
#'   (4.94).
#' @examples
#' morie_poisson_dist(2.0, k = 3)$pmf
#' @export
morie_poisson_dist <- function(a, k = NULL) {
  a <- as.numeric(a)
  if (a < 0) stop("a must be >= 0.", call. = FALSE)
  pmf <- NULL
  if (!is.null(k)) {
    k <- as.integer(k)
    pmf <- if (a == 0) as.numeric(k == 0L) else exp(k * log(a) - a - lgamma(k + 1))
  }
  kmax <- max(50, ceiling(a + 15 * sqrt(a + 1)))
  ks <- 0:(kmax - 1)
  p_all <- exp(ks * log(max(a, .Machine$double.xmin)) - a - lgamma(ks + 1))
  if (a == 0) p_all <- as.numeric(ks == 0L)
  mu <- sum(ks * p_all)
  v <- sum(ks^2 * p_all) - mu^2
  list(pmf = pmf, mean = mu, variance = v,
       mode = if (a > 0) max(ceiling(a) - 1, 0) else 0L,
       p_zero = exp(-a))
}

#' Hypergeometric pmf and its binomial limit
#'
#' P(k) = C(K,k) C(N-K, n-k) / C(N,n); as N grows at fixed p = K/N
#' it approaches the binomial. Mirrors morie.fn ...4e71/4e73/4e75.
#'
#' @param k successes drawn.
#' @param N,K,n population, successes in population, draws.
#' @return list(pmf, binomial_limit, abs_error).
#' @references Morin (2016), eqs. (4.71), (4.73), (4.75).
#' @examples
#' morie_hypergeometric_dist(2, 52, 13, 5)$pmf
#' @export
morie_hypergeometric_dist <- function(k, N, K, n) {
  k <- as.integer(k)
  N <- as.integer(N)
  K <- as.integer(K)
  n <- as.integer(n)
  if (K > N || n > N) stop("need K <= N and n <= N.", call. = FALSE)
  pmf <- if (k > min(K, n) || (n - k) > (N - K) || k < 0L) {
    0
  } else {
    choose(K, k) * choose(N - K, n - k) / choose(N, n)
  }
  bl <- morie_binomial_dist(n, K / N, k = k)$pmf
  list(pmf = pmf, binomial_limit = bl, abs_error = abs(pmf - bl))
}

#' Exponential waiting-time surface
#'
#' Density lambda e^(-lambda t), interval probability
#' e^(-lambda t) lambda dt, moments (tau, 2 tau^2, tau^2) and the
#' survival crossing time log(ratio)/(r_fast - r_slow). Mirrors
#' morie.fn ...4e23-4e30, 4e83/4e85.
#'
#' @param lam rate lambda > 0.
#' @param t time >= 0.
#' @param dt interval width (default 0: skip).
#' @param rate_slow,ratio optional crossing-time inputs.
#' @return list: density, interval_probability, mean, second_moment,
#'   variance, crossing_time (if rate_slow given).
#' @references Morin (2016), eqs. (4.23)-(4.30), (4.83)-(4.86).
#' @examples
#' morie_exponential_dist(2.0, t = 0.5)$density
#' @export
morie_exponential_dist <- function(lam, t = 0, dt = 0, rate_slow = NULL,
                                   ratio = NULL) {
  lam <- as.numeric(lam)
  if (lam <= 0 || t < 0 || dt < 0) {
    stop("need lambda > 0, t >= 0, dt >= 0.", call. = FALSE)
  }
  tau <- 1 / lam
  crossing <- NULL
  if (!is.null(rate_slow)) {
    if (is.null(ratio) || lam <= rate_slow || ratio <= 0) {
      stop("crossing needs lam > rate_slow and ratio > 0.", call. = FALSE)
    }
    crossing <- log(ratio) / (lam - rate_slow)
  }
  list(
    density = lam * exp(-lam * t),
    interval_probability = exp(-lam * t) * lam * dt,
    mean = tau,
    second_moment = 2 * tau^2,
    variance = tau^2,
    crossing_time = crossing
  )
}

#' Gaussian approximations of binomial and Poisson peaks
#'
#' The centered forms e^(-x^2/n)/sqrt(pi n) (2n fair flips),
#' e^(-2x^2/n)/sqrt(pi n/2) (n fair flips),
#' e^(-x^2/(2npq))/sqrt(2 pi npq) (biased), and
#' e^(-(k-a)^2/(2a))/sqrt(2 pi a) (Poisson). Mirrors morie.fn ch5.
#'
#' @param x deviation from the center.
#' @param n flip count (interpretation set by \code{form}).
#' @param p success probability for the biased form.
#' @param a Poisson mean for the Poisson form.
#' @param form one of "two_n", "n", "biased", "poisson".
#' @return the Gaussian density value.
#' @references Morin (2016), eqs. (5.13)-(5.15), (5.23).
#' @examples
#' morie_gaussian_approx(0, n = 50, form = "two_n")
#' @export
morie_gaussian_approx <- function(x, n = NULL, p = NULL, a = NULL,
                                  form = c("two_n", "n", "biased", "poisson")) {
  form <- match.arg(form)
  x <- as.numeric(x)
  switch(form,
    two_n = {
      if (is.null(n) || n < 1) stop("need n >= 1.", call. = FALSE)
      exp(-x^2 / n) / sqrt(pi * n)
    },
    n = {
      if (is.null(n) || n < 1) stop("need n >= 1.", call. = FALSE)
      exp(-2 * x^2 / n) / sqrt(pi * n / 2)
    },
    biased = {
      if (is.null(n) || is.null(p)) stop("need n and p.", call. = FALSE)
      npq <- n * p * (1 - p)
      if (npq <= 0) stop("npq must be > 0.", call. = FALSE)
      exp(-x^2 / (2 * npq)) / sqrt(2 * pi * npq)
    },
    poisson = {
      if (is.null(a) || a <= 0) stop("need a > 0.", call. = FALSE)
      exp(-(x)^2 / (2 * a)) / sqrt(2 * pi * a)
    }
  )
}

#' The Y = mX + Z correlation model
#'
#' mu_y = m mu_x + mu_z, sigma_y = sqrt(m^2 sigma_x^2 + sigma_z^2),
#' r = m sigma_x / sigma_y, the reverse slope r sigma_x / sigma_y,
#' the regression-to-the-mean factor r^2 and the excess-score factor
#' sqrt((1-r)/(1+r)). Mirrors morie.fn ch6 model modules (and the
#' misnamed "10e6").
#'
#' @param m slope of the underlying relation.
#' @param sigma_x,sigma_z spreads of signal and noise.
#' @param mu_x,mu_z means of signal and noise.
#' @return list: mu_y, sigma_y, r, reverse_slope, r_squared,
#'   excess_factor.
#' @references Morin (2016), eqs. (6.3)-(6.6), (6.17), (6.36),
#'   (6.40), (6.76), (6.81).
#' @examples
#' morie_linear_corr_model(1, 7.5, 10.6)$sigma_y
#' @export
morie_linear_corr_model <- function(m, sigma_x, sigma_z, mu_x = 0, mu_z = 0) {
  if (sigma_x < 0 || sigma_z < 0) stop("sigmas must be >= 0.", call. = FALSE)
  sigma_y <- sqrt(m^2 * sigma_x^2 + sigma_z^2)
  if (sigma_y == 0) stop("degenerate model: sigma_y = 0.", call. = FALSE)
  r <- m * sigma_x / sigma_y
  excess <- if (abs(r) < 1) sqrt((1 - r) / (1 + r)) else NA_real_
  list(
    mu_y = m * mu_x + mu_z,
    sigma_y = sigma_y,
    r = r,
    reverse_slope = r * sigma_x / sigma_y,
    r_squared = r^2,
    excess_factor = excess
  )
}

#' Least-squares line and sample correlation
#'
#' A = (<xy> - <x><y>)/(<x^2> - <x>^2), B = <y> - A<x> (both book
#' forms cross-checked), residual sum of squares, sample r with the
#' 1/n covariance, and the slope product A*C = r^2. Mirrors
#' morie.fn ch6 data modules (and the misnamed "19e2").
#'
#' @param x,y data vectors, length >= 2.
#' @return list: A, B, S, r, C, slope_product.
#' @references Morin (2016), eqs. (6.12)-(6.14), (6.42)-(6.55),
#'   (6.82), (6.92).
#' @examples
#' morie_least_squares(c(2, 3, 3, 5, 7), c(1, 1, 3, 4, 6))$A
#' @export
morie_least_squares <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y) || length(x) < 2L) {
    stop("x and y must be equal-length, n >= 2.", call. = FALSE)
  }
  mx <- mean(x)
  my <- mean(y)
  mxy <- mean(x * y)
  mx2 <- mean(x^2)
  denom <- mx2 - mx^2
  if (denom == 0) stop("all x identical: slope undefined.", call. = FALSE)
  a_slope <- (mxy - mx * my) / denom
  b_first <- (my * mx2 - mx * mxy) / denom
  b_second <- my - a_slope * mx
  if (abs(b_first - b_second) > 1e-9 * max(1, abs(b_first))) {
    stop("intercept forms disagree.", call. = FALSE)
  }
  resid <- y - (a_slope * x + b_second)
  sx <- sqrt(mean((x - mx)^2))
  sy <- sqrt(mean((y - my)^2))
  if (sx == 0 || sy == 0) stop("degenerate data: zero variance.", call. = FALSE)
  r <- mean((x - mx) * (y - my)) / (sx * sy)
  my2 <- mean(y^2)
  c_slope <- (mxy - mx * my) / (my2 - my^2)
  list(A = a_slope, B = b_second, S = sum(resid^2), r = r, C = c_slope,
       slope_product = a_slope * c_slope)
}

#' Density of the sum of two independent variables on grids
#'
#' Numeric convolution integral of rho_x and rho_y at z, and the
#' closed Gaussian special case N(0, sx^2 + sy^2). Mirrors morie.fn
#' ...6e64-6e70.
#'
#' @param grid_x,density_x,grid_y,density_y the two densities.
#' @param z evaluation point.
#' @param sigma_x,sigma_y optional Gaussian sigmas for the closed form.
#' @return list(density, gaussian_closed_form).
#' @references Morin (2016), eqs. (6.64)-(6.70).
#' @examples
#' g <- seq(-10, 10, length.out = 801)
#' morie_sum_density(g, stats::dnorm(g), g, stats::dnorm(g, sd = 2), 1)$density
#' @export
morie_sum_density <- function(grid_x, density_x, grid_y, density_y, z,
                              sigma_x = NULL, sigma_y = NULL) {
  gx <- as.numeric(grid_x)
  dx <- as.numeric(density_x)
  gy <- as.numeric(grid_y)
  dy <- as.numeric(density_y)
  vals <- dx * stats::approx(gy, dy, xout = z - gx, yleft = 0,
                             yright = 0)$y
  dens <- sum((vals[-1] + vals[-length(vals)]) / 2 * diff(gx))
  closed <- NULL
  if (!is.null(sigma_x) && !is.null(sigma_y)) {
    closed <- stats::dnorm(z, sd = sqrt(sigma_x^2 + sigma_y^2))
  }
  list(density = dens, gaussian_closed_form = closed)
}

#' The (1 + a)^n approximation ladder
#'
#' e^(na) (valid na^2 << 1), e^(na) e^(-na^2/2) (valid na^3 << 1),
#' and the difference quotient ((x+d)^n - x^n)/d -> n x^(n-1).
#' Mirrors morie.fn ch7 appendix modules.
#'
#' @param a perturbation, > -1.
#' @param n exponent.
#' @param order 1 or 2.
#' @param x,delta optional difference-quotient inputs.
#' @param power exponent for the difference quotient.
#' @return list: exact, approx, validity, quotient, derivative.
#' @references Morin (2016), Appendix C, eqs. (7.14), (7.21),
#'   (7.23)-(7.24), (7.31)-(7.35).
#' @examples
#' morie_approx_ladder(-1 / 365, 23)$approx
#' @export
morie_approx_ladder <- function(a, n, order = 1, x = NULL, delta = NULL,
                                power = 2) {
  if (a <= -1) stop("need a > -1.", call. = FALSE)
  exact <- (1 + a)^n
  if (order == 1) {
    approx <- exp(n * a)
    validity <- abs(n * a^2)
  } else if (order == 2) {
    approx <- exp(n * a - n * a^2 / 2)
    validity <- abs(n * a^3)
  } else {
    stop("order must be 1 or 2.", call. = FALSE)
  }
  quotient <- NULL
  derivative <- NULL
  if (!is.null(x) && !is.null(delta)) {
    if (delta == 0) stop("delta must be nonzero.", call. = FALSE)
    quotient <- ((x + delta)^power - x^power) / delta
    derivative <- power * x^(power - 1)
  }
  list(exact = exact, approx = approx, validity = validity,
       quotient = quotient, derivative = derivative)
}
