.mrn_prob <- function(p, name) {
  if (!(p >= 0 && p <= 1)) stop(name, " must be in [0, 1]")
  p
}

.mrn_int <- function(n, name) {
  if (n != round(n) || n < 0) stop(name, " must be a non-negative integer")
  as.integer(n)
}

.mrn_binom <- function(k, n, p) {
  if (k > n) return(0)
  if (p == 0) return(as.numeric(k == 0))
  if (p == 1) return(as.numeric(k == n))
  if (n <= 1000) return(choose(n, k) * p^k * (1 - p)^(n - k))
  exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) + k * log(p) + (n - k) * log1p(-p))
}

.mrn_pois <- function(k, a) if (a > 0) exp(k * log(a) - a - lgamma(k + 1)) else as.numeric(k == 0)

#' Elementary probability results of Morin (2016)
#'
#' Counting: `permutations_count` (N!, eq. 1.3), `partial_permutations`
#' (N!/(N-n)!, eq. 1.5), `binomial_expansion` (eq. 1.21), `hockey_stick`
#' (eq. 1.29).  Events: `classify_events` (independence and exclusivity,
#' sec. 2.2.3), `conditional_from_joint` (P(B|A) = P(A and B)/P(A), eq.
#' 2.48), `conditional_subset` (eq. 2.49).  Moments: `bernoulli_variance`
#' (pq, eq. 3.22), `binomial_variance` (npq, eq. 3.33).  Distributions:
#' `binomial_pmf_vector` (eq. 4.10), `hypergeometric_pmf` (eq. 4.71),
#' `poisson_small_interval` (eq. 4.18), `poisson_mean_rate` (eq. 4.19),
#' `poisson_zero_series` (eq. 4.53), `density_expectation` (trapezoid rule,
#' eq. 4.55), `poisson_binomial_peak_ratio` (eq. 4.98), the Gaussian
#' approximations `gaussian_approx_2n`, `gaussian_approx_n`,
#' `gaussian_approx_biased` (eqs. 5.13-5.15) and `gaussian_sum_density`
#' (eq. 6.70).
#'
#' @param p,p_a,p_b,p_ab,p_a_and_b Probabilities.
#' @param n,N,K,k Non-negative integers.
#' @param a,b Terms of the binomial, or the Poisson mean `a`.
#' @param tol Tolerance of the event classification.
#' @param grid,density Grid and density values.
#' @param x Deviation from the mean.
#' @param z Value of the sum.
#' @param sigma_x,sigma_y Standard deviations.
#' @param lam Rate.
#' @param t Duration.
#' @param eps Interval length.
#' @param terms Number of series terms.
#' @return Lists with the same components as the Python arm.
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. CreateSpace Independent Publishing.
#' @examples
#' hypergeometric_pmf(2, 20, 7, 5)$probability
#' gaussian_approx_2n(3, 50)$PG
#' @export
bernoulli_variance <- function(p) {
  p <- .mrn_prob(p, "p")
  list(p = p, variance = p * (1 - p))
}

#' @rdname bernoulli_variance
#' @export
binomial_expansion <- function(a, b, n) {
  n <- .mrn_int(n, "n")
  terms <- vapply(0:n, function(k) choose(n, k) * a^(n - k) * b^k, 0)
  direct <- (a + b)^n
  list(terms = terms, sum = sum(terms), direct = direct, max_abs_error = abs(sum(terms) - direct))
}

#' @rdname bernoulli_variance
#' @export
binomial_pmf_vector <- function(n, p) {
  .morie_arg(n, "n")
  n <- .mrn_int(n, "n")
  p <- .mrn_prob(p, "p")
  pmf <- vapply(0:n, function(k) .mrn_binom(k, n, p), 0)
  list(pmf = pmf, total = sum(pmf))
}

#' @rdname bernoulli_variance
#' @export
binomial_variance <- function(n, p) {
  n <- .mrn_int(n, "n")
  p <- .mrn_prob(p, "p")
  list(n = n, p = p, variance = n * p * (1 - p))
}

#' @rdname bernoulli_variance
#' @export
classify_events <- function(p_a, p_b, p_ab, tol = 1e-12) {
  list(independent = abs(p_ab - p_a * p_b) <= tol, exclusive = p_ab <= tol, p_a = p_a, p_b = p_b, p_ab = p_ab)
}

#' @rdname bernoulli_variance
#' @export
conditional_from_joint <- function(p_a_and_b, p_a) {
  if (p_a == 0) stop("P(A) must be positive")
  if (p_a_and_b > p_a + 1e-12) stop("P(A and B) cannot exceed P(A)")
  list(p_a_and_b = p_a_and_b, p_a = p_a, p_b_given_a = p_a_and_b / p_a)
}

#' @rdname bernoulli_variance
#' @export
conditional_subset <- function(p_b, p_a) {
  if (p_a == 0) stop("P(A) must be positive")
  if (p_b > p_a + 1e-12) stop("subset case needs P(B) <= P(A)")
  list(p_b = p_b, p_a = p_a, p_b_given_a = p_b / p_a)
}

#' @rdname bernoulli_variance
#' @export
density_expectation <- function(grid, density) {
  x <- as.numeric(grid)
  f <- x * as.numeric(density)
  m <- length(x)
  if (m < 2 || length(f) != m) stop("grid and density must be equal-length vectors, n >= 2")
  list(expectation = sum((f[-1] + f[-m]) * diff(x)) / 2)
}

#' @rdname bernoulli_variance
#' @export
gaussian_approx_2n <- function(x, n) {
  if (n < 1) stop("n must be >= 1")
  list(x = x, n = n, PG = exp(-x * x / n) / sqrt(pi * n))
}

#' @rdname bernoulli_variance
#' @export
gaussian_approx_n <- function(x, n) {
  if (n < 1) stop("n must be >= 1")
  list(x = x, n = n, PG = exp(-2 * x * x / n) / sqrt(pi * n / 2))
}

#' @rdname bernoulli_variance
#' @export
gaussian_approx_biased <- function(x, n, p) {
  npq <- n * p * (1 - p)
  if (npq <= 0) stop("npq must be > 0")
  list(x = x, n = n, p = p, PG = exp(-x * x / (2 * npq)) / sqrt(2 * pi * npq))
}

#' @rdname bernoulli_variance
#' @export
gaussian_sum_density <- function(z, sigma_x, sigma_y) {
  if (sigma_x <= 0 || sigma_y <= 0) stop("sigmas must be > 0")
  s <- sqrt(sigma_x^2 + sigma_y^2)
  list(density = exp(-z^2 / (2 * s^2)) / sqrt(2 * pi * s^2), sigma_sum = s)
}

#' @rdname bernoulli_variance
#' @export
hockey_stick <- function(n, k) {
  n <- .mrn_int(n, "n")
  k <- .mrn_int(k, "k")
  if (k < 1 || k > n) stop("hockey stick needs 1 <= k <= n")
  s <- sum(choose((k - 1):(n - 1), k - 1))
  list(n = n, k = k, sum = s, binomial = choose(n, k), identity_holds = s == choose(n, k))
}

#' @rdname bernoulli_variance
#' @export
hypergeometric_pmf <- function(k, N, K, n) {
  if (K > N || n > N) stop("need K <= N and n <= N")
  pr <- if (k > min(K, n) || n - k > N - K) 0 else choose(K, k) * choose(N - K, n - k) / choose(N, n)
  list(probability = pr)
}

#' @rdname bernoulli_variance
#' @export
partial_permutations <- function(N, n) {
  N <- .mrn_int(N, "N")
  n <- .mrn_int(n, "n")
  if (n > N) stop("n cannot exceed N")
  list(N = N, n = n, partial_permutations = prod(seq_len(n) + N - n))
}

#' @rdname bernoulli_variance
#' @export
permutations_count <- function(n) {
  .morie_arg(n, "n")
  list(n = .mrn_int(n, "n"), permutations = factorial(n))
}

#' @rdname bernoulli_variance
#' @export
poisson_binomial_peak_ratio <- function(n, p) {
  k <- round(p * n)
  if (k == 0 || k == n) stop("pn must be an interior integer; increase n")
  r <- .mrn_pois(k, p * n) / .mrn_binom(k, n, p)
  list(ratio = r, sqrt_1_minus_p = sqrt(1 - p), abs_error = abs(r - sqrt(1 - p)))
}

#' @rdname bernoulli_variance
#' @export
poisson_mean_rate <- function(lam, t) {
  if (lam < 0 || t < 0) stop("lambda and t must be >= 0")
  list(lambda = lam, t = t, expected_events = lam * t)
}

#' @rdname bernoulli_variance
#' @export
poisson_small_interval <- function(lam, eps) {
  exact <- .mrn_pois(1, lam * eps)
  list(approx = lam * eps, exact = exact, abs_error = abs(lam * eps - exact))
}

#' @rdname bernoulli_variance
#' @export
poisson_zero_series <- function(a, terms = 60) {
  if (a < 0) stop("a must be >= 0")
  j <- seq_len(terms) - 1
  partials <- cumsum((-1)^j * a^j / factorial(j))
  list(partial_sums = partials, e_minus_a = exp(-a), final_error = abs(partials[terms] - exp(-a)))
}
