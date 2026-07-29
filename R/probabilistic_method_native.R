# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The probabilistic method. Mirrors morie.fn.prbmth.
#
# Every bound here is one-sided, which is exactly what makes it
# testable: a bound claimed to hold must never be violated by a
# directly computed probability, and a bound claimed to be useful must
# be below 1. Both are checked, because a bound of 1 holds universally
# and says nothing.
#
# Alon N, Spencer JH (2016), The Probabilistic Method, 4th ed.
# Erdos P (1947); Erdos P, Lovasz L (1975); Chernoff H (1952);
# Azuma K (1967); Hoeffding W (1963).

#' First moment / union bound
#'
#' If the expected number of bad events is below 1, some outcome has
#' none. This proves existence without exhibiting anything, which is
#' both the point and the limitation.
#'
#' @param n_events Number of bad events.
#' @param event_probability Probability of each.
#' @return A list with `expected`, `exists`.
#' @export
morie_union_bound_exists <- function(n_events, event_probability) {
  m <- as.integer(n_events)
  p <- as.numeric(event_probability)
  if (is.na(m) || m < 0L) {
    stop(sprintf("n_events must be non-negative; got %s", n_events),
         call. = FALSE)
  }
  if (p < 0 || p > 1) {
    stop(sprintf("event_probability must lie in [0, 1]; got %s", p),
         call. = FALSE)
  }
  expected <- m * p
  list(expected = expected, exists = expected < 1, n_events = m,
       event_probability = p, n = m, method = "First moment method")
}

#' Erdos's first-moment lower bound on the diagonal Ramsey number
#'
#' Colouring \eqn{K_n} at random, the expected number of monochromatic
#' \eqn{K_k} is \eqn{\binom{n}{k} 2^{1 - \binom{k}{2}}}; below 1 it
#' proves \eqn{R(k,k) > n}.
#'
#' A search that runs to its own ceiling has found the CEILING, not the
#' bound, so `search_capped` is reported and warned about rather than
#' letting the cap read as a result.
#'
#' @param k Clique size, at least 2.
#' @return A list with `bound`, `certifies`, `expected_at_bound`,
#'   `search_capped`, `warnings`.
#' @references Erdos P (1947) \emph{Bull AMS} 53:292-294.
#' @export
morie_first_moment_ramsey <- function(k) {
  k <- as.integer(k)
  if (is.na(k) || k < 2L) {
    stop(sprintf("k must be at least 2; got %s", k), call. = FALSE)
  }
  CAP <- 100000L
  expo <- 1 - choose(k, 2)
  n <- k
  best <- k - 1L
  while (n < CAP) {
    log2e <- (lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)) / log(2) + expo
    if (log2e < 0) { best <- n; n <- n + 1L } else break
  }
  capped <- n >= CAP
  log2_at <- (lgamma(best + 1) - lgamma(k + 1) -
                lgamma(best - k + 1)) / log(2) + expo
  warns <- character(0)
  if (capped) {
    warns <- sprintf(paste(
      "The search reached its ceiling of %d without the expected count",
      "exceeding 1, so the value returned is the CEILING, not the bound.",
      "The true bound is larger."), CAP)
  }
  list(bound = best, certifies = sprintf("R(%d,%d) > %d", k, k, best),
       expected_at_bound = 2^log2_at,
       asymptotic_2_to_k_over_2 = 2^(k / 2),
       search_capped = capped, search_cap = CAP, k = k, n = best,
       warnings = warns, method = "First moment method (Erdos 1947)")
}

#' The alteration method
#'
#' Colour \eqn{K_n} at random, then DELETE one vertex from each
#' monochromatic \eqn{K_k}. What remains is clean, and its expected
#' size is \eqn{n - \binom{n}{k} 2^{1 - \binom{k}{2}}}.
#'
#' Asymptotically this gains a factor of 2 in the exponent, but it is
#' NOT uniformly better than the plain union bound: at the union
#' bound's own \eqn{n} the expected count is below 1, so the surviving
#' set exceeds \eqn{n - 1}, which after the floor can be \eqn{n - 1}.
#' Measured, the union bound wins by one at \eqn{k = 4} and \eqn{k = 5}
#' and ties at \eqn{k = 6}. Both are valid, so `best_bound` reports the
#' maximum and `improvement` is signed.
#'
#' @param k Clique size, at least 2.
#' @return A list with `bound`, `first_moment_bound`, `improvement`,
#'   `best_bound`, `optimal_n`, `search_capped`.
#' @references Alon N, Spencer JH (2016), Ch 3.
#' @export
morie_alteration_ramsey <- function(k) {
  k <- as.integer(k)
  if (is.na(k) || k < 2L) {
    stop(sprintf("k must be at least 2; got %s", k), call. = FALSE)
  }
  ACAP <- 200000L
  expo <- 1 - choose(k, 2)
  best_n <- k; best_val <- 0
  reached_end <- TRUE
  n <- k
  while (n < ACAP) {
    log2e <- (lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)) / log(2) + expo
    if (log2e > 60) { reached_end <- FALSE; break }
    val <- n - 2^log2e
    if (val > best_val) { best_val <- val; best_n <- n }
    else if (n > best_n + 5000L) { reached_end <- FALSE; break }
    n <- n + 1L
  }
  bound <- floor(best_val)
  fm <- morie_first_moment_ramsey(k)$bound
  list(bound = bound, certifies = sprintf("R(%d,%d) > %g", k, k, bound),
       first_moment_bound = fm, improvement = bound - fm,
       best_bound = max(bound, fm), optimal_n = best_n,
       expected_survivors = best_val, search_capped = reached_end,
       search_cap = ACAP, k = k, n = bound,
       method = "Alteration method (Alon and Spencer, Ch 3)")
}

#' The symmetric Lovasz Local Lemma
#'
#' If each bad event has probability at most \eqn{p} and is independent
#' of all but at most \eqn{d} others, and \eqn{e p (d+1) \le 1}, then
#' with positive probability none occurs.
#'
#' What makes it remarkable is that the union bound is useless once
#' \eqn{mp \ge 1} however small the dependency, while the Local Lemma
#' does not care how many events there are at all -- only how entangled
#' each one is.
#'
#' @param p Event probability.
#' @param d Dependency degree.
#' @return A list with `condition_value`, `applies`, `max_degree_at_p`,
#'   `max_probability_at_d`.
#' @references Erdos P, Lovasz L (1975).
#' @export
morie_lovasz_local_lemma <- function(p, d) {
  p <- as.numeric(p); d <- as.integer(d)
  if (p < 0 || p > 1) {
    stop(sprintf("p must lie in [0, 1]; got %s", p), call. = FALSE)
  }
  if (is.na(d) || d < 0L) {
    stop(sprintf("d must be non-negative; got %s", d), call. = FALSE)
  }
  lhs <- exp(1) * p * (d + 1)
  list(condition_value = lhs, applies = lhs <= 1, p = p, d = d,
       max_degree_at_p = if (p > 0) floor(1 / (exp(1) * p) - 1) else NA,
       max_probability_at_d = 1 / (exp(1) * (d + 1)),
       slack = 1 - lhs, n = d,
       method = "Lovasz Local Lemma (Erdos and Lovasz 1975)")
}

#' Chernoff bounds on a binomial tail
#'
#' The exact tail is computed alongside, so the test is that the bound
#' HOLDS -- and separately that it is not vacuous, since a bound of 1 is
#' always true and never useful.
#'
#' @param n,p Binomial parameters.
#' @param t Threshold.
#' @param tail "upper" or "lower".
#' @return A list with `bound`, `exact_tail`, `holds`, `vacuous`,
#'   `warnings`.
#' @references Chernoff H (1952) \emph{Ann Math Stat} 23(4):493-507.
#' @export
morie_chernoff_bound <- function(n, p, t, tail = c("upper", "lower")) {
  tail <- match.arg(tail)
  n <- as.integer(n); p <- as.numeric(p); t <- as.numeric(t)
  if (is.na(n) || n < 1L) {
    stop(sprintf("n must be positive; got %s", n), call. = FALSE)
  }
  if (p < 0 || p > 1) {
    stop(sprintf("p must lie in [0, 1]; got %s", p), call. = FALSE)
  }
  mu <- n * p
  if (mu <= 0) {
    stop("np must be positive for a multiplicative bound.", call. = FALSE)
  }
  if (tail == "upper") {
    delta <- t / mu - 1
    bound <- if (delta <= 0) 1 else
      exp(mu * (delta - (1 + delta) * log1p(delta)))
    lo <- ceiling(t)
    exact <- if (lo > n) 0 else sum(stats::dbinom(seq.int(lo, n), n, p))
  } else {
    delta <- 1 - t / mu
    bound <- if (delta <= 0) 1 else exp(-mu * delta * delta / 2)
    hi <- floor(t)
    exact <- if (hi < 0) 0 else sum(stats::dbinom(seq.int(0, hi), n, p))
  }
  bound <- min(bound, 1)
  holds <- bound >= exact - 1e-12
  vacuous <- bound >= 1 - 1e-12
  warns <- character(0)
  if (!holds) {
    warns <- c(warns, sprintf(paste(
      "The bound (%.6g) is below the exact tail (%.6g), which is",
      "impossible."), bound, exact))
  }
  if (vacuous) {
    warns <- c(warns, paste(
      "The bound is 1, which is true of every probability and therefore says",
      "nothing."))
  }
  list(bound = bound, exact_tail = exact, holds = holds, vacuous = vacuous,
       slack = bound - exact, mu = mu, delta = delta, tail = tail, n = n,
       warnings = warns, method = "Chernoff bound (Chernoff 1952)")
}

#' Azuma-Hoeffding for a martingale with bounded differences
#'
#' \eqn{P(|X_n - X_0| \ge t) \le 2\exp(-t^2 / (2 n c^2))}. It assumes
#' nothing beyond the step size, which is why it applies to processes
#' with no independence at all -- and why it is conservative for those
#' that do.
#'
#' @param n Steps.
#' @param c Bound on each difference.
#' @param t Deviation.
#' @return A list with `bound`, `typical_deviation`, `deviations_out`.
#' @references Azuma K (1967) \emph{Tohoku Math J} 19(3):357-367.
#' @export
morie_azuma_bound <- function(n, c, t) {
  n <- as.integer(n); c <- as.numeric(c); t <- as.numeric(t)
  if (is.na(n) || n < 1L) {
    stop(sprintf("n must be positive; got %s", n), call. = FALSE)
  }
  if (c <= 0) stop(sprintf("c must be positive; got %s", c), call. = FALSE)
  if (t < 0) stop(sprintf("t must be non-negative; got %s", t), call. = FALSE)
  bound <- min(2 * exp(-t * t / (2 * n * c * c)), 1)
  list(bound = bound, vacuous = bound >= 1 - 1e-12,
       typical_deviation = c * sqrt(n), deviations_out = t / (c * sqrt(n)),
       n = n, method = "Azuma-Hoeffding inequality (Azuma 1967)")
}

#' The second moment method
#'
#' By Chebyshev, \eqn{P(X = 0) \le \mathrm{Var}(X)/E\[X\]^2}. This is the
#' companion to the first moment: together they make threshold results
#' sharp, since the first shows a property vanishes below the threshold
#' and the second shows it appears above. Neither alone establishes one.
#'
#' @param expectation,variance Moments of the count.
#' @return A list with `p_zero_bound`, `variance_ratio`, `positive_whp`.
#' @export
morie_second_moment_threshold <- function(expectation, variance) {
  e <- as.numeric(expectation); v <- as.numeric(variance)
  if (v < 0) {
    stop(sprintf("variance must be non-negative; got %s", v), call. = FALSE)
  }
  if (e <= 0) {
    stop(sprintf("expectation must be positive; got %s", e), call. = FALSE)
  }
  ratio <- v / (e * e)
  list(p_zero_bound = min(ratio, 1), variance_ratio = ratio,
       expectation = e, variance = v, positive_whp = ratio < 0.5,
       vacuous = min(ratio, 1) >= 1 - 1e-12, n = 1,
       method = "Second moment method (Chebyshev)")
}
