# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P10: near-repeat contagion as a branching process
# (research/lean/P10Contagion.lean).
#
#   Research.P10.generation_mean                E Z_{k+1} = n E Z_k, so E Z_k = n^k
#   Research.P10.cluster_size_of_lt_one         0 <= n < 1: sum_k n^k = 1/(1-n)
#   Research.P10.stationary_rate                background mu: mean rate mu/(1-n)
#   Research.P10.cluster_size_diverges_of_ge_one n >= 1: partial sums unbounded
#   Research.P10.endogeneity_share              share of triggered events = n

#' Branching-ratio arithmetic of a self-exciting (Hawkes) crime process
#'
#' Every recorded event triggers on average \code{n} further events (the
#' branching ratio, the integral of the triggering kernel). The offspring
#' of one background event form a Galton-Watson tree with mean offspring
#' \code{n}: generation \code{k} has expected size \eqn{n^k}
#' (\code{Research.P10.generation_mean}); for \eqn{n < 1} the expected
#' cluster size is \eqn{1/(1-n)} and the stationary mean rate is
#' \eqn{\mu/(1-n)} for background rate \eqn{\mu}
#' (\code{cluster_size_of_lt_one}, \code{stationary_rate}); the share of
#' events that are triggered rather than background is exactly \code{n}
#' (\code{endogeneity_share}). For \eqn{n \ge 1} the expected cluster size
#' is unbounded (\code{cluster_size_diverges_of_ge_one}): a fitted branching
#' ratio at or above one is a model that predicts unbounded crime, not a
#' forecast, and is the first thing to check in any near-repeat fit.
#'
#' @param n Branching ratio, non-negative.
#' @param mu Background rate (events per unit time), positive.
#' @param generations How many generation means to report.
#' @return A list with \code{n}, \code{subcritical}, \code{generation_means},
#'   \code{expected_cluster_size} (\code{Inf} when \eqn{n \ge 1}),
#'   \code{stationary_rate}, \code{endogeneity_share} and \code{theorems}.
#' @examples
#' morie_contagion_branching(n = 0.4, mu = 2)
#' morie_contagion_branching(n = 1.1, mu = 2)$expected_cluster_size   # Inf
#' @export
morie_contagion_branching <- function(n, mu = 1, generations = 10L) {
  if (length(n) != 1L || is.na(n) || n < 0) stop("n must be a single non-negative number", call. = FALSE)
  if (length(mu) != 1L || is.na(mu) || mu <= 0) stop("mu must be a single positive number", call. = FALSE)
  sub <- n < 1
  list(
    n = n, subcritical = sub,
    generation_means = n ^ (0:(generations - 1L)),
    expected_cluster_size = if (sub) 1 / (1 - n) else Inf,
    stationary_rate = if (sub) mu / (1 - n) else Inf,
    endogeneity_share = if (sub) n else NA_real_,
    theorems = c("Research.P10.generation_mean", "Research.P10.cluster_size_of_lt_one",
                 "Research.P10.stationary_rate", "Research.P10.cluster_size_diverges_of_ge_one",
                 "Research.P10.endogeneity_share")
  )
}

#' Extinction probability of a near-repeat chain
#'
#' With offspring probabilities \code{p[1], p[2], ...} for 0, 1, 2, ... triggered
#' events, the chain started by one event dies out with probability equal to the
#' smallest fixed point of the generating function \eqn{f(s) = \sum_k p_k s^k} in
#' \eqn{[0, 1]}, reached as the limit of \eqn{s_0 = 0}, \eqn{s_{n+1} = f(s_n)}
#' (\code{Research.P10.iter_tendsto}, \code{extinction_fixed},
#' \code{extinction_le_fixed}). When the mean offspring \eqn{m = \sum_k k p_k}
#' is below one the only fixed point is one and extinction is certain
#' (\code{subcritical_extinction_one}); above one a fixed point below one
#' exists and extinction has probability strictly less than one
#' (\code{supercritical_extinction_lt_one}). The branching ratio of
#' \code{\link{morie_contagion_branching}} is this \eqn{m}; the function gives
#' the probability that any particular chain ends, which the ratio alone does not.
#' @param p Offspring probabilities for 0, 1, 2, ... children; non-negative,
#'   summing to one.
#' @param tol Stop iterating when consecutive iterates differ by less than this.
#' @param max_iter Iteration cap.
#' @return A list with \code{mean_offspring}, \code{regime} (\code{"subcritical"},
#'   \code{"critical"} or \code{"supercritical"}), \code{extinction} (the smallest
#'   fixed point), \code{iterates} (the monotone sequence), \code{survival}
#'   (\code{1 - extinction}), \code{fixed_point_check} (\eqn{f(q) - q}) and
#'   \code{theorems}.
#' @examples
#' morie_contagion_extinction(c(0.3, 0.3, 0.4))   # mean 1.1: survives with positive probability
#' morie_contagion_extinction(c(0.5, 0.3, 0.2))   # mean 0.7: dies out
#' @export
morie_contagion_extinction <- function(p, tol = 1e-14, max_iter = 100000L) {
  if (!is.numeric(p) || length(p) == 0L || anyNA(p) || any(p < 0) || abs(sum(p) - 1) > 1e-10) stop("p must be non-negative probabilities summing to one", call. = FALSE)
  k <- seq_along(p) - 1
  f <- function(s) sum(p * s^k)
  m <- sum(k * p)
  s <- 0
  iterates <- numeric(0)
  for (i in seq_len(max_iter)) {
    s_new <- f(s)
    iterates <- c(iterates, s_new)
    if (abs(s_new - s) < tol) { s <- s_new; break }
    s <- s_new
  }
  regime <- if (m < 1) "subcritical" else if (m > 1) "supercritical" else "critical"
  list(mean_offspring = m, regime = regime, extinction = s, survival = 1 - s,
       iterates = iterates, fixed_point_check = f(s) - s,
       theorems = c("Research.P10.iter_mono", "Research.P10.iter_tendsto", "Research.P10.extinction_fixed",
                    "Research.P10.extinction_le_fixed", "Research.P10.subcritical_extinction_one",
                    "Research.P10.supercritical_extinction_lt_one"))
}
