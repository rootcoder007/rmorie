# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P7: the law of crime concentration and crime-free places.
#
# Every identity here is a machine-checked theorem in
# research/lean/P7Concentration.lean (Lean 4 + Mathlib, 0 sorry, standard
# axioms only):
#
#   Research.P7.gini_zero_decomposition   G(all places) = z + (1 - z) * G(places with any crime),
#                                         z = share of places with zero crime
#   Research.P7.poisson_zero_prob         P(Poisson(mu) = 0) = exp(-mu)
#
# The first is exact algebra on any count vector; the second is the null
# that says how many crime-free places a uniform Poisson process would
# produce at the observed mean count. Whether counts are Poisson, and at
# what unit, is the analyst's modelling decision.

#' Gini coefficient of a count vector
#'
#' Mean-absolute-difference form \eqn{\sum_i\sum_j |x_i - x_j| / (2 n^2 \bar x)}.
#'
#' @param x Non-negative counts; at least one positive.
#' @return The Gini coefficient in [0, 1).
#' @examples
#' morie_concentration_gini(c(0, 0, 0, 1, 9))
#' @export
morie_concentration_gini <- function(x) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0) || sum(x) <= 0) {
    stop("x must be non-negative counts with a positive total", call. = FALSE)
  }
  n <- length(x)
  xs <- sort(as.numeric(x))
  # O(n log n) form of the double sum
  2 * sum(seq_len(n) * xs) / (n * sum(xs)) - (n + 1) / n
}

#' Decompose crime concentration into crime-free places and the rest
#'
#' Splits the Gini of all places exactly into the zero share \eqn{z} and
#' the Gini among places with any crime: \eqn{G = z + (1-z)G_+}
#' (\code{Research.P7.gini_zero_decomposition}). Reports, next to it, the
#' zero share a uniform Poisson null would produce at the observed mean
#' count per place, \eqn{e^{-\mu}} (\code{Research.P7.poisson_zero_prob}),
#' and the Gini the null would imply if the positive places were as
#' concentrated as observed. The gap between the observed zero share and
#' \eqn{e^{-\mu}} is the part of "concentration" that is genuinely more
#' crime-free places than chance; the gap in \eqn{G_+} is concentration
#' among places that do see crime.
#'
#' @param x Non-negative counts per place; at least one positive.
#' @return A list with \code{n}, \code{mean_count}, \code{zero_share},
#'   \code{gini_all}, \code{gini_positive}, \code{identity_check}
#'   (\code{gini_all - (zero_share + (1 - zero_share) * gini_positive)},
#'   zero up to rounding), \code{null_zero_share} (\eqn{e^{-\mu}}),
#'   \code{null_gini_same_positive} (\eqn{e^{-\mu} + (1 - e^{-\mu}) G_+}),
#'   \code{excess_zero_share} and \code{theorems}.
#' @examples
#' set.seed(1)
#' x <- rpois(1000, 0.4)                       # uniform rates: any "law" here is chance
#' morie_concentration_decompose(x)[c("zero_share", "null_zero_share", "gini_all")]
#' y <- rpois(1000, rgamma(1000, 0.3, 0.3 / 0.4)) # heterogeneous rates
#' morie_concentration_decompose(y)[c("zero_share", "null_zero_share", "gini_positive")]
#' @export
morie_concentration_decompose <- function(x) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0) || sum(x) <= 0) {
    stop("x must be non-negative counts with a positive total", call. = FALSE)
  }
  n <- length(x)
  z <- mean(x == 0)
  g_all <- morie_concentration_gini(x)
  g_pos <- if (sum(x > 0) > 1) morie_concentration_gini(x[x > 0]) else 0
  mu <- mean(x)
  z_null <- exp(-mu)
  list(
    n = n, mean_count = mu, zero_share = z,
    gini_all = g_all, gini_positive = g_pos,
    identity_check = g_all - (z + (1 - z) * g_pos),
    null_zero_share = z_null,
    null_gini_same_positive = z_null + (1 - z_null) * g_pos,
    excess_zero_share = z - z_null,
    theorems = c("Research.P7.gini_zero_decomposition", "Research.P7.poisson_zero_prob",
                 "Research.P7.Mixture.mixture_zero_ge_exp_neg_mean")
  )
}

#' Dispersion of place counts against the Poisson null and its mixtures
#'
#' For any finite mixture of Poisson counts (places with unequal
#' intensities) the variance is at least the mean, the excess being the
#' variance of the intensity, with equality exactly when the intensity is
#' constant on the support (\code{Research.P7.Mixture.variance_eq},
#' \code{mixture_var_ge_mean}, \code{mixture_var_eq_mean_iff}); and the
#' zero share is at least \eqn{e^{-\mu}} (\code{mixture_zero_ge_exp_neg_mean}).
#' So a dispersion index above one and a zero share above the Poisson null
#' are implied by any heterogeneity of exposure and cannot by themselves
#' separate a "criminology of place" from unequal exposure. This function
#' reports the sample dispersion index, the implied intensity variance
#' under the mixture reading, and the zero-share gap.
#'
#' @param x Non-negative counts per place; at least one positive.
#' @return A list with \code{mean_count}, \code{variance} (sample, denominator
#'   \code{n - 1}), \code{dispersion_index} (variance over mean),
#'   \code{implied_intensity_variance} (variance minus mean, floored at 0),
#'   \code{implied_intensity_sd}, \code{zero_share}, \code{null_zero_share}
#'   (\eqn{e^{-\mu}}, a lower bound for every mixture with the same mean),
#'   \code{zero_share_gap} and \code{theorems}.
#' @examples
#' set.seed(1)
#' x <- rpois(2000, rgamma(2000, shape = 2, rate = 5))   # heterogeneous places
#' unlist(morie_concentration_dispersion(x)[c("dispersion_index", "implied_intensity_sd", "zero_share_gap")])
#' @export
morie_concentration_dispersion <- function(x) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0) || sum(x) <= 0 || length(x) < 2L) {
    stop("x must be at least two non-negative counts with a positive total", call. = FALSE)
  }
  mu <- mean(x)
  v <- stats::var(x)
  z <- mean(x == 0)
  z_null <- exp(-mu)
  list(
    mean_count = mu, variance = v, dispersion_index = v / mu,
    implied_intensity_variance = max(v - mu, 0),
    implied_intensity_sd = sqrt(max(v - mu, 0)),
    zero_share = z, null_zero_share = z_null, zero_share_gap = z - z_null,
    theorems = c("Research.P7.Mixture.variance_eq", "Research.P7.Mixture.mixture_var_ge_mean",
                 "Research.P7.Mixture.mixture_var_eq_mean_iff",
                 "Research.P7.Mixture.mixture_zero_ge_exp_neg_mean")
  )
}


#' Growth of the number of distinct places under preferential allocation
#'
#' Under Polya (Dirichlet-process) allocation with concentration \eqn{M}, the
#' \eqn{i}-th event lands on a new place with probability \eqn{M/(M+i-1)}, so
#' the expected number of distinct places after \eqn{n} events is
#' \eqn{M\sum_{i<n} 1/(M+i)}, squeezed between \eqn{M\log((M+n)/M)} and
#' \eqn{1 + M\log((M+n-1)/M)} (\code{Research.P7.expectedDistinct_bounds}):
#' logarithmic growth. A series of repeat victimisations whose count of
#' distinct addresses grows polynomially is not a concentration-only
#' (Polya) process. The function reports the observed growth curve, the
#' concentration that matches the final count, the proved envelope for that
#' concentration, and a log-log slope of the observed curve as the
#' discriminating diagnostic.
#'
#' @param place Vector of place identifiers in event order.
#' @return A list with \code{n}, \code{distinct} (observed count after each
#'   event), \code{M_hat} (concentration matching the final count),
#'   \code{expected} (Polya expectation curve at \code{M_hat}), \code{lower},
#'   \code{upper} (the proved envelope), \code{loglog_slope} (slope of
#'   log distinct on log n over the second half of the series; near 0 for
#'   logarithmic growth, near a positive constant for a power law) and
#'   \code{theorems}.
#' @examples
#' set.seed(1)
#' place <- integer(500); k <- 0
#' for (i in seq_along(place)) { if (runif(1) < 5 / (5 + i - 1)) { k <- k + 1; place[i] <- k } else place[i] <- place[sample(i - 1, 1)] }
#' g <- morie_concentration_distinct_growth(place)
#' c(M_hat = g$M_hat, slope = g$loglog_slope)
#' @export
morie_concentration_distinct_growth <- function(place) {
  n <- length(place)
  if (n < 2) stop("need at least two events", call. = FALSE)
  distinct <- cumsum(!duplicated(place))
  expect_fun <- function(M, n) M * sum(1 / (M + 0:(n - 1)))
  K <- distinct[n]
  # the root to machine precision, so the R and Python arms agree to rounding
  M_hat <- if (K >= n) Inf else if (K <= 1) 0 else {
    stats::uniroot(function(M) expect_fun(M, n) - K, c(1e-8, 1e8), tol = .Machine$double.eps)$root
  }
  expected <- if (is.finite(M_hat) && M_hat > 0) M_hat * cumsum(1 / (M_hat + 0:(n - 1))) else rep(NA_real_, n)
  idx <- seq_len(n)
  lower <- if (is.finite(M_hat) && M_hat > 0) M_hat * log((M_hat + idx) / M_hat) else rep(NA_real_, n)
  upper <- if (is.finite(M_hat) && M_hat > 0) 1 + M_hat * log((M_hat + idx - 1) / M_hat) else rep(NA_real_, n)
  half <- idx[idx >= n / 2]
  slope <- if (length(half) > 2 && all(distinct[half] > 0)) unname(stats::coef(stats::lm(log(distinct[half]) ~ log(half)))[2]) else NA_real_
  list(n = n, distinct = distinct, M_hat = M_hat, expected = expected, lower = lower, upper = upper,
       loglog_slope = slope,
       theorems = c("Research.P7.log_le_S", "Research.P7.S_le_log", "Research.P7.expectedDistinct_bounds"))
}
