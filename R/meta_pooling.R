# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P13: pooling evaluations across sites or studies
# (research/lean/P13Meta.lean; Weisburd & Britt ch. 11; DerSimonian & Laird 1986).
#
#   Research.P13.truncation_bias              E[max(X,0)] = E[X] + E[max(-X,0)] >= E[X]
#   Research.P13.pos_part_pos                 one outcome of positive probability with X > 0  =>  E[max(X,0)] > 0
#   Research.P13.dl_biased_under_homogeneity  E[X] = 0 and such an outcome  =>  E[max(X,0)] > E[X]
#   Research.P13.tauDL_nonneg / tauDL_eq_zero_iff   tau2_DL >= 0; = 0 iff Q <= k - 1 (c > 0)
#   Research.P13.re_var_ge / re_var_eq_iff    1/sum 1/(v_i + t) >= 1/sum 1/v_i, equality iff t = 0

#' Fixed-effect and DerSimonian-Laird random-effects pooling, with what the truncation does
#'
#' Pools \code{k} site or study estimates with known variances. The
#' DerSimonian-Laird between-site variance is a truncation,
#' \eqn{\hat\tau^2 = \max(0, (Q - (k-1))/c)}, non-negative and zero exactly
#' when \eqn{Q \le k - 1} (\code{Research.P13.tauDL_nonneg},
#' \code{tauDL_eq_zero_iff}). For any \eqn{\tau^2 \ge 0} the random-effects
#' variance of the pooled estimate is at least the fixed-effect variance, with
#' equality only at \eqn{\tau^2 = 0} (\code{re_var_ge}, \code{re_var_eq_iff}):
#' the random-effects interval is never narrower, and the two methods agree
#' in a finite sample only when the estimate is truncated. Because
#' \eqn{E[\max(X, 0)] > E[X]} whenever \eqn{X > 0} has positive probability
#' (\code{truncation_bias}, \code{pos_part_pos}), the estimator has positive
#' expectation under homogeneity, where \eqn{E[Q] = k - 1}
#' (\code{dl_biased_under_homogeneity}); see
#' \code{\link{morie_meta_dl_bias}} for the size of that bias.
#' @param estimates Site estimates (e.g. hot-spots effects), one per site.
#' @param variances Their sampling variances, positive.
#' @param level Confidence level for the two intervals.
#' @return A list with \code{k}, \code{fixed} and \code{random} (each with
#'   \code{estimate}, \code{variance}, \code{se}, \code{ci}), \code{Q},
#'   \code{df} (\eqn{k-1}), \code{c}, \code{tau2} (DerSimonian-Laird),
#'   \code{truncated} (whether \eqn{Q \le k-1} forced \eqn{\hat\tau^2 = 0}),
#'   \code{variance_ratio} (random over fixed, at least 1), \code{weights}
#'   (fixed and random, normalised) and \code{theorems}.
#' @examples
#' est <- c(-0.25, -0.10, -0.40, 0.05, -0.30)
#' v <- c(0.010, 0.020, 0.015, 0.030, 0.012)
#' m <- morie_meta_random_effects(est, v)
#' c(fixed = m$fixed$estimate, random = m$random$estimate, tau2 = m$tau2, ratio = m$variance_ratio)
#' @export
morie_meta_random_effects <- function(estimates, variances, level = 0.95) {
  k <- length(estimates)
  if (length(variances) != k) stop("estimates and variances must have equal length", call. = FALSE)
  if (k < 2L) stop("need at least two sites", call. = FALSE)
  if (anyNA(estimates) || anyNA(variances) || any(variances <= 0)) stop("variances must be positive and nothing missing", call. = FALSE)
  if (level <= 0 || level >= 1) stop("level must lie in (0, 1)", call. = FALSE)
  w <- 1 / variances
  theta_fe <- sum(w * estimates) / sum(w)
  q <- sum(w * (estimates - theta_fe)^2)
  c_dl <- sum(w) - sum(w^2) / sum(w)
  tau2 <- max(0, (q - (k - 1)) / c_dl)
  w_re <- 1 / (variances + tau2)
  theta_re <- sum(w_re * estimates) / sum(w_re)
  var_fe <- 1 / sum(w)
  var_re <- 1 / sum(w_re)
  z <- stats::qnorm(1 - (1 - level) / 2)
  ci <- function(est, v) c(lower = est - z * sqrt(v), upper = est + z * sqrt(v))
  list(
    k = k,
    fixed = list(estimate = theta_fe, variance = var_fe, se = sqrt(var_fe), ci = ci(theta_fe, var_fe)),
    random = list(estimate = theta_re, variance = var_re, se = sqrt(var_re), ci = ci(theta_re, var_re)),
    Q = q, df = k - 1, c = c_dl, tau2 = tau2, truncated = q <= k - 1,
    variance_ratio = var_re / var_fe,
    weights = list(fixed = w / sum(w), random = w_re / sum(w_re)),
    theorems = c("Research.P13.tauDL_nonneg", "Research.P13.tauDL_eq_zero_iff",
                 "Research.P13.re_var_ge", "Research.P13.re_var_eq_iff")
  )
}

#' Size of the DerSimonian-Laird truncation bias under homogeneity
#'
#' Draws \code{n_draws} sets of site estimates from a common effect with the
#' given variances (so the true \eqn{\tau^2} is zero) and reports the mean
#' DerSimonian-Laird \eqn{\hat\tau^2}, the share of draws in which it is
#' positive, and the mean ratio of the random- to the fixed-effect variance.
#' The mean is positive because \eqn{E[\max(X,0)] = E[X] + E[\max(-X,0)]}
#' with \eqn{E[X] = 0} and \eqn{P(X > 0) > 0} (\code{Research.P13.truncation_bias},
#' \code{pos_part_pos}, \code{dl_biased_under_homogeneity}); the estimator
#' cannot average to the truth it is estimating. Draws come from the Philox
#' stream, so the R and Python arms report identical numbers for a seed.
#' @param variances Site sampling variances, positive; at least two.
#' @param n_draws Number of simulated meta-analyses.
#' @param seed Philox seed (non-negative).
#' @return A list with \code{k}, \code{mean_tau2}, \code{share_positive},
#'   \code{mean_variance_ratio}, \code{mean_Q} (close to \eqn{k - 1}),
#'   \code{n_draws} and \code{theorems}.
#' @examples
#' morie_meta_dl_bias(c(0.010, 0.020, 0.015, 0.030, 0.012), n_draws = 500, seed = 1)
#' @export
morie_meta_dl_bias <- function(variances, n_draws = 2000L, seed = 0) {
  k <- length(variances)
  if (k < 2L || anyNA(variances) || any(variances <= 0)) stop("variances must be at least two positive numbers", call. = FALSE)
  n_draws <- as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 1L) stop("n_draws must be a positive integer", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed < 0) stop("seed must be a single non-negative number", call. = FALSE)
  u <- .morie_random_uniform(k * n_draws, seed = seed, stream = 0)
  z <- stats::qnorm(u)
  w <- 1 / variances
  c_dl <- sum(w) - sum(w^2) / sum(w)
  tau2 <- numeric(n_draws)
  q_all <- numeric(n_draws)
  ratio <- numeric(n_draws)
  for (d in seq_len(n_draws)) {
    est <- sqrt(variances) * z[(d - 1L) * k + seq_len(k)]
    theta_fe <- sum(w * est) / sum(w)
    q <- sum(w * (est - theta_fe)^2)
    t2 <- max(0, (q - (k - 1)) / c_dl)
    tau2[d] <- t2
    q_all[d] <- q
    ratio[d] <- (1 / sum(1 / (variances + t2))) / (1 / sum(w))
  }
  list(k = k, mean_tau2 = mean(tau2), share_positive = mean(tau2 > 0),
       mean_variance_ratio = mean(ratio), mean_Q = mean(q_all), n_draws = n_draws,
       theorems = c("Research.P13.truncation_bias", "Research.P13.pos_part_pos",
                    "Research.P13.dl_biased_under_homogeneity"))
}
