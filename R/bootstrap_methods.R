# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie - Multi-domain Open Research and Inferential Estimation
# Copyright (C) 2026 Vansh Singh Ruhela and morie contributors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.

# ---------------------------------------------------------------------------
# Bootstrap & resampling inference  (R port of src/morie/bootstrap_methods.py)
# ---------------------------------------------------------------------------
# Provides nonparametric / parametric / wild / block bootstrap, jackknife,
# permutation tests, subsampling, the .632/.632+ estimator, and k-fold CV.
# All functions take a vector or matrix `data` and a `statistic` function
# (statistic(data) -> numeric scalar).  Results are returned as plain
# `list`s with a class attribute so rmorie::print methods can dispatch.

# ---- Result container constructors ----------------------------------------

.new_bootstrap_result <- function(estimate, se, ci_lower, ci_upper, bias,
                                  n_boot, method, ci_method,
                                  boot_distribution, original_estimate,
                                  acceleration = 0) {
  structure(
    list(estimate = estimate, se = se,
         ci_lower = ci_lower, ci_upper = ci_upper, bias = bias,
         n_boot = n_boot, method = method, ci_method = ci_method,
         boot_distribution = boot_distribution,
         original_estimate = original_estimate,
         acceleration = acceleration),
    class = c("morie_bootstrap_result", "list")
  )
}

.new_jackknife_result <- function(estimate, se, ci_lower, ci_upper, bias,
                                  n, jackknife_estimates, pseudovalues,
                                  influence_values) {
  structure(
    list(estimate = estimate, se = se,
         ci_lower = ci_lower, ci_upper = ci_upper, bias = bias,
         n = n, jackknife_estimates = jackknife_estimates,
         pseudovalues = pseudovalues,
         influence_values = influence_values),
    class = c("morie_jackknife_result", "list")
  )
}

.new_permutation_test_result <- function(observed_statistic, p_value,
                                         null_distribution, n_permutations,
                                         alternative,
                                         ci_lower = NA_real_,
                                         ci_upper = NA_real_) {
  structure(
    list(observed_statistic = observed_statistic, p_value = p_value,
         null_distribution = null_distribution,
         n_permutations = n_permutations,
         alternative = alternative,
         ci_lower = ci_lower, ci_upper = ci_upper),
    class = c("morie_permutation_test_result", "list")
  )
}

.new_cv_result <- function(scores, mean_score, se_score, ci_lower, ci_upper,
                           n_folds, metric, fold_sizes) {
  structure(
    list(scores = scores, mean_score = mean_score, se_score = se_score,
         ci_lower = ci_lower, ci_upper = ci_upper,
         n_folds = n_folds, metric = metric, fold_sizes = fold_sizes),
    class = c("morie_cv_result", "list")
  )
}

# Helper: percentile-of-vector (matches numpy.percentile linear interp).
.pct <- function(x, p) unname(stats::quantile(x, probs = p / 100,
                                              names = FALSE, type = 7))

# Helper: subset rows of a vector or matrix.
.idx <- function(data, idx) {
  if (is.matrix(data) || is.data.frame(data)) data[idx, , drop = FALSE]
  else data[idx]
}

.nrow_like <- function(data) {
  if (is.matrix(data) || is.data.frame(data)) nrow(data) else length(data)
}

#' Nonparametric bootstrap inference
#'
#' Resamples observations with replacement and computes confidence
#' intervals via the percentile, normal, basic, BCa, or studentized
#' method.  Optionally supports stratified or cluster resampling.
#'
#' @param data A numeric vector or matrix of observations.
#' @param statistic Function of one argument that returns a scalar.
#' @param n_boot Number of bootstrap replicates (default 2000).
#' @param ci_level Confidence level (default 0.95).
#' @param ci_method One of \code{"percentile"}, \code{"normal"},
#'   \code{"basic"}, \code{"bca"}, \code{"studentized"}.
#' @param seed Random seed.
#' @param stratify Optional vector of stratum labels (length n).
#' @param cluster Optional vector of cluster labels (length n).
#'
#' @return A \code{morie_bootstrap_result} list.
#' @export
bootstrap <- function(data, statistic, n_boot = 2000L, ci_level = 0.95,
                      ci_method = "bca", seed = 42L,
                      stratify = NULL, cluster = NULL) {
  set.seed(seed)
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))
  boot_stats <- numeric(n_boot)

  if (!is.null(cluster)) {
    cluster <- as.vector(cluster)
    uniq <- unique(cluster)
    nc <- length(uniq)
    for (b in seq_len(n_boot)) {
      samp <- sample(uniq, size = nc, replace = TRUE)
      idx <- unlist(lapply(samp, function(c) which(cluster == c)))
      boot_stats[b] <- as.numeric(statistic(.idx(data, idx)))
    }
  } else if (!is.null(stratify)) {
    stratify <- as.vector(stratify)
    strata <- unique(stratify)
    for (b in seq_len(n_boot)) {
      idx <- unlist(lapply(strata, function(s) {
        s_idx <- which(stratify == s)
        sample(s_idx, size = length(s_idx), replace = TRUE)
      }))
      boot_stats[b] <- as.numeric(statistic(.idx(data, idx)))
    }
  } else {
    for (b in seq_len(n_boot)) {
      idx <- sample.int(n, size = n, replace = TRUE)
      boot_stats[b] <- as.numeric(statistic(.idx(data, idx)))
    }
  }

  se   <- stats::sd(boot_stats)
  bias <- mean(boot_stats) - original
  alpha <- 1 - ci_level
  acc <- 0

  if (ci_method == "percentile") {
    ci_lo <- .pct(boot_stats, 100 * alpha / 2)
    ci_hi <- .pct(boot_stats, 100 * (1 - alpha / 2))
  } else if (ci_method == "normal") {
    z <- stats::qnorm(1 - alpha / 2)
    ci_lo <- original - bias - z * se
    ci_hi <- original - bias + z * se
  } else if (ci_method == "basic") {
    p_lo <- .pct(boot_stats, 100 * (1 - alpha / 2))
    p_hi <- .pct(boot_stats, 100 * alpha / 2)
    ci_lo <- 2 * original - p_lo
    ci_hi <- 2 * original - p_hi
  } else if (ci_method == "bca") {
    bca <- .bca_interval(data, statistic, boot_stats, original, ci_level)
    ci_lo <- bca$ci_lo
    ci_hi <- bca$ci_hi
    acc <- bca$acc
  } else if (ci_method == "studentized") {
    boot_ses <- numeric(n_boot)
    for (b in seq_len(n_boot)) {
      idx <- sample.int(n, size = n, replace = TRUE)
      boot_data <- .idx(data, idx)
      m <- .nrow_like(boot_data)
      inner <- numeric(50)
      for (ib in seq_len(50)) {
        inner_idx <- sample.int(m, size = m, replace = TRUE)
        inner[ib] <- as.numeric(statistic(.idx(boot_data, inner_idx)))
      }
      boot_ses[b] <- stats::sd(inner)
    }
    t_stats <- (boot_stats - original) / pmax(boot_ses, 1e-10)
    t_lo <- .pct(t_stats, 100 * (1 - alpha / 2))
    t_hi <- .pct(t_stats, 100 * alpha / 2)
    ci_lo <- original - t_lo * se
    ci_hi <- original - t_hi * se
  } else {
    stop(sprintf("Unknown ci_method: %s", ci_method))
  }

  .new_bootstrap_result(
    estimate = original, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n_boot = n_boot, method = "nonparametric", ci_method = ci_method,
    boot_distribution = boot_stats, original_estimate = original,
    acceleration = acc
  )
}

# BCa (bias-corrected and accelerated) percentile interval.
.bca_interval <- function(data, statistic, boot_stats, original, ci_level) {
  n <- .nrow_like(data)
  alpha <- 1 - ci_level

  # Bias-correction z0.
  z0 <- stats::qnorm(mean(boot_stats < original))

  # Acceleration via leave-one-out jackknife.
  jack <- numeric(n)
  for (i in seq_len(n)) {
    jack[i] <- as.numeric(statistic(.idx(data, -i)))
  }
  jm <- mean(jack)
  num <- sum((jm - jack) ^ 3)
  den <- 6 * (sum((jm - jack) ^ 2)) ^ 1.5
  a <- num / max(den, 1e-10)

  z_lo <- stats::qnorm(alpha / 2)
  z_hi <- stats::qnorm(1 - alpha / 2)

  a1 <- stats::pnorm(z0 + (z0 + z_lo) / max(1 - a * (z0 + z_lo), 0.01))
  a2 <- stats::pnorm(z0 + (z0 + z_hi) / max(1 - a * (z0 + z_hi), 0.01))

  ci_lo <- .pct(boot_stats, 100 * min(max(a1, 0.001), 0.999))
  ci_hi <- .pct(boot_stats, 100 * min(max(a2, 0.001), 0.999))
  list(ci_lo = ci_lo, ci_hi = ci_hi, acc = as.numeric(a))
}

#' Parametric bootstrap
#'
#' Generates bootstrap samples from a fitted parametric distribution
#' rather than from the empirical sample.
#'
#' @param data Original numeric data (used to fit the distribution).
#' @param statistic Function returning a scalar.
#' @param distribution One of \code{"normal"}, \code{"poisson"},
#'   \code{"binomial"}, \code{"exponential"}, \code{"gamma"}.
#' @param n_boot Number of replicates.
#' @param ci_level Confidence level.
#' @param seed Random seed.
#' @param ... Distribution-specific parameters (mu, sigma, lam, p,
#'   scale, shape).
#' @return A \code{morie_bootstrap_result}.
#' @export
parametric_bootstrap <- function(data, statistic, distribution = "normal",
                                 n_boot = 2000L, ci_level = 0.95,
                                 seed = 42L, ...) {
  set.seed(seed)
  data <- as.numeric(data)
  n <- length(data)
  original <- as.numeric(statistic(data))
  boot_stats <- numeric(n_boot)
  dp <- list(...)

  if (distribution == "normal") {
    mu <- if (!is.null(dp$mu)) dp$mu else mean(data)
    sigma <- if (!is.null(dp$sigma)) dp$sigma else stats::sd(data)
    for (b in seq_len(n_boot))
      boot_stats[b] <- as.numeric(statistic(stats::rnorm(n, mu, sigma)))
  } else if (distribution == "poisson") {
    lam <- if (!is.null(dp$lam)) dp$lam else mean(data)
    for (b in seq_len(n_boot))
      boot_stats[b] <- as.numeric(statistic(as.numeric(stats::rpois(n, lam))))
  } else if (distribution == "binomial") {
    p <- if (!is.null(dp$p)) dp$p else mean(data)
    for (b in seq_len(n_boot))
      boot_stats[b] <- as.numeric(statistic(as.numeric(stats::rbinom(n, 1, p))))
  } else if (distribution == "exponential") {
    scale <- if (!is.null(dp$scale)) dp$scale else mean(data)
    for (b in seq_len(n_boot))
      boot_stats[b] <- as.numeric(statistic(stats::rexp(n, rate = 1 / scale)))
  } else if (distribution == "gamma") {
    shape <- dp$shape
    scale <- dp$scale
    if (is.null(shape) || is.null(scale)) {
      mu <- mean(data)
      va <- stats::var(data)
      shape <- mu ^ 2 / max(va, 1e-10)
      scale <- max(va, 1e-10) / mu
    }
    for (b in seq_len(n_boot))
      boot_stats[b] <- as.numeric(statistic(stats::rgamma(n, shape = shape,
                                                          scale = scale)))
  } else {
    stop(sprintf("Unknown distribution: %s", distribution))
  }

  se <- stats::sd(boot_stats)
  bias <- mean(boot_stats) - original
  alpha <- 1 - ci_level
  ci_lo <- .pct(boot_stats, 100 * alpha / 2)
  ci_hi <- .pct(boot_stats, 100 * (1 - alpha / 2))

  .new_bootstrap_result(
    estimate = original, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n_boot = n_boot,
    method = paste0("parametric_", distribution),
    ci_method = "percentile",
    boot_distribution = boot_stats, original_estimate = original
  )
}

#' Wild bootstrap for linear regression with heteroskedasticity
#'
#' Multiplies the residuals by random weights (Rademacher or Mammen)
#' and refits OLS.
#'
#' @param y Numeric response vector.
#' @param X Numeric design matrix (include an intercept column).
#' @param statistic_idx Column index of the coefficient of interest
#'   (1-based).
#' @param n_boot Number of replicates.
#' @param ci_level Confidence level.
#' @param weight_distribution \code{"rademacher"} or \code{"mammen"}.
#' @param seed Random seed.
#' @return A \code{morie_bootstrap_result}.
#' @export
wild_bootstrap <- function(y, X, statistic_idx = 2L, n_boot = 999L,
                           ci_level = 0.95,
                           weight_distribution = "rademacher",
                           seed = 42L) {
  set.seed(seed)
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)

  fit <- stats::lm.fit(X, y)
  beta_hat <- fit$coefficients
  beta_hat[is.na(beta_hat)] <- 0
  residuals <- y - drop(X %*% beta_hat)
  y_hat <- drop(X %*% beta_hat)
  original <- as.numeric(beta_hat[statistic_idx])

  boot_stats <- numeric(n_boot)

  for (b in seq_len(n_boot)) {
    if (weight_distribution == "rademacher") {
      w <- sample(c(-1, 1), size = n, replace = TRUE)
    } else if (weight_distribution == "mammen") {
      sq5 <- sqrt(5)
      p_m <- (sq5 + 1) / (2 * sq5)
      v1 <- -(sq5 - 1) / 2
      v2 <-  (sq5 + 1) / 2
      w <- ifelse(stats::runif(n) < p_m, v1, v2)
    } else {
      stop(sprintf("Unknown weight_distribution: %s", weight_distribution))
    }
    y_boot <- y_hat + residuals * w
    fit_b <- stats::lm.fit(X, y_boot)
    cb <- fit_b$coefficients
    cb[is.na(cb)] <- 0
    boot_stats[b] <- as.numeric(cb[statistic_idx])
  }

  se <- stats::sd(boot_stats)
  bias <- mean(boot_stats) - original
  alpha <- 1 - ci_level
  ci_lo <- .pct(boot_stats, 100 * alpha / 2)
  ci_hi <- .pct(boot_stats, 100 * (1 - alpha / 2))

  .new_bootstrap_result(
    estimate = original, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n_boot = n_boot, method = "wild", ci_method = "percentile",
    boot_distribution = boot_stats, original_estimate = original
  )
}

#' Block bootstrap for dependent / time-series data
#'
#' Resamples blocks of consecutive observations.
#'
#' @param data Numeric vector or matrix.
#' @param statistic Function returning a scalar.
#' @param block_size Integer block length.
#' @param n_boot Number of replicates.
#' @param ci_level Confidence level.
#' @param method One of \code{"moving"}, \code{"circular"},
#'   \code{"stationary"}.
#' @param seed Random seed.
#' @return A \code{morie_bootstrap_result}.
#' @export
block_bootstrap <- function(data, statistic, block_size,
                            n_boot = 2000L, ci_level = 0.95,
                            method = "circular", seed = 42L) {
  set.seed(seed)
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))
  n_blocks <- as.integer(ceiling(n / block_size))
  boot_stats <- numeric(n_boot)

  for (b in seq_len(n_boot)) {
    if (method == "circular") {
      starts <- sample.int(n, size = n_blocks, replace = TRUE) - 1L
      idx_all <- unlist(lapply(starts, function(s)
        ((s + 0:(block_size - 1)) %% n) + 1L))
      idx <- idx_all[seq_len(n)]
    } else if (method == "moving") {
      max_start <- n - block_size
      starts <- sample.int(max_start + 1L, size = n_blocks, replace = TRUE)
      idx_all <- unlist(lapply(starts, function(s) s + 0:(block_size - 1)))
      idx <- idx_all[seq_len(n)]
    } else if (method == "stationary") {
      idx <- integer(n)
      i <- sample.int(n, 1L)
      k <- 0L
      while (k < n) {
        k <- k + 1L
        idx[k] <- ((i - 1L) %% n) + 1L
        if (stats::runif(1) < 1 / block_size) i <- sample.int(n, 1L)
        else i <- i + 1L
      }
    } else {
      stop(sprintf("Unknown method: %s", method))
    }
    boot_stats[b] <- as.numeric(statistic(.idx(data, idx)))
  }

  se <- stats::sd(boot_stats)
  bias <- mean(boot_stats) - original
  alpha <- 1 - ci_level
  ci_lo <- .pct(boot_stats, 100 * alpha / 2)
  ci_hi <- .pct(boot_stats, 100 * (1 - alpha / 2))

  .new_bootstrap_result(
    estimate = original, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n_boot = n_boot, method = paste0("block_", method),
    ci_method = "percentile",
    boot_distribution = boot_stats, original_estimate = original
  )
}

#' Delete-one (leave-one-out) jackknife
#'
#' @param data Numeric vector or matrix.
#' @param statistic Function returning a scalar.
#' @param ci_level Confidence level.
#' @return A \code{morie_jackknife_result}.
#' @export
jackknife <- function(data, statistic, ci_level = 0.95) {
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))

  jack <- numeric(n)
  for (i in seq_len(n)) jack[i] <- as.numeric(statistic(.idx(data, -i)))

  jm <- mean(jack)
  pseudovalues <- n * original - (n - 1) * jack
  influence <- original - jack

  bias <- (n - 1) * (jm - original)
  se <- sqrt((n - 1) / n * sum((jack - jm) ^ 2))

  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  ci_lo <- original - bias - z * se
  ci_hi <- original - bias + z * se

  .new_jackknife_result(
    estimate = original - bias, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n = n, jackknife_estimates = jack,
    pseudovalues = pseudovalues, influence_values = influence
  )
}

#' Delete-d (generalised) jackknife
#'
#' @param data Numeric vector or matrix.
#' @param statistic Function returning a scalar.
#' @param d Number of observations to delete per replicate.
#' @param ci_level Confidence level.
#' @param max_subsets Maximum subsets to evaluate.
#' @param seed Random seed.
#' @return A \code{morie_jackknife_result}.
#' @export
delete_d_jackknife <- function(data, statistic, d = 2L,
                               ci_level = 0.95, max_subsets = 5000L,
                               seed = 42L) {
  set.seed(seed)
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))
  total_subsets <- choose(n, d)

  if (total_subsets <= max_subsets) {
    delete_sets <- utils::combn(n, d, simplify = FALSE)
  } else {
    delete_sets <- replicate(max_subsets,
                             sort(sample.int(n, d, replace = FALSE)),
                             simplify = FALSE)
    keys <- vapply(delete_sets, function(x) paste(x, collapse = ","),
                   character(1))
    delete_sets <- delete_sets[!duplicated(keys)]
  }

  m <- length(delete_sets)
  jack <- numeric(m)
  for (k in seq_len(m)) {
    jack[k] <- as.numeric(statistic(.idx(data, -delete_sets[[k]])))
  }

  jm <- mean(jack)
  cc <- (n - d) / d
  bias <- cc * (jm - original)
  se <- sqrt(cc / m * sum((jack - jm) ^ 2))

  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  ci_lo <- original - bias - z * se
  ci_hi <- original - bias + z * se

  pseudovalues <- n * original - (n - d) * jack

  .new_jackknife_result(
    estimate = original - bias, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n = n, jackknife_estimates = jack,
    pseudovalues = pseudovalues, influence_values = original - jack
  )
}

#' Two-sample permutation test
#'
#' Shuffles the combined samples \code{n_permutations} times to
#' construct the null distribution of the chosen test statistic.
#'
#' @param group1,group2 Numeric vectors.
#' @param statistic Either \code{"mean_diff"}, \code{"median_diff"},
#'   \code{"t_stat"}, or a function \code{f(g1, g2) -> scalar}.
#' @param n_permutations Number of permutations.
#' @param alternative \code{"two-sided"}, \code{"greater"},
#'   or \code{"less"}.
#' @param seed Random seed.
#' @return A \code{morie_permutation_test_result}.
#' @export
permutation_test <- function(group1, group2, statistic = "mean_diff",
                             n_permutations = 9999L,
                             alternative = "two-sided", seed = 42L) {
  set.seed(seed)
  g1 <- as.numeric(group1)
  g2 <- as.numeric(group2)
  combined <- c(g1, g2)
  n1 <- length(g1)
  n <- length(combined)

  if (is.function(statistic)) {
    stat_fn <- statistic
  } else if (identical(statistic, "mean_diff")) {
    stat_fn <- function(a, b) mean(a) - mean(b)
  } else if (identical(statistic, "median_diff")) {
    stat_fn <- function(a, b) stats::median(a) - stats::median(b)
  } else if (identical(statistic, "t_stat")) {
    stat_fn <- function(a, b) {
      s1 <- stats::var(a)
      s2 <- stats::var(b)
      se <- sqrt(s1 / length(a) + s2 / length(b))
      (mean(a) - mean(b)) / max(se, 1e-10)
    }
  } else {
    stop(sprintf("Unknown statistic: %s", statistic))
  }

  observed <- as.numeric(stat_fn(g1, g2))
  null_dist <- numeric(n_permutations)
  for (i in seq_len(n_permutations)) {
    perm <- sample(combined, size = n, replace = FALSE)
    null_dist[i] <- as.numeric(stat_fn(perm[seq_len(n1)], perm[(n1 + 1):n]))
  }

  p_value <- switch(
    alternative,
    "two-sided" = mean(abs(null_dist) >= abs(observed)),
    "greater"   = mean(null_dist >= observed),
    "less"      = mean(null_dist <= observed),
    stop(sprintf("Unknown alternative: %s", alternative))
  )
  # Exact-permutation correction.
  p_value <- (p_value * n_permutations + 1) / (n_permutations + 1)

  .new_permutation_test_result(
    observed_statistic = observed, p_value = p_value,
    null_distribution = null_dist,
    n_permutations = n_permutations, alternative = alternative
  )
}

#' Paired permutation test (sign-flipping)
#'
#' @param x,y Paired numeric vectors (same length).
#' @param statistic \code{"mean_diff"} or \code{"median_diff"}.
#' @param n_permutations Number of permutations.
#' @param alternative \code{"two-sided"}, \code{"greater"}, \code{"less"}.
#' @param seed Random seed.
#' @return A \code{morie_permutation_test_result}.
#' @export
paired_permutation_test <- function(x, y, statistic = "mean_diff",
                                    n_permutations = 9999L,
                                    alternative = "two-sided", seed = 42L) {
  set.seed(seed)
  diffs <- as.numeric(x) - as.numeric(y)
  n <- length(diffs)

  if (identical(statistic, "mean_diff")) stat_fn <- mean
  else if (identical(statistic, "median_diff")) stat_fn <- stats::median
  else stop(sprintf("Unknown statistic: %s", statistic))

  observed <- as.numeric(stat_fn(diffs))
  null_dist <- numeric(n_permutations)
  for (i in seq_len(n_permutations)) {
    signs <- sample(c(-1, 1), size = n, replace = TRUE)
    null_dist[i] <- as.numeric(stat_fn(diffs * signs))
  }

  p_value <- switch(
    alternative,
    "two-sided" = mean(abs(null_dist) >= abs(observed)),
    "greater"   = mean(null_dist >= observed),
    "less"      = mean(null_dist <= observed),
    stop(sprintf("Unknown alternative: %s", alternative))
  )
  p_value <- (p_value * n_permutations + 1) / (n_permutations + 1)

  .new_permutation_test_result(
    observed_statistic = observed, p_value = p_value,
    null_distribution = null_dist,
    n_permutations = n_permutations, alternative = alternative
  )
}

#' Subsampling inference (Politis, Romano & Wolf)
#'
#' Draws without replacement at a smaller sample size; valid under
#' weaker conditions than the bootstrap.
#'
#' @param data Numeric vector or matrix.
#' @param statistic Function returning a scalar.
#' @param subsample_size Subsample size; default \code{floor(n^0.7)}.
#' @param n_subsamples Number of subsamples.
#' @param ci_level Confidence level.
#' @param seed Random seed.
#' @return A \code{morie_bootstrap_result}.
#' @export
subsampling <- function(data, statistic, subsample_size = NULL,
                        n_subsamples = 1000L, ci_level = 0.95,
                        seed = 42L) {
  set.seed(seed)
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))

  if (is.null(subsample_size)) subsample_size <- as.integer(n ^ 0.7)
  subsample_size <- min(subsample_size, n - 1L)

  sub_stats <- numeric(n_subsamples)
  for (b in seq_len(n_subsamples)) {
    idx <- sample.int(n, size = subsample_size, replace = FALSE)
    sub_stats[b] <- as.numeric(statistic(.idx(data, idx)))
  }

  scaling <- sqrt(n / subsample_size)
  scaled_diffs <- scaling * (sub_stats - original)
  alpha <- 1 - ci_level
  q_lo <- .pct(scaled_diffs, 100 * alpha / 2)
  q_hi <- .pct(scaled_diffs, 100 * (1 - alpha / 2))

  ci_lo <- original - q_hi / sqrt(n)
  ci_hi <- original - q_lo / sqrt(n)
  se <- stats::sd(scaled_diffs) / sqrt(n)

  .new_bootstrap_result(
    estimate = original, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = 0,
    n_boot = n_subsamples, method = "subsampling",
    ci_method = "subsampling",
    boot_distribution = sub_stats, original_estimate = original
  )
}

#' .632 and .632+ bootstrap estimators for prediction error
#'
#' @param X Numeric design matrix (n x p).
#' @param y Numeric response (length n).
#' @param model_fn Function \code{model_fn(X_train, y_train)} returning
#'   a model object that supports \code{predict(model, X_test)}.
#' @param score_fn Function \code{score_fn(y_true, y_pred) -> scalar}.
#' @param n_boot Number of bootstrap replicates.
#' @param seed Random seed.
#' @return Named numeric list with apparent_error, bootstrap_error,
#'   error_632, error_632plus.
#' @export
bootstrap_632 <- function(X, y, model_fn, score_fn,
                          n_boot = 200L, seed = 42L) {
  set.seed(seed)
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- length(y)

  model_full <- model_fn(X, y)
  y_pred_full <- stats::predict(model_full, X)
  apparent <- as.numeric(score_fn(y, y_pred_full))

  boot_errors <- numeric(0)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    oob <- setdiff(seq_len(n), unique(idx))
    if (length(oob) == 0L) next
    model_b <- model_fn(X[idx, , drop = FALSE], y[idx])
    y_pred_oob <- stats::predict(model_b, X[oob, , drop = FALSE])
    boot_errors <- c(boot_errors,
                     as.numeric(score_fn(y[oob], y_pred_oob)))
  }

  if (length(boot_errors) == 0L) {
    return(list(apparent_error = apparent,
                bootstrap_error = NA_real_,
                error_632 = NA_real_,
                error_632plus = NA_real_))
  }

  boot_error <- mean(boot_errors)
  error_632 <- 0.368 * apparent + 0.632 * boot_error

  uniq_y <- unique(y)
  if (length(uniq_y) <= 2L) {
    p <- mean(y == uniq_y[1])
    q <- mean(y_pred_full == uniq_y[1])
    gamma <- p * (1 - q) + (1 - p) * q
  } else {
    counts <- as.numeric(table(y))
    gamma <- 1 - sum((counts / n) ^ 2)
  }

  R <- (boot_error - apparent) / max(gamma - apparent, 1e-10)
  R <- min(max(R, 0), 1)
  w <- 0.632 / (1 - 0.368 * R)
  error_632plus <- (1 - w) * apparent + w * boot_error

  list(apparent_error = apparent,
       bootstrap_error = boot_error,
       error_632 = error_632,
       error_632plus = error_632plus)
}

#' K-fold cross-validation
#'
#' Supports plain, stratified, and grouped variants.
#'
#' Bootstrap-flavoured K-fold cross-validation (internal).
#'
#' Lower-level CV used by [repeated_cv()] / [leave_one_out_cv()].
#' Public CV with `(fit_fn, predict_fn, X, y, ...)` signature lives in
#' [cross_validate()] (validation.R). Renamed to avoid the symbol
#' collision that R CMD check surfaced as unused-arg notes.
#'
#' @param X Numeric design matrix.
#' @param y Response vector.
#' @param model_fn Function \code{(X_train, y_train) -> model}.
#' @param score_fn Function \code{(y_true, y_pred) -> scalar}.
#' @param n_folds Number of folds.
#' @param stratify Optional stratification vector.
#' @param groups Optional grouping vector (no group split across folds).
#' @param seed Random seed.
#' @return A \code{morie_cv_result}.
#' @keywords internal
#' @noRd
.boot_cross_validate <- function(X, y, model_fn, score_fn,
                                 n_folds = 10L, stratify = NULL,
                                 groups = NULL, seed = 42L) {
  set.seed(seed)
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- length(y)

  if (!is.null(groups)) {
    groups <- as.vector(groups)
    uniq <- sample(unique(groups))
    group_folds <- split(uniq, cut(seq_along(uniq), n_folds, labels = FALSE))
    fold_indices <- lapply(group_folds,
                           function(gf) which(groups %in% gf))
  } else if (!is.null(stratify)) {
    stratify <- as.vector(stratify)
    fold_indices <- vector("list", n_folds)
    for (s in unique(stratify)) {
      s_idx <- sample(which(stratify == s))
      splits <- split(s_idx,
                      cut(seq_along(s_idx), n_folds, labels = FALSE))
      for (f in seq_len(n_folds))
        fold_indices[[f]] <- c(fold_indices[[f]], splits[[f]])
    }
  } else {
    idx <- sample.int(n)
    fold_indices <- split(idx, cut(seq_along(idx), n_folds, labels = FALSE))
  }

  scores <- numeric(n_folds)
  fold_sizes <- integer(n_folds)
  for (f in seq_len(n_folds)) {
    test_idx <- fold_indices[[f]]
    train_idx <- unlist(fold_indices[-f])
    model <- model_fn(X[train_idx, , drop = FALSE], y[train_idx])
    y_pred <- stats::predict(model, X[test_idx, , drop = FALSE])
    scores[f] <- as.numeric(score_fn(y[test_idx], y_pred))
    fold_sizes[f] <- length(test_idx)
  }

  mean_score <- mean(scores)
  se_score <- stats::sd(scores) / sqrt(n_folds)
  z <- stats::qnorm(0.975)

  .new_cv_result(
    scores = scores, mean_score = mean_score, se_score = se_score,
    ci_lower = mean_score - z * se_score,
    ci_upper = mean_score + z * se_score,
    n_folds = n_folds, metric = "custom",
    fold_sizes = as.integer(fold_sizes)
  )
}

#' Repeated K-fold cross-validation
#'
#' @inheritParams .boot_cross_validate
#' @param X Numeric matrix or data.frame of predictors.
#' @param y Numeric or factor outcome vector aligned with rows of `X`.
#' @param model_fn Function `(X, y) -> fitted-model` used on each training fold.
#' @param score_fn Function `(y_true, y_pred) -> numeric` returning a single performance metric.
#' @param n_folds Integer; number of folds per repeat (default 10).
#' @param n_repeats Number of repetitions.
#' @param seed Integer RNG seed for reproducibility.
#' @return A \code{morie_cv_result} pooling scores across repeats.
#' @export
repeated_cv <- function(X, y, model_fn, score_fn,
                        n_folds = 10L, n_repeats = 10L, seed = 42L) {
  all_scores <- numeric(0)
  all_fold_sizes <- integer(0)
  for (r in seq_len(n_repeats)) {
    res <- .boot_cross_validate(X, y, model_fn, score_fn,
                                n_folds = n_folds, seed = seed + r - 1L)
    all_scores <- c(all_scores, res$scores)
    all_fold_sizes <- c(all_fold_sizes, res$fold_sizes)
  }
  mean_score <- mean(all_scores)
  se_score <- stats::sd(all_scores) / sqrt(length(all_scores))
  z <- stats::qnorm(0.975)
  .new_cv_result(
    scores = all_scores, mean_score = mean_score, se_score = se_score,
    ci_lower = mean_score - z * se_score,
    ci_upper = mean_score + z * se_score,
    n_folds = n_folds * n_repeats, metric = "custom",
    fold_sizes = all_fold_sizes
  )
}

#' Leave-one-out cross-validation
#'
#' @inheritParams .boot_cross_validate
#' @param X Numeric matrix or data.frame of predictors.
#' @param y Numeric or factor outcome vector aligned with rows of `X`.
#' @param model_fn Function `(X, y) -> fitted-model` used on each training fold.
#' @param score_fn Function `(y_true, y_pred) -> numeric` returning a single performance metric.
#' @return A \code{morie_cv_result}.
#' @export
leave_one_out_cv <- function(X, y, model_fn, score_fn) {
  .boot_cross_validate(X, y, model_fn, score_fn, n_folds = length(y))
}
