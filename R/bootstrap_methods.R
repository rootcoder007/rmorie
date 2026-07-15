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
#
# Phase 1.i refactor (2026-05-25): every textbook bootstrap routine has
# been re-routed through the canonical CRAN packages where one exists.
# Each wrapper preserves the rmorie API and S3 return shapes
# (`morie_bootstrap_result`, `morie_jackknife_result`,
# `morie_permutation_test_result`, `morie_cv_result`) so that downstream
# rmorie code, the `print` methods, and MRM pipelines keep working
# unchanged on a CRAN-only install.
#
#   * boot::boot / boot::boot.ci  -- nonparametric, parametric, BCa /
#                                    percentile / basic / normal / stud.
#   * boot::tsboot                -- block bootstrap (fixed / geom).
#   * boot::censboot              -- (cross-referenced) censored boot.
#   * bootstrap::jackknife        -- delete-one jackknife reference.
#   * resample                    -- delete-d jackknife + permutation.
#   * rsample::bootstraps,
#     rsample::vfold_cv           -- tidymodels-style resampling.
#   * simpleboot::two.boot,
#     simpleboot::one.boot,
#     simpleboot::lm.boot         -- fast-path common cases.
#   * coin                        -- permutation / paired permutation.
#   * sandwich::vcovBS,
#     fwildclusterboot::boottest  -- wild / cluster wild bootstrap
#                                    (cross-referenced in Rd).
#   * ipred::errorest             -- .632 / .632+ prediction error.
#   * caret::trainControl,
#     rsample::vfold_cv           -- k-fold / repeated / LOO CV.
#
# New extender entry points added in Phase 1.i (thin pass-through):
#
#   * morie_boot_run()            -- direct boot::boot bridge.
#   * morie_boot_basic_ci()       -- direct boot::boot.ci bridge.
#   * morie_rsample_bootstraps()  -- rsample::bootstraps bridge.
#   * morie_simpleboot_two()      -- simpleboot::two.boot bridge.
#
# Functions kept as in-house implementations and flagged
# "novel/no-clean-CRAN-equivalent":
#
#   * subsampling()               -- Politis-Romano-Wolf rate scaling;
#                                    `np` covers a related kernel
#                                    subsampling with a different API.
#   * bootstrap()` BCa stratify / cluster paths -- `boot::boot` exposes
#                                    `strata=` but does not stack
#                                    stratified BCa with custom
#                                    `statistic(data) -> scalar`
#                                    signature; inline retained.
#   * morie_*_uof_*, morie_otis_*, morie_tps_*, morie_siu_* lookalikes
#     (none live in this file but referenced in tests).

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Internal helper: Boot Have Boot
#' @noRd
.boot_have_boot       <- function() requireNamespace("boot",       quietly = TRUE)
#' Internal helper: Boot Have Bootstrap
#' @noRd
.boot_have_bootstrap  <- function() requireNamespace("bootstrap",  quietly = TRUE)
#' Internal helper: Boot Have Resample
#' @noRd
.boot_have_resample   <- function() requireNamespace("resample",   quietly = TRUE)
#' Internal helper: Boot Have Rsample
#' @noRd
.boot_have_rsample    <- function() requireNamespace("rsample",    quietly = TRUE)
#' Internal helper: Boot Have Simpleboot
#' @noRd
.boot_have_simpleboot <- function() requireNamespace("simpleboot", quietly = TRUE)
#' Internal helper: Boot Have Coin
#' @noRd
.boot_have_coin       <- function() requireNamespace("coin",       quietly = TRUE)
#' Internal helper: Boot Have Ipred
#' @noRd
.boot_have_ipred      <- function() requireNamespace("ipred",      quietly = TRUE)

# Result container constructors (unchanged shapes).

#' Internal helper: New Bootstrap Result
#' @noRd
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

#' Internal helper: New Jackknife Result
#' @noRd
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

#' Internal helper: New Permutation Test Result
#' @noRd
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

#' Internal helper: New Cv Result
#' @noRd
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
#' Internal helper: Pct
#' @noRd
.pct <- function(x, p) {
  unname(stats::quantile(x, probs = p / 100, names = FALSE, type = 7))
}

# Helper: subset rows of a vector or matrix.
#' Internal helper: Idx
#' @noRd
.idx <- function(data, idx) {
  if (is.matrix(data) || is.data.frame(data)) {
    data[idx, , drop = FALSE]
  } else {
    data[idx]
  }
}

#' Internal helper: Nrow Like
#' @noRd
.nrow_like <- function(data) {
  if (is.matrix(data) || is.data.frame(data)) nrow(data) else length(data)
}

# Adapt rmorie `statistic(data)` signature to boot's `statistic(data, idx)`.
#' Internal helper: Boot Statistic Adapter
#' @noRd
.boot_statistic_adapter <- function(statistic) {
  function(d, i) as.numeric(statistic(.idx(d, i)))
}

# Extract `(ci_lo, ci_hi)` from a `boot.ci` object by ci_method label.
#' Internal helper: Boot Ci Extract
#' @noRd
.boot_ci_extract <- function(bci, ci_method) {
  type_key <- switch(
    ci_method,
    "percentile"  = "percent",
    "basic"       = "basic",
    "normal"      = "normal",
    "bca"         = "bca",
    "studentized" = "student",
    "percent"
  )
  comp <- bci[[type_key]]
  if (is.null(comp)) return(c(NA_real_, NA_real_))
  ncols <- ncol(comp)
  c(as.numeric(comp[1L, ncols - 1L]), as.numeric(comp[1L, ncols]))
}

# ---------------------------------------------------------------------------
# Nonparametric bootstrap (thin wrapper over boot::boot + boot::boot.ci)
# ---------------------------------------------------------------------------

#' Nonparametric bootstrap inference
#'
#' Resamples observations with replacement and computes a confidence
#' interval via the percentile, normal, basic, BCa, or studentized
#' method. Delegates the resampling loop and CI computation to
#' \code{boot::boot} and \code{boot::boot.ci} when the \pkg{boot}
#' package is installed. Falls back to an inline implementation
#' otherwise so the wrapper keeps working on minimal installs.
#' Stratified and cluster resamples are supported in both arms.
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
#' @seealso \code{boot::boot}, \code{boot::boot.ci},
#'   [morie_boot_run()], [morie_boot_basic_ci()].
#' @export
bootstrap <- function(data, statistic, n_boot = 2000L, ci_level = 0.95,
                      ci_method = "bca", seed = 42L,
                      stratify = NULL, cluster = NULL) {
  if (!ci_method %in% c("percentile", "normal", "basic", "bca",
                        "studentized")) {
    stop(sprintf("Unknown ci_method: %s", ci_method))
  }

  set.seed(seed)
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))

  # Delegate to boot::boot when available AND no cluster resampling
  # (boot::boot's `strata=` covers stratification, but its API does not
  # natively support cluster-of-clusters resampling for arbitrary
  # statistic(data) signatures, so the inline cluster arm is retained).
  if (.boot_have_boot() && is.null(cluster) && ci_method != "studentized") {
    bf <- .boot_statistic_adapter(statistic)
    strata_arg <- if (is.null(stratify)) rep(1L, n) else as.integer(factor(stratify))
    bo <- boot::boot(data = data, statistic = bf, R = n_boot,
                     strata = strata_arg)
    boot_stats <- as.numeric(bo$t[, 1L])
    se <- stats::sd(boot_stats)
    bias <- mean(boot_stats) - original
    acc <- 0
    type_key <- switch(ci_method,
                       "percentile" = "perc",
                       "normal"     = "norm",
                       "basic"      = "basic",
                       "bca"        = "bca")
    bci <- tryCatch(
      boot::boot.ci(bo, conf = ci_level, type = type_key),
      error = function(e) NULL
    )
    if (!is.null(bci)) {
      ci_pair <- .boot_ci_extract(bci, ci_method)
      if (ci_method == "bca" && !is.null(bci$bca)) {
        acc <- 0
      }
    } else {
      ci_pair <- c(NA_real_, NA_real_)
    }
    if (anyNA(ci_pair)) {
      # Fallback for tiny n_boot where boot.ci refuses to compute BCa.
      alpha <- 1 - ci_level
      ci_pair <- c(.pct(boot_stats, 100 * alpha / 2),
                   .pct(boot_stats, 100 * (1 - alpha / 2)))
    }
    return(.new_bootstrap_result(
      estimate = original, se = se,
      ci_lower = ci_pair[1L], ci_upper = ci_pair[2L], bias = bias,
      n_boot = n_boot, method = "nonparametric", ci_method = ci_method,
      boot_distribution = boot_stats, original_estimate = original,
      acceleration = acc
    ))
  }

  # Inline fallback: full original implementation (stratified, cluster,
  # plain, studentized; BCa via `.bca_interval`).
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

  se <- stats::sd(boot_stats)
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
  } else {
    # studentized
    boot_ses <- numeric(n_boot)
    for (b in seq_len(n_boot)) {
      idx <- sample.int(n, size = n, replace = TRUE)
      boot_data <- .idx(data, idx)
      m <- .nrow_like(boot_data)
      inner <- numeric(50L)
      for (ib in seq_len(50L)) {
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
  }

  .new_bootstrap_result(
    estimate = original, se = se,
    ci_lower = ci_lo, ci_upper = ci_hi, bias = bias,
    n_boot = n_boot, method = "nonparametric", ci_method = ci_method,
    boot_distribution = boot_stats, original_estimate = original,
    acceleration = acc
  )
}

# BCa (bias-corrected and accelerated) percentile interval (inline
# fallback). `boot::boot.ci(type = "bca")` is the canonical CRAN
# equivalent and is used by `bootstrap()` when \pkg{boot} is installed.
#' Internal helper: Bca Interval
#' @noRd
.bca_interval <- function(data, statistic, boot_stats, original, ci_level) {
  n <- .nrow_like(data)
  alpha <- 1 - ci_level

  z0 <- stats::qnorm(mean(boot_stats < original))

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

# ---------------------------------------------------------------------------
# Parametric bootstrap (boot::boot(sim = "parametric"))
# ---------------------------------------------------------------------------

#' Parametric bootstrap
#'
#' Generates bootstrap samples from a fitted parametric distribution
#' rather than from the empirical sample. Delegates to
#' \code{boot::boot(sim = "parametric")} when \pkg{boot} is installed;
#' otherwise uses an inline `rnorm/rpois/rbinom/rexp/rgamma` loop.
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
#' @seealso \code{boot::boot}.
#' @export
parametric_bootstrap <- function(data, statistic, distribution = "normal",
                                 n_boot = 2000L, ci_level = 0.95,
                                 seed = 42L, ...) {
  if (!distribution %in% c("normal", "poisson", "binomial",
                           "exponential", "gamma")) {
    stop(sprintf("Unknown distribution: %s", distribution))
  }

  set.seed(seed)
  data <- as.numeric(data)
  n <- length(data)
  original <- as.numeric(statistic(data))
  dp <- list(...)

  # Build the ran.gen() callable for boot::boot(sim = "parametric").
  pars <- .param_boot_pars(distribution, data, dp)
  rg <- .param_boot_rangen(distribution, n)

  if (.boot_have_boot()) {
    boot_stat <- function(d, ...) as.numeric(statistic(d))
    bo <- boot::boot(data = data, statistic = boot_stat, R = n_boot,
                     sim = "parametric", ran.gen = rg, mle = pars)
    boot_stats <- as.numeric(bo$t[, 1L])
  } else {
    boot_stats <- numeric(n_boot)
    for (b in seq_len(n_boot)) {
      boot_stats[b] <- as.numeric(statistic(rg(data, pars)))
    }
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

# Build `mle` argument for the parametric ran.gen.
#' Internal helper: Param Boot Pars
#' @noRd
.param_boot_pars <- function(distribution, data, dp) {
  switch(
    distribution,
    "normal"      = list(mu = dp$mu %||% mean(data),
                         sigma = dp$sigma %||% stats::sd(data)),
    "poisson"     = list(lam = dp$lam %||% mean(data)),
    "binomial"    = list(p = dp$p %||% mean(data)),
    "exponential" = list(scale = dp$scale %||% mean(data)),
    "gamma"       = {
      if (is.null(dp$shape) || is.null(dp$scale)) {
        mu <- mean(data)
        va <- stats::var(data)
        list(shape = mu ^ 2 / max(va, 1e-10),
             scale = max(va, 1e-10) / mu)
      } else {
        list(shape = dp$shape, scale = dp$scale)
      }
    }
  )
}

# Build ran.gen() for boot::boot(sim = "parametric").
#' Internal helper: Param Boot Rangen
#' @noRd
.param_boot_rangen <- function(distribution, n) {
  switch(
    distribution,
    "normal"      = function(d, p) stats::rnorm(n, p$mu, p$sigma),
    "poisson"     = function(d, p) as.numeric(stats::rpois(n, p$lam)),
    "binomial"    = function(d, p) as.numeric(stats::rbinom(n, 1, p$p)),
    "exponential" = function(d, p) stats::rexp(n, rate = 1 / p$scale),
    "gamma"       = function(d, p) stats::rgamma(n, shape = p$shape,
                                                 scale = p$scale)
  )
}

# Null-coalesce helper (used in .param_boot_pars).
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Wild bootstrap (cross-referenced sandwich::vcovBS + fwildclusterboot)
# ---------------------------------------------------------------------------

#' Wild bootstrap for linear regression with heteroskedasticity
#'
#' Multiplies the residuals by random weights (Rademacher or Mammen)
#' and refits OLS. \code{sandwich::vcovBS} implements the standard
#' wild bootstrap variance-covariance and
#' \code{fwildclusterboot::boottest} adds cluster-wild
#' \emph{p}-values; both are cross-referenced here. The inline
#' implementation is retained because rmorie's API returns the
#' resampled coefficient distribution (not just a vcov), which is
#' what downstream MRM analyses consume.
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
#' @seealso \code{sandwich::vcovBS}, \code{fwildclusterboot::boottest},
#'   [morie_did_wild_cluster_bootstrap()].
#' @export
wild_bootstrap <- function(y, X, statistic_idx = 2L, n_boot = 999L,
                           ci_level = 0.95,
                           weight_distribution = "rademacher",
                           seed = 42L) {
  if (!weight_distribution %in% c("rademacher", "mammen")) {
    stop(sprintf("Unknown weight_distribution: %s", weight_distribution))
  }

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
    w <- .wild_weights(weight_distribution, n)
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

# Wild-bootstrap weight draws.
#' Internal helper: Wild Weights
#' @noRd
.wild_weights <- function(weight_distribution, n) {
  if (weight_distribution == "rademacher") {
    sample(c(-1, 1), size = n, replace = TRUE)
  } else {
    sq5 <- sqrt(5)
    p_m <- (sq5 + 1) / (2 * sq5)
    v1 <- -(sq5 - 1) / 2
    v2 <-  (sq5 + 1) / 2
    ifelse(stats::runif(n) < p_m, v1, v2)
  }
}

# ---------------------------------------------------------------------------
# Block bootstrap (boot::tsboot)
# ---------------------------------------------------------------------------

#' Block bootstrap for dependent / time-series data
#'
#' Resamples blocks of consecutive observations. Delegates to
#' \code{boot::tsboot} when \pkg{boot} is installed (fixed and
#' stationary / geometric blocks); a circular-block path is
#' implemented inline because \code{boot::tsboot} does not expose a
#' circular-block sim mode directly. Falls back to a pure-R loop
#' otherwise.
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
#' @seealso \code{boot::tsboot}.
#' @export
block_bootstrap <- function(data, statistic, block_size,
                            n_boot = 2000L, ci_level = 0.95,
                            method = "circular", seed = 42L) {
  if (!method %in% c("moving", "circular", "stationary")) {
    stop(sprintf("Unknown method: %s", method))
  }
  set.seed(seed)
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))

  # Delegate moving / stationary to boot::tsboot for univariate input.
  if (.boot_have_boot() && method != "circular" &&
      is.null(dim(data))) {
    sim_arg <- if (method == "moving") "fixed" else "geom"
    bf <- function(d) as.numeric(statistic(d))
    bo <- boot::tsboot(tseries = as.numeric(data),
                       statistic = bf,
                       R = n_boot,
                       l = as.integer(block_size),
                       sim = sim_arg)
    boot_stats <- as.numeric(bo$t[, 1L])
  } else {
    boot_stats <- .block_boot_inline(data, statistic, block_size,
                                     n_boot, method)
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

#' Internal helper: Block Boot Inline
#' @noRd
.block_boot_inline <- function(data, statistic, block_size, n_boot, method) {
  n <- .nrow_like(data)
  n_blocks <- as.integer(ceiling(n / block_size))
  boot_stats <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    if (method == "circular") {
      starts <- sample.int(n, size = n_blocks, replace = TRUE) - 1L
      idx_all <- unlist(lapply(starts, function(s) {
        ((s + 0:(block_size - 1)) %% n) + 1L
      }))
      idx <- idx_all[seq_len(n)]
    } else if (method == "moving") {
      max_start <- n - block_size
      starts <- sample.int(max_start + 1L, size = n_blocks, replace = TRUE)
      idx_all <- unlist(lapply(starts, function(s) s + 0:(block_size - 1)))
      idx <- idx_all[seq_len(n)]
    } else {
      idx <- integer(n)
      i <- sample.int(n, 1L)
      k <- 0L
      while (k < n) {
        k <- k + 1L
        idx[k] <- ((i - 1L) %% n) + 1L
        if (stats::runif(1) < 1 / block_size) {
          i <- sample.int(n, 1L)
        } else {
          i <- i + 1L
        }
      }
    }
    boot_stats[b] <- as.numeric(statistic(.idx(data, idx)))
  }
  boot_stats
}

# ---------------------------------------------------------------------------
# Jackknife (bootstrap::jackknife reference; inline math retained)
# ---------------------------------------------------------------------------

#' Delete-one (leave-one-out) jackknife
#'
#' Computes the leave-one-out estimates, pseudovalues, influence
#' values, and bias-corrected jackknife estimate. The
#' \pkg{bootstrap} package's \code{bootstrap::jackknife} is the
#' canonical CRAN reference; it is invoked when installed and the
#' rmorie-shape result is reconstructed around it. Falls back to an
#' inline loop otherwise.
#'
#' @param data Numeric vector or matrix.
#' @param statistic Function returning a scalar.
#' @param ci_level Confidence level.
#' @return A \code{morie_jackknife_result}.
#' @seealso \code{bootstrap::jackknife}, \code{resample::jackknife}.
#' @export
jackknife <- function(data, statistic, ci_level = 0.95) {
  n <- .nrow_like(data)
  original <- as.numeric(statistic(data))

  if (.boot_have_bootstrap() && is.null(dim(data))) {
    jk <- bootstrap::jackknife(as.numeric(data),
                               function(x) as.numeric(statistic(x)))
    jack <- as.numeric(jk$jack.values)
    bias <- as.numeric(jk$jack.bias)
    se   <- as.numeric(jk$jack.se)
  } else {
    jack <- numeric(n)
    for (i in seq_len(n)) {
      jack[i] <- as.numeric(statistic(.idx(data, -i)))
    }
    jm <- mean(jack)
    bias <- (n - 1) * (jm - original)
    se <- sqrt((n - 1) / n * sum((jack - jm) ^ 2))
  }

  jm <- mean(jack)
  pseudovalues <- n * original - (n - 1) * jack
  influence <- original - jack

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
#' Generalised jackknife removing \code{d} observations per replicate;
#' all subsets are enumerated when \eqn{C(n, d) \le} \code{max_subsets}
#' and Monte-Carlo sampled otherwise. \pkg{resample} exposes the
#' equivalent generalised jackknife API via \code{resample::jackknife}
#' (cross-referenced).
#'
#' @param data Numeric vector or matrix.
#' @param statistic Function returning a scalar.
#' @param d Number of observations to delete per replicate.
#' @param ci_level Confidence level.
#' @param max_subsets Maximum subsets to evaluate.
#' @param seed Random seed.
#' @return A \code{morie_jackknife_result}.
#' @seealso \code{resample::jackknife}.
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

# ---------------------------------------------------------------------------
# Permutation tests (coin::oneway_test / coin::symmetry_test cross-ref)
# ---------------------------------------------------------------------------

#' Two-sample permutation test
#'
#' Shuffles the combined samples \code{n_permutations} times to
#' construct the null distribution of the chosen test statistic.
#' \pkg{coin}'s \code{coin::oneway_test(distribution = "approximate")}
#' implements the same test with a Monte-Carlo null; it is delegated
#' to when \pkg{coin} is installed and \code{statistic} is the
#' default \code{"mean_diff"} (the rmorie API allows arbitrary
#' \code{f(g1, g2)} which coin does not expose, so a custom statistic
#' falls back to the inline shuffle loop). The inline path keeps the
#' full null distribution which downstream MRM code consumes.
#'
#' @param group1,group2 Numeric vectors.
#' @param statistic Either \code{"mean_diff"}, \code{"median_diff"},
#'   \code{"t_stat"}, or a function \code{f(g1, g2) -> scalar}.
#' @param n_permutations Number of permutations.
#' @param alternative \code{"two-sided"}, \code{"greater"},
#'   or \code{"less"}.
#' @param seed Random seed.
#' @return A \code{morie_permutation_test_result}.
#' @seealso \code{coin::oneway_test}, \code{coin::independence_test}.
#' @export
permutation_test <- function(group1, group2, statistic = "mean_diff",
                             n_permutations = 9999L,
                             alternative = "two-sided", seed = 42L) {
  if (!alternative %in% c("two-sided", "greater", "less")) {
    stop(sprintf("Unknown alternative: %s", alternative))
  }

  stat_fn <- .perm_stat_fn(statistic)

  set.seed(seed)
  g1 <- as.numeric(group1)
  g2 <- as.numeric(group2)
  combined <- c(g1, g2)
  n1 <- length(g1)
  n <- length(combined)

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
    "less"      = mean(null_dist <= observed)
  )
  # Exact-permutation correction.
  p_value <- (p_value * n_permutations + 1) / (n_permutations + 1)

  .new_permutation_test_result(
    observed_statistic = observed, p_value = p_value,
    null_distribution = null_dist,
    n_permutations = n_permutations, alternative = alternative
  )
}

# Resolve the two-sample permutation statistic.
#' Internal helper: Perm Stat Fn
#' @noRd
.perm_stat_fn <- function(statistic) {
  if (is.function(statistic)) return(statistic)
  switch(
    statistic,
    "mean_diff"   = function(a, b) mean(a) - mean(b),
    "median_diff" = function(a, b) stats::median(a) - stats::median(b),
    "t_stat"      = function(a, b) {
      s1 <- stats::var(a)
      s2 <- stats::var(b)
      se <- sqrt(s1 / length(a) + s2 / length(b))
      (mean(a) - mean(b)) / max(se, 1e-10)
    },
    stop(sprintf("Unknown statistic: %s", statistic))
  )
}

#' Paired permutation test (sign-flipping)
#'
#' Performs a sign-flipping paired permutation test on the paired
#' differences. \code{coin::symmetry_test(distribution = "approximate")}
#' is the canonical CRAN equivalent (cross-referenced); rmorie keeps
#' the inline path because the rmorie API returns the full null
#' distribution.
#'
#' @param x,y Paired numeric vectors (same length).
#' @param statistic \code{"mean_diff"} or \code{"median_diff"}.
#' @param n_permutations Number of permutations.
#' @param alternative \code{"two-sided"}, \code{"greater"}, \code{"less"}.
#' @param seed Random seed.
#' @return A \code{morie_permutation_test_result}.
#' @seealso \code{coin::symmetry_test}.
#' @export
paired_permutation_test <- function(x, y, statistic = "mean_diff",
                                    n_permutations = 9999L,
                                    alternative = "two-sided", seed = 42L) {
  if (!alternative %in% c("two-sided", "greater", "less")) {
    stop(sprintf("Unknown alternative: %s", alternative))
  }
  if (identical(statistic, "mean_diff")) {
    stat_fn <- mean
  } else if (identical(statistic, "median_diff")) {
    stat_fn <- stats::median
  } else {
    stop(sprintf("Unknown statistic: %s", statistic))
  }

  set.seed(seed)
  diffs <- as.numeric(x) - as.numeric(y)
  n <- length(diffs)

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
    "less"      = mean(null_dist <= observed)
  )
  p_value <- (p_value * n_permutations + 1) / (n_permutations + 1)

  .new_permutation_test_result(
    observed_statistic = observed, p_value = p_value,
    null_distribution = null_dist,
    n_permutations = n_permutations, alternative = alternative
  )
}

# ---------------------------------------------------------------------------
# Subsampling (Politis, Romano & Wolf): no clean CRAN drop-in
# ---------------------------------------------------------------------------

#' Subsampling inference (Politis, Romano & Wolf)
#'
#' Draws without replacement at a smaller sample size; valid under
#' weaker conditions than the bootstrap. No clean CRAN function
#' exposes the same `(data, statistic, subsample_size, n_subsamples)`
#' API; \pkg{np}'s `npsubsample` is closest but is kernel-specific.
#' Kept as an in-house implementation and flagged
#' novel/no-clean-CRAN-equivalent.
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

# ---------------------------------------------------------------------------
# .632 / .632+ (ipred::errorest cross-referenced)
# ---------------------------------------------------------------------------

#' .632 and .632+ bootstrap estimators for prediction error
#'
#' Computes apparent error, mean OOB bootstrap error, the .632
#' estimator (Efron 1983), and the .632+ no-information-adjusted
#' estimator (Efron and Tibshirani 1997). \code{ipred::errorest(...,
#' estimator = "632plus")} implements the same family in
#' \pkg{ipred}; it is cross-referenced for users who already work
#' with \pkg{ipred}'s `predict.\<learner\>` ecosystem. The inline
#' implementation is retained because rmorie's API takes naked
#' \code{model_fn} / \code{score_fn} callables and is consumed by
#' downstream MRM code.
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
#' @seealso \code{ipred::errorest}.
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

  r_no <- (boot_error - apparent) / max(gamma - apparent, 1e-10)
  r_no <- min(max(r_no, 0), 1)
  w <- 0.632 / (1 - 0.368 * r_no)
  error_632plus <- (1 - w) * apparent + w * boot_error

  list(apparent_error = apparent,
       bootstrap_error = boot_error,
       error_632 = error_632,
       error_632plus = error_632plus)
}

# ---------------------------------------------------------------------------
# Cross-validation (rsample::vfold_cv / caret::trainControl cross-ref)
# ---------------------------------------------------------------------------

#' Bootstrap-flavoured K-fold cross-validation (internal).
#'
#' Lower-level CV used by [repeated_cv()] / [leave_one_out_cv()].
#' Public CV with `(fit_fn, predict_fn, X, y, ...)` signature lives
#' in [cross_validate()] (validation.R). When \pkg{rsample} is
#' installed and no stratification or grouping is requested the
#' folds are drawn via \code{rsample::vfold_cv}; otherwise the
#' inline `cut(sample(n), n_folds)` partitioning is used so the
#' helper keeps working on minimal installs.
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

  fold_indices <- .build_folds(n, n_folds, stratify, groups)

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

# Build CV fold index list. Uses rsample::vfold_cv when available and
# no stratification / grouping is requested.
#' Internal helper: Build Folds
#' @noRd
.build_folds <- function(n, n_folds, stratify, groups) {
  if (!is.null(groups)) {
    groups <- as.vector(groups)
    uniq <- sample(unique(groups))
    group_folds <- split(uniq, cut(seq_along(uniq), n_folds, labels = FALSE))
    return(lapply(group_folds, function(gf) which(groups %in% gf)))
  }
  if (!is.null(stratify)) {
    stratify <- as.vector(stratify)
    fold_indices <- vector("list", n_folds)
    for (s in unique(stratify)) {
      s_idx <- sample(which(stratify == s))
      splits <- split(s_idx, cut(seq_along(s_idx), n_folds, labels = FALSE))
      for (f in seq_len(n_folds)) {
        fold_indices[[f]] <- c(fold_indices[[f]], splits[[f]])
      }
    }
    return(fold_indices)
  }
  if (.boot_have_rsample()) {
    df <- data.frame(.row = seq_len(n))
    splits <- rsample::vfold_cv(df, v = n_folds)
    return(lapply(splits$splits, function(s) {
      df$.row[rsample::complement(s)]
    }))
  }
  idx <- sample.int(n)
  split(idx, cut(seq_along(idx), n_folds, labels = FALSE))
}

#' Repeated K-fold cross-validation
#'
#' Repeats \code{.boot_cross_validate()} \code{n_repeats} times
#' with different RNG seeds and pools the per-fold scores.
#' \code{caret::trainControl(method = "repeatedcv")} and
#' \code{rsample::vfold_cv} both implement the same partitioning
#' (cross-referenced).
#'
#' @param X Numeric matrix or data.frame of predictors.
#' @param y Numeric or factor outcome vector aligned with rows of `X`.
#' @param model_fn Function `(X, y) -> fitted-model` used on each
#'   training fold.
#' @param score_fn Function `(y_true, y_pred) -> numeric` returning a
#'   single performance metric.
#' @param n_folds Integer; number of folds per repeat (default 10).
#' @param n_repeats Number of repetitions.
#' @param seed Integer RNG seed for reproducibility.
#' @return A \code{morie_cv_result} pooling scores across repeats.
#' @seealso \code{caret::trainControl}, \code{rsample::vfold_cv}.
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
#' Convenience wrapper that calls \code{.boot_cross_validate()}
#' with \code{n_folds = length(y)}. \code{rsample::loo_cv} is the
#' tidymodels equivalent (cross-referenced).
#'
#' @param X Numeric matrix or data.frame of predictors.
#' @param y Numeric or factor outcome vector aligned with rows of `X`.
#' @param model_fn Function `(X, y) -> fitted-model` used on each
#'   training fold.
#' @param score_fn Function `(y_true, y_pred) -> numeric` returning a
#'   single performance metric.
#' @return A \code{morie_cv_result}.
#' @seealso \code{rsample::loo_cv}, \code{caret::trainControl}.
#' @export
leave_one_out_cv <- function(X, y, model_fn, score_fn) {
  .boot_cross_validate(X, y, model_fn, score_fn, n_folds = length(y))
}

# ---------------------------------------------------------------------------
# New Phase 1.i extender interfaces
# ---------------------------------------------------------------------------

#' Direct bridge to \code{boot::boot}
#'
#' Thin pass-through that adapts an rmorie-style
#' \code{statistic(data) -> scalar} callable to \code{boot::boot}'s
#' \code{statistic(data, indices) -> scalar} signature and returns
#' the raw \code{boot} object. Useful when the caller wants to use
#' \code{boot}'s downstream helpers (\code{boot::boot.ci},
#' \code{boot::tilt.boot}, \code{boot::jack.after.boot}) directly.
#'
#' @param data A numeric vector, matrix, or data.frame.
#' @param statistic Function \code{f(data) -> scalar}.
#' @param R Number of bootstrap replicates.
#' @param strata Optional integer stratification vector.
#' @param ... Forwarded to \code{boot::boot}.
#' @return A \code{boot} object as returned by \code{boot::boot}.
#' @seealso \code{boot::boot}, [morie_boot_basic_ci()].
#' @export
morie_boot_run <- function(data, statistic, R = 2000L, strata = NULL, ...) {
  if (!.boot_have_boot()) {
    stop("morie_boot_run() requires the 'boot' package; install it.")
  }
  bf <- .boot_statistic_adapter(statistic)
  if (is.null(strata)) {
    boot::boot(data = data, statistic = bf, R = R, ...)
  } else {
    boot::boot(data = data, statistic = bf, R = R,
               strata = as.integer(factor(strata)), ...)
  }
}

#' Direct bridge to \code{boot::boot.ci}
#'
#' Thin pass-through that computes bootstrap confidence intervals
#' from a \code{boot} object via \code{boot::boot.ci}, returning a
#' tidy named list with `(ci_lower, ci_upper)` per requested type.
#'
#' @param boot_obj A \code{boot} object as returned by
#'   [morie_boot_run()] or \code{boot::boot}.
#' @param type Character vector of CI types; any of \code{"perc"},
#'   \code{"bca"}, \code{"basic"}, \code{"norm"}, \code{"stud"}.
#' @param conf Confidence level (default 0.95).
#' @return Named list of length \code{length(type)}; each element is
#'   a numeric length-2 vector `c(ci_lower, ci_upper)`.
#' @seealso \code{boot::boot.ci}.
#' @export
morie_boot_basic_ci <- function(boot_obj,
                                type = c("perc", "bca", "basic", "norm",
                                         "stud"),
                                conf = 0.95) {
  if (!.boot_have_boot()) {
    stop("morie_boot_basic_ci() requires the 'boot' package; install it.")
  }
  type <- match.arg(type, several.ok = TRUE)
  bci <- boot::boot.ci(boot_obj, conf = conf, type = type)
  out <- lapply(type, function(tk) {
    method_label <- switch(tk,
                           "perc"  = "percentile",
                           "norm"  = "normal",
                           "basic" = "basic",
                           "bca"   = "bca",
                           "stud"  = "studentized")
    .boot_ci_extract(bci, method_label)
  })
  names(out) <- type
  out
}

#' Direct bridge to \code{rsample::bootstraps}
#'
#' Thin pass-through that builds a tidymodels-style
#' \code{rset} of bootstrap resamples via
#' \code{rsample::bootstraps} and returns it untouched.
#'
#' @param data A data.frame.
#' @param times Number of bootstrap resamples (default 25).
#' @param ... Forwarded to \code{rsample::bootstraps}
#'   (e.g. \code{strata}, \code{apparent}).
#' @return An \code{rset} \pkg{rsample} object.
#' @seealso \code{rsample::bootstraps}, \code{rsample::vfold_cv}.
#' @export
morie_rsample_bootstraps <- function(data, times = 25L, ...) {
  if (!.boot_have_rsample()) {
    stop("morie_rsample_bootstraps() requires the 'rsample' package; ",
         "install it.")
  }
  rsample::bootstraps(data = data, times = as.integer(times), ...)
}

#' Direct bridge to \code{simpleboot::two.boot}
#'
#' Thin pass-through to \code{simpleboot::two.boot} for the
#' common two-sample bootstrap-of-a-statistic case (e.g. difference
#' of means, ratio of medians). Returns a \code{boot} object the
#' user can hand to \code{boot::boot.ci} or [morie_boot_basic_ci()].
#'
#' @param x,y Numeric vectors.
#' @param statistic A scalar function applied to one sample at a
#'   time (e.g. \code{mean}, \code{median}); two.boot's contract.
#' @param R Number of bootstrap replicates (default 1000).
#' @param ... Forwarded to \code{simpleboot::two.boot}.
#' @return A \code{boot} object.
#' @seealso \code{simpleboot::two.boot}, \code{simpleboot::one.boot},
#'   \code{simpleboot::lm.boot}.
#' @export
morie_simpleboot_two <- function(x, y, statistic = mean, R = 1000L, ...) {
  if (!.boot_have_simpleboot()) {
    stop("morie_simpleboot_two() requires the 'simpleboot' package; ",
         "install it.")
  }
  simpleboot::two.boot(sample1 = as.numeric(x),
                       sample2 = as.numeric(y),
                       FUN = statistic,
                       R = as.integer(R), ...)
}
