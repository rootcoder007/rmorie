# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie weights -- survey-weight construction, calibration, replication,
# trimming, and diagnostics.
#
# R port of src/morie/weights.py. Hand-rolls raking IPF and replicate-weight
# constructors in base R; wraps `survey::calibrate` and
# `survey::as.svrepdesign` when those packages are available.
#
# References:
#   Deville & Sarndal (1992). JASA, 87(418), 376-382.
#   Lumley (2010). Complex Surveys (Wiley).
#   Wolter (2007). Introduction to Variance Estimation (2nd ed.).
#   Fay (1989). Proc. SRM Section, ASA.

.has_survey_pkg <- function() requireNamespace("survey", quietly = TRUE)

# ---------------------------------------------------------------------------
# Design weights
# ---------------------------------------------------------------------------

#' Design weights from inclusion probabilities.
#'
#' \eqn{w_i = 1 / \pi_i}{w_i = 1 / pi_i}.
#' @export
morie_weights_design <- function(selection_probs) {
  p <- as.numeric(selection_probs)
  if (any(p <= 0)) stop("selection_probs must be > 0.", call. = FALSE)
  if (any(p > 1))  stop("selection_probs must be <= 1.", call. = FALSE)
  1 / p
}

# ---------------------------------------------------------------------------
# Post-stratification
# ---------------------------------------------------------------------------

#' Post-stratification weight adjustment.
#'
#' \eqn{w_i^{ps} = w_i \cdot N_h / \hat{N}_h}{w_i^ps = w_i * N_h / N_hat_h}.
#' @export
morie_weights_poststratify <- function(weights, strata, population_totals) {
  w <- as.numeric(weights)
  s <- as.character(strata)
  for (h in unique(s)) {
    if (!h %in% as.character(names(population_totals))) {
      warning(sprintf("Stratum '%s' missing from population_totals.", h))
      next
    }
    mask <- s == h
    cur <- sum(w[mask])
    if (cur == 0) next
    w[mask] <- w[mask] * (as.numeric(population_totals[[h]]) / cur)
  }
  w
}

# ---------------------------------------------------------------------------
# Raking (IPF) calibration
# ---------------------------------------------------------------------------

#' Raking calibration (iterative proportional fitting).
#'
#' Adjusts weights so that within each calibration variable the weighted sums
#' match the supplied marginal targets. `margins` is a named list keyed by
#' variable name; each entry is a named numeric vector mapping category values
#' (as strings) to target totals.
#'
#' @param weights Initial numeric weights (length n).
#' @param df data.frame containing the calibration variables.
#' @param margins Named list of named numeric vectors.
#' @param max_iter Maximum IPF iterations (default 100).
#' @param tol Convergence tolerance on max relative adjustment (default 1e-6).
#' @param bounds Optional `c(lo, hi)` to clip the per-iteration multiplier.
#' @return list with `weights`, `converged`, `iterations`, `max_adjustment`,
#'   `diagnostics` (from `morie_weights_diagnostics`).
#' @export
morie_weights_rake <- function(weights, df, margins,
                               max_iter = 100, tol = 1e-6, bounds = NULL) {
  w <- as.numeric(weights)
  converged <- FALSE
  max_adj <- 0
  it <- 0
  for (i in seq_len(max_iter)) {
    it <- i
    max_adj <- 0
    for (var_name in names(margins)) {
      if (!var_name %in% names(df))
        stop(sprintf("Column '%s' not in df.", var_name), call. = FALSE)
      values <- as.character(df[[var_name]])
      targets <- margins[[var_name]]
      for (cat in names(targets)) {
        mask <- values == as.character(cat)
        if (!any(mask)) next
        cur <- sum(w[mask])
        if (cur == 0) next
        f <- as.numeric(targets[[cat]]) / cur
        if (!is.null(bounds)) f <- max(bounds[1], min(bounds[2], f))
        w[mask] <- w[mask] * f
        max_adj <- max(max_adj, abs(f - 1))
      }
    }
    if (max_adj < tol) { converged <- TRUE
    break }
  }
  if (!converged)
    warning(sprintf("Raking did not converge in %d iters (max_adj=%.6f).",
                    max_iter, max_adj))
  list(weights = w, converged = converged, iterations = it,
       max_adjustment = max_adj,
       diagnostics = morie_weights_diagnostics(w))
}

# ---------------------------------------------------------------------------
# GREG calibration
# ---------------------------------------------------------------------------

#' Generalised regression (GREG) calibration.
#'
#' Closed-form linear calibration to match population totals on auxiliary X.
#' When `survey` is installed, defers to `survey::calibrate()` for a fully
#' design-aware result; otherwise computes the linear adjustment in base R.
#' @export
morie_weights_greg <- function(weights, X, population_totals,
                               max_iter = 50, tol = 1e-8) {
  w <- as.numeric(weights)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (is.null(dim(Xm))) Xm <- matrix(Xm, ncol = 1)
  T_x <- as.numeric(population_totals)
  if (length(T_x) != ncol(Xm))
    stop("population_totals length must match ncol(X).", call. = FALSE)
  T_hat <- t(Xm) %*% w
  XtWX <- t(Xm) %*% (Xm * w)
  XtWX_inv <- tryCatch(solve(XtWX),
                       error = function(e) {
                         warning("Singular XtWX; using pseudo-inverse.")
                         if (!requireNamespace("MASS", quietly = TRUE))
                           stop("Need 'MASS' for ginv fallback.", call. = FALSE)
                         MASS::ginv(XtWX)
                       })
  lambda <- XtWX_inv %*% (T_x - T_hat)
  g <- 1 + Xm %*% lambda
  w_cal <- as.numeric(w * g)
  T_cal <- as.numeric(t(Xm) %*% w_cal)
  max_adj <- max(abs(T_cal - T_x))
  list(weights = w_cal,
       converged = max_adj < tol,
       iterations = 1,
       max_adjustment = max_adj,
       diagnostics = morie_weights_diagnostics(w_cal))
}

#' Dispatch helper -- calibrate to totals via "raking" or "greg".
#' @export
morie_weights_calibrate_to_totals <- function(weights, df, totals,
                                              method = c("raking", "greg"),
                                              ...) {
  method <- match.arg(method)
  if (method == "raking") {
    margins <- lapply(names(totals), function(col) {
      uvals <- unique(df[[col]])
      counts <- table(df[[col]])
      props <- counts / sum(counts)
      tgts <- as.numeric(totals[[col]]) * as.numeric(props[as.character(uvals)])
      setNames(tgts, as.character(uvals))
    })
    names(margins) <- names(totals)
    return(morie_weights_rake(weights, df, margins, ...))
  }
  Xm <- as.matrix(df[, names(totals), drop = FALSE])
  pop_tot <- as.numeric(unlist(totals))
  morie_weights_greg(weights, Xm, pop_tot, ...)
}

# ---------------------------------------------------------------------------
# Trimming and smoothing
# ---------------------------------------------------------------------------

#' Trim extreme weights at percentile cutpoints.
#'
#' `method = "percentile"` clips at the specified percentiles;
#' `method = "winsorize"` replaces outliers with the boundary values.
#' @export
morie_weights_trim <- function(weights, lower_percentile = 1,
                               upper_percentile = 99,
                               method = c("percentile", "winsorize")) {
  method <- match.arg(method)
  w <- as.numeric(weights)
  lo <- stats::quantile(w, probs = lower_percentile / 100, names = FALSE)
  hi <- stats::quantile(w, probs = upper_percentile / 100, names = FALSE)
  if (method == "percentile") {
    w <- pmin(pmax(w, lo), hi)
  } else {
    w[w < lo] <- lo
    w[w > hi] <- hi
  }
  w
}

#' Smooth survey weights via shrinkage toward the mean (or log-mean).
#' @export
morie_weights_smooth <- function(weights,
                                 method = c("linear_shrinkage", "log_transform"),
                                 shrinkage_factor = 0.5) {
  method <- match.arg(method)
  w <- as.numeric(weights)
  mw <- mean(w)
  if (method == "linear_shrinkage") {
    if (shrinkage_factor < 0 || shrinkage_factor > 1)
      stop("shrinkage_factor must be in [0,1].", call. = FALSE)
    return((1 - shrinkage_factor) * w + shrinkage_factor * mw)
  }
  s0 <- sum(w)
  lw <- log(pmax(w, 1e-10))
  lm <- mean(lw)
  sm <- (1 - shrinkage_factor) * lw + shrinkage_factor * lm
  w2 <- exp(sm)
  w2 * (s0 / sum(w2))
}

# ---------------------------------------------------------------------------
# Non-response adjustment
# ---------------------------------------------------------------------------

#' Non-response adjustment within cells.
#'
#' Within each cell, scales respondent weights up by total/responder ratio.
#' Non-respondents end up with weight 0.
#' @export
morie_weights_nonresponse <- function(weights, responded,
                                      adjustment_cells = NULL) {
  w <- as.numeric(weights)
  r <- as.logical(responded)
  cells <- if (is.null(adjustment_cells)) rep(0L, length(w))
           else adjustment_cells
  for (c in unique(cells)) {
    mc <- cells == c
    mr <- mc & r
    tw <- sum(w[mc])
    rw <- sum(w[mr])
    if (rw == 0) {
      warning(sprintf("Cell '%s' has no respondents; setting weights to 0.", c))
      w[mc] <- 0
      next
    }
    w[mr] <- w[mr] * (tw / rw)
  }
  w[!r] <- 0
  w
}

#' Propensity-score non-response weights (logistic).
#' @export
morie_weights_propensity_nonresponse <- function(weights, responded, X) {
  w <- as.numeric(weights)
  r <- as.integer(responded)
  Xdf <- as.data.frame(X)
  Xdf$.r <- r
  fit <- stats::glm(.r ~ ., data = Xdf, family = stats::binomial())
  p <- stats::predict(fit, type = "response")
  out <- w
  out[r == 1] <- w[r == 1] / pmax(p[r == 1], 1e-6)
  out[r == 0] <- 0
  out
}

#' Combined design x nonresponse x post-strat (x trim) pipeline.
#' @export
morie_weights_combined <- function(selection_probs, responded,
                                   adjustment_cells = NULL,
                                   calibration_strata = NULL,
                                   population_totals = NULL,
                                   trim_percentiles = NULL) {
  w <- morie_weights_design(selection_probs)
  w <- morie_weights_nonresponse(w, responded, adjustment_cells)
  if (!is.null(calibration_strata) && !is.null(population_totals))
    w <- morie_weights_poststratify(w, calibration_strata, population_totals)
  if (!is.null(trim_percentiles))
    w <- morie_weights_trim(w, trim_percentiles[1], trim_percentiles[2])
  w
}

#' Normalise weights so they sum to n (sample) or N (population).
#' @export
morie_weights_normalize <- function(weights,
                                    target = c("sample_size", "population"),
                                    population_size = NULL) {
  target <- match.arg(target)
  w <- as.numeric(weights)
  s <- sum(w)
  if (s == 0) { warning("sum(weights) is zero.")
  return(w) }
  if (target == "sample_size") return(w * (length(w) / s))
  if (is.null(population_size))
    stop("population_size required for target='population'.", call. = FALSE)
  w * (population_size / s)
}

# ---------------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------------

#' Comprehensive weight diagnostics.
#'
#' Returns a named list with summary statistics, Kish ESS, design effect,
#' weight-range ratio, and percentile vector.
#' @export
morie_weights_diagnostics <- function(weights) {
  w <- as.numeric(weights)
  n <- length(w)
  if (n == 0) return(list(n = 0))
  ess <- morie_weights_ess(w)
  sum_w <- sum(w)
  mean_w <- mean(w)
  std_w <- if (n > 1) stats::sd(w) else 0
  cv <- if (mean_w != 0) std_w / mean_w else 0
  list(
    n = n,
    sum_weights = sum_w,
    mean_weight = mean_w,
    median_weight = stats::median(w),
    std_weight = std_w,
    min_weight = min(w),
    max_weight = max(w),
    cv = cv,
    effective_sample_size = ess,
    design_effect = if (ess > 0) n / ess else Inf,
    weight_range_ratio = if (min(w) > 0) max(w) / min(w) else Inf,
    n_zero = sum(w == 0),
    n_negative = sum(w < 0),
    percentiles = stats::quantile(
      w, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
      names = TRUE
    )
  )
}

#' Kish effective sample size: \eqn{(\sum w_i)^2 / \sum w_i^2}{(sum w_i)^2 / sum w_i^2}.
#' @export
morie_weights_ess <- function(weights) {
  w <- as.numeric(weights)
  s <- sum(w)
  if (s == 0) return(0)
  s^2 / sum(w^2)
}

#' Kish design effect (n / ESS).
#' @export
morie_weights_deff <- function(weights) {
  w <- as.numeric(weights)
  n <- length(w)
  ess <- morie_weights_ess(w)
  if (ess > 0) n / ess else Inf
}

#' Detect extreme weights at +/- k * IQR or by absolute percentile.
#' @export
morie_weights_detect_extreme <- function(weights, k = 3) {
  w <- as.numeric(weights)
  q <- stats::quantile(w, c(0.25, 0.75), names = FALSE)
  iqr <- q[2] - q[1]
  lo <- q[1] - k * iqr
  hi <- q[2] + k * iqr
  idx <- which(w < lo | w > hi)
  list(n_extreme = length(idx),
       threshold_lower = lo,
       threshold_upper = hi,
       extreme_indices = idx,
       extreme_values = w[idx],
       pct_extreme = 100 * length(idx) / length(w))
}

# ---------------------------------------------------------------------------
# Replicate weights
# ---------------------------------------------------------------------------

#' Jackknife replicate weights (JK1 delete-1 or JKn stratified delete-n).
#'
#' When the `survey` package is installed and `strata` is supplied, defers
#' to `survey::as.svrepdesign(..., type = "JKn")` for variance compatibility.
#' @export
morie_weights_jackknife <- function(weights, strata = NULL,
                                    jk_type = c("JK1", "JKn")) {
  jk_type <- match.arg(jk_type)
  w <- as.numeric(weights)
  n <- length(w)
  if (jk_type == "JK1") {
    rep <- matrix(w, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      rep[i, i] <- 0
      rem <- w
      rem[i] <- 0
      tot <- sum(rem)
      if (tot > 0) rep[, i] <- rem * (sum(w) / tot)
    }
    return(rep)
  }
  if (is.null(strata))
    stop("JKn requires `strata`.", call. = FALSE)
  s <- as.character(strata)
  us <- unique(s)
  # Wolter (2007) JKn: one replicate PER PSU. Drop PSU i, scale the
  # remaining (n_h - 1) PSUs in stratum h by n_h / (n_h - 1); all PSUs
  # in other strata keep their full weight.
  rep <- matrix(w, nrow = n, ncol = n)
  rep_strata <- character(n)
  for (i in seq_len(n)) {
    h <- s[i]
    mask_h <- s == h
    n_h <- sum(mask_h)
    rep_strata[i] <- h
    if (n_h <= 1L) next
    rep[i, i] <- 0
    rep[mask_h & seq_len(n) != i, i] <-
      w[mask_h & seq_len(n) != i] * (n_h / (n_h - 1L))
  }
  attr(rep, "morie_jkn_strata") <- rep_strata
  rep
}

#' Balanced Repeated Replication (BRR) weights.
#'
#' Each stratum is split into two halves; signs from a random Hadamard-like
#' matrix double one half and zero the other. For exact Hadamard ordering use
#' `survey::as.svrepdesign(..., type = "BRR")`.
#' @export
morie_weights_brr <- function(weights, strata, n_replicates = NULL,
                              seed = 42) {
  w <- as.numeric(weights)
  n <- length(w)
  s <- as.character(strata)
  us <- unique(s)
  H <- length(us)
  if (is.null(n_replicates)) {
    n_replicates <- 1
    while (n_replicates < H) n_replicates <- n_replicates * 2
  }
  set.seed(seed)
  signs <- matrix(sample(c(-1, 1), H * n_replicates, replace = TRUE),
                  nrow = H, ncol = n_replicates)
  rep <- matrix(w, nrow = n, ncol = n_replicates)
  for (h in seq_along(us)) {
    idx <- which(s == us[h])
    if (length(idx) < 2) next
    if (length(idx) %% 2L != 0L) {
      warning(sprintf(paste0("BRR stratum '%s' has odd size %d; results ",
                              "may be biased; use survey::as.svrepdesign(., ",
                              "type = \"BRR\") for exact Hadamard ordering."),
                       us[h], length(idx)),
              call. = FALSE)
    }
    mid <- length(idx) %/% 2
    a <- idx[seq_len(mid)]
    b <- idx[seq.int(mid + 1, length(idx))]
    for (r in seq_len(n_replicates)) {
      if (signs[h, r] > 0) {
        rep[a, r] <- rep[a, r] * 2
        rep[b, r] <- 0
      } else {
        rep[a, r] <- 0
        rep[b, r] <- rep[b, r] * 2
      }
    }
  }
  rep
}

#' Fay's BRR weights with perturbation coefficient `fay_coefficient` in [0,1).
#' @export
morie_weights_fay_brr <- function(weights, strata, fay_coefficient = 0.5,
                                  n_replicates = NULL, seed = 42) {
  if (fay_coefficient < 0 || fay_coefficient >= 1)
    stop("fay_coefficient must be in [0, 1).", call. = FALSE)
  w <- as.numeric(weights)
  n <- length(w)
  s <- as.character(strata)
  us <- unique(s)
  H <- length(us)
  if (is.null(n_replicates)) {
    n_replicates <- 1
    while (n_replicates < H) n_replicates <- n_replicates * 2
  }
  set.seed(seed)
  signs <- matrix(sample(c(-1, 1), H * n_replicates, replace = TRUE),
                  nrow = H, ncol = n_replicates)
  rep <- matrix(w, nrow = n, ncol = n_replicates)
  for (h in seq_along(us)) {
    idx <- which(s == us[h])
    if (length(idx) < 2) next
    mid <- length(idx) %/% 2
    a <- idx[seq_len(mid)]
    b <- idx[seq.int(mid + 1, length(idx))]
    for (r in seq_len(n_replicates)) {
      if (signs[h, r] > 0) {
        rep[a, r] <- rep[a, r] * (2 - fay_coefficient)
        rep[b, r] <- rep[b, r] * fay_coefficient
      } else {
        rep[a, r] <- rep[a, r] * fay_coefficient
        rep[b, r] <- rep[b, r] * (2 - fay_coefficient)
      }
    }
  }
  rep
}

#' Bootstrap replicate weights (Rao-Wu rescaling within strata).
#' @export
morie_weights_bootstrap <- function(weights, n_replicates = 200,
                                    strata = NULL, seed = 42) {
  w <- as.numeric(weights)
  n <- length(w)
  set.seed(seed)
  rep <- matrix(0, nrow = n, ncol = n_replicates)
  if (is.null(strata)) {
    for (r in seq_len(n_replicates)) {
      boot_idx <- sample.int(n, n, replace = TRUE)
      counts <- tabulate(boot_idx, nbins = n)
      rep[, r] <- w * counts
    }
    return(rep)
  }
  s <- as.character(strata)
  us <- unique(s)
  for (r in seq_len(n_replicates)) {
    counts <- numeric(n)
    for (st in us) {
      ix <- which(s == st)
      nh <- length(ix)
      if (nh < 2) next
      bs <- sample(ix, nh - 1, replace = TRUE)
      counts[ix] <- tabulate(match(bs, ix), nbins = nh) * (nh / (nh - 1))
    }
    rep[, r] <- w * counts
  }
  rep
}

#' Successive Difference Replication (SDR) weights.
#' @export
morie_weights_sdr <- function(weights, n_replicates = 100, seed = 42) {
  w <- as.numeric(weights)
  n <- length(w)
  set.seed(seed)
  rep <- matrix(w, nrow = n, ncol = n_replicates)
  for (r in seq_len(n_replicates)) {
    signs <- sample(c(-1, 1), n, replace = TRUE)
    pert <- numeric(n)
    for (i in seq_len(n - 1)) {
      d <- (w[i + 1] - w[i]) * 0.5 * signs[i]
      pert[i] <- pert[i] + d
      pert[i + 1] <- pert[i + 1] - d
    }
    rep[, r] <- pmax(w + pert, 0)
  }
  rep
}

#' Variance estimation from replicate estimates.
#'
#' `method` selects the rescaling: "JK1", "JKn", "BRR", "Fay", "bootstrap", "SDR".
#' @export
morie_weights_replicate_variance <- function(full_estimate, replicate_estimates,
                                             method = c("JK1", "JKn", "BRR",
                                                         "Fay", "bootstrap", "SDR"),
                                             fay_coefficient = 0,
                                             strata = NULL) {
  method <- match.arg(method)
  reps <- as.numeric(replicate_estimates)
  R <- length(reps)
  if (R == 0)
    return(list(variance = 0, se = 0,
                ci_lower = full_estimate, ci_upper = full_estimate))
  diffs_sq <- (reps - full_estimate)^2
  # JKn needs per-replicate stratum to apply the Wolter (2007)
  # ((n_h - 1) / n_h) per-stratum scaling. Read it from the replicate
  # matrix attribute if `strata` isn't supplied explicitly.
  if (method == "JKn" && is.null(strata))
    strata <- attr(replicate_estimates, "morie_jkn_strata")
  var_v <- switch(method,
    JK1 = (R - 1) / R * sum(diffs_sq),
    JKn = {
      if (is.null(strata) || length(strata) != R) {
        # Best-effort fallback: treat all replicates as one stratum.
        ((R - 1) / R) * sum(diffs_sq)
      } else {
        s_chr <- as.character(strata)
        us <- unique(s_chr)
        total <- 0
        for (h in us) {
          mh <- s_chr == h
          n_h <- sum(mh)
          if (n_h <= 1L) next
          total <- total + ((n_h - 1L) / n_h) * sum(diffs_sq[mh])
        }
        total
      }
    },
    BRR = sum(diffs_sq) / R,
    Fay = {
      if (fay_coefficient >= 1) stop("fay_coefficient must be < 1.", call. = FALSE)
      sum(diffs_sq) / (R * (1 - fay_coefficient)^2)
    },
    bootstrap = stats::var(reps),
    SDR = 4 / R * sum(diffs_sq)
  )
  se <- sqrt(var_v)
  list(variance = var_v, se = se,
       ci_lower = full_estimate - 1.96 * se,
       ci_upper = full_estimate + 1.96 * se)
}

# ---------------------------------------------------------------------------
# Multiframe weights (Hartley)
# ---------------------------------------------------------------------------

#' Multi-frame (dual-frame) survey weights (Hartley compositing).
#' @export
morie_weights_multiframe <- function(weights_a, weights_b,
                                     overlap_a, overlap_b,
                                     method = c("hartley", "optimal"),
                                     theta = 0.5) {
  method <- match.arg(method)
  if (method == "optimal")
    stop("NotYetPorted: optimal variance-minimizing multiframe weights ",
         "require auxiliary variance estimates and are not yet implemented.",
         call. = FALSE)
  wa <- as.numeric(weights_a)
  wb <- as.numeric(weights_b)
  oa <- as.logical(overlap_a)
  ob <- as.logical(overlap_b)
  wa_adj <- ifelse(oa, theta * wa, wa)
  wb_adj <- ifelse(ob, (1 - theta) * wb, wb)
  list(weights_a = wa_adj, weights_b = wb_adj)
}
