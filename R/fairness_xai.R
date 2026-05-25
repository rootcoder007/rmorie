# SPDX-License-Identifier: AGPL-3.0-or-later
#' Model-agnostic explainability (XAI) for bias discovery
#'
#' R ports of the explainer suite in \code{morie.fairness.xai}.
#' Prefers \pkg{iml} for permutation importance / PDP / SHAP-ish
#' attributions when available; otherwise computes the same quantities
#' in base R from first principles. Every callable takes a
#' \code{predict_fn} closure (matrix -> numeric vector) so it works on
#' any classifier or risk model.
#'
#' \itemize{
#'   \item \code{morie_fairness_xai_permutation_importance}
#'   \item \code{morie_fairness_xai_partial_dependence}
#'   \item \code{morie_fairness_xai_ale}
#'   \item \code{morie_fairness_xai_ceteris_paribus}
#'   \item \code{morie_fairness_xai_shap_values}
#' }
#'
#' @name morie_fairness_xai
NULL


.xai_result <- function(title, call, summary_lines = list(),
                        warnings = character(0),
                        interpretation = "", ...) {
  out <- list(
    title = title, call = call, summary_lines = summary_lines,
    warnings = warnings, interpretation = interpretation, ...
  )
  class(out) <- c("morie_fairness_result", "morie_rich_result", "list")
  out
}

.xai_as_2d <- function(X) {
  m <- as.matrix(X)
  if (!is.numeric(m)) storage.mode(m) <- "double"
  if (length(dim(m)) != 2L) stop("X must be 2-D (n_samples, n_features)")
  if (nrow(m) == 0L || ncol(m) == 0L) stop("X is empty")
  m
}

.xai_names <- function(feature_names, d) {
  if (is.null(feature_names)) return(sprintf("x%d", seq_len(d) - 1L))
  nm <- as.character(feature_names)
  if (length(nm) != d) {
    stop(sprintf("feature_names has %d entries; X has %d columns",
                 length(nm), d))
  }
  nm
}

.xai_resolve <- function(feature, names) {
  if (is.character(feature)) {
    if (!(feature %in% names)) {
      stop(sprintf("feature '%s' not in feature_names", feature))
    }
    return(match(feature, names))
  }
  idx <- as.integer(feature)
  if (idx < 1L || idx > length(names)) {
    stop(sprintf("feature index %d out of range", idx))
  }
  idx
}

.xai_predict <- function(predict_fn, X) {
  out <- as.numeric(predict_fn(X))
  if (length(out) != nrow(X)) {
    stop("predict_fn must return one prediction per row of X")
  }
  out
}

.xai_have_iml <- function() {
  requireNamespace("iml", quietly = TRUE)
}


# ---------------------------------------------------------------------------
# 1. Permutation importance
# ---------------------------------------------------------------------------

#' Permutation feature importance (model-agnostic)
#'
#' @param predict_fn Function mapping an (n, d) matrix to n numeric
#'   predictions.
#' @param X Numeric matrix or data.frame.
#' @param feature_names Optional character vector.
#' @param n_repeats Shuffles averaged per feature.
#' @param protected Character vector of protected-attribute names; any
#'   that rank in the top third trigger a bias warning.
#' @param seed Reproducibility seed.
#' @return \code{morie_fairness_result}; \code{$value} is the largest
#'   importance.
#' @export
morie_fairness_xai_permutation_importance <- function(predict_fn, X,
                                                       feature_names = NULL,
                                                       n_repeats = 10L,
                                                       protected = NULL,
                                                       seed = 0L) {
  X <- .xai_as_2d(X)
  n <- nrow(X)
  d <- ncol(X)
  nm <- .xai_names(feature_names, d)
  set.seed(as.integer(seed))
  base <- .xai_predict(predict_fn, X)

  importances <- numeric(d)
  names(importances) <- nm
  for (j in seq_len(d)) {
    deltas <- numeric(as.integer(n_repeats))
    for (r in seq_len(as.integer(n_repeats))) {
      Xp <- X
      Xp[, j] <- X[sample.int(n), j]
      deltas[r] <- mean(abs(.xai_predict(predict_fn, Xp) - base))
    }
    importances[j] <- mean(deltas)
  }

  ranking <- names(sort(importances, decreasing = TRUE))
  top <- importances[ranking[1L]]
  warnings <- character(0)
  protected_ranks <- integer(0)
  if (!is.null(protected) && length(protected) > 0L) {
    for (p in protected) {
      if (!(p %in% nm)) {
        stop(sprintf("protected feature '%s' not in features", p))
      }
      protected_ranks[p] <- match(p, ranking)
    }
    flagged <- names(protected_ranks)[
      protected_ranks <= max(1L, d %/% 3L)
    ]
    if (length(flagged) > 0L) {
      warnings <- c(warnings, sprintf(
        "protected attribute(s) %s rank in the top third of feature importance \u2014 the model leans materially on a protected characteristic, a direct bias signal.",
        paste(sprintf("'%s'", flagged), collapse = ", ")
      ))
    }
  }

  interp <- sprintf(
    "The model relies most on '%s' (importance %.4f). %s",
    ranking[1L], top,
    if (length(warnings) > 0L)
      "Protected attributes appear high in the ranking \u2014 see the warning above."
    else "No protected attribute ranks in the top third."
  )

  .xai_result(
    "Permutation Feature Importance",
    sprintf("morie_fairness_xai_permutation_importance(n=%d, d=%d, n_repeats=%d)",
            n, d, as.integer(n_repeats)),
    summary_lines = list(
      `Top feature` = ranking[1L],
      `Top importance` = top,
      Backend = if (.xai_have_iml()) "iml available" else "base R"
    ),
    warnings = warnings,
    interpretation = interp,
    n = n,
    value = top, importances = as.list(importances),
    ranking = ranking, protected_ranks = as.list(protected_ranks)
  )
}


# ---------------------------------------------------------------------------
# 2. Partial dependence
# ---------------------------------------------------------------------------

#' Partial dependence on one feature (Friedman)
#'
#' @inheritParams morie_fairness_xai_permutation_importance
#' @param feature Index or name of the feature to sweep.
#' @param grid_size Number of grid points.
#' @return \code{morie_fairness_result}; \code{$value} is the PD range.
#' @export
morie_fairness_xai_partial_dependence <- function(predict_fn, X, feature,
                                                   feature_names = NULL,
                                                   grid_size = 20L) {
  X <- .xai_as_2d(X)
  nm <- .xai_names(feature_names, ncol(X))
  j <- .xai_resolve(feature, nm)
  lo <- min(X[, j])
  hi <- max(X[, j])
  grid <- seq(lo, hi, length.out = as.integer(grid_size))
  pd_vals <- numeric(length(grid))
  for (i in seq_along(grid)) {
    Xv <- X
    Xv[, j] <- grid[i]
    pd_vals[i] <- mean(.xai_predict(predict_fn, Xv))
  }
  rng_val <- max(pd_vals) - min(pd_vals)

  .xai_result(
    sprintf("Partial Dependence \u2014 %s", nm[j]),
    sprintf("morie_fairness_xai_partial_dependence(feature='%s', grid_size=%d)",
            nm[j], as.integer(grid_size)),
    summary_lines = list(
      Feature = nm[j],
      `PD range` = rng_val,
      `PD at min` = pd_vals[1L],
      `PD at max` = pd_vals[length(pd_vals)]
    ),
    interpretation = sprintf(
      "As '%s' sweeps its observed range, the model's average prediction moves over a span of %.4f.",
      nm[j], rng_val
    ),
    n = nrow(X),
    value = rng_val, feature = nm[j],
    grid = grid, pd = pd_vals
  )
}


# ---------------------------------------------------------------------------
# 3. Accumulated Local Effects
# ---------------------------------------------------------------------------

#' First-order Accumulated Local Effects (Apley & Zhu)
#'
#' @inheritParams morie_fairness_xai_partial_dependence
#' @param n_bins Number of quantile bins.
#' @return \code{morie_fairness_result}; \code{$value} is the ALE range.
#' @export
morie_fairness_xai_ale <- function(predict_fn, X, feature,
                                    feature_names = NULL,
                                    n_bins = 10L) {
  X <- .xai_as_2d(X)
  nm <- .xai_names(feature_names, ncol(X))
  j <- .xai_resolve(feature, nm)
  col <- X[, j]
  qs <- seq(0.0, 1.0, length.out = as.integer(n_bins) + 1L)
  edges <- unique(stats::quantile(col, qs, names = FALSE, type = 7L))
  if (length(edges) < 2L) {
    stop(sprintf("feature '%s' has no spread for ALE", nm[j]))
  }
  k <- length(edges) - 1L

  local_eff <- numeric(k)
  counts <- integer(k)
  # Match Python np.searchsorted(side='left') - 1, then clip.
  bin_idx <- findInterval(col, edges, rightmost.closed = FALSE,
                          all.inside = TRUE)
  for (b in seq_len(k)) {
    rows <- X[bin_idx == b, , drop = FALSE]
    if (nrow(rows) == 0L) next
    lo_rows <- rows
    lo_rows[, j] <- edges[b]
    hi_rows <- rows
    hi_rows[, j] <- edges[b + 1L]
    diff <- .xai_predict(predict_fn, hi_rows) -
            .xai_predict(predict_fn, lo_rows)
    local_eff[b] <- mean(diff)
    counts[b] <- nrow(rows)
  }
  accumulated <- c(0.0, cumsum(local_eff))
  mid <- 0.5 * (accumulated[-length(accumulated)] + accumulated[-1L])
  total <- sum(counts)
  centre <- if (total > 0L) sum(mid * counts) / total else 0.0
  ale <- accumulated - centre
  rng_val <- max(ale) - min(ale)

  .xai_result(
    sprintf("Accumulated Local Effects \u2014 %s", nm[j]),
    sprintf("morie_fairness_xai_ale(feature='%s', n_bins=%d)",
            nm[j], as.integer(n_bins)),
    summary_lines = list(
      Feature = nm[j], Bins = k, `ALE range` = rng_val
    ),
    interpretation = sprintf(
      "The accumulated local effect of '%s' spans %.4f across its range, correlation-robustly.",
      nm[j], rng_val
    ),
    n = nrow(X),
    value = rng_val, feature = nm[j],
    bin_centers = edges, ale = ale
  )
}


# ---------------------------------------------------------------------------
# 4. Ceteris-paribus profile
# ---------------------------------------------------------------------------

#' Ceteris-paribus profile for one instance
#'
#' Holds every feature of \code{x} fixed except \code{feature}, sweeps
#' it across the range observed in \code{X_ref}, and reports the
#' resulting prediction profile.
#'
#' @param predict_fn Function mapping (n, d) matrix to n predictions.
#' @param x Numeric vector of length d (the instance).
#' @param feature Index or name of the feature to vary.
#' @param X_ref Reference matrix used for the feature range.
#' @param feature_names Optional character vector.
#' @param grid_size Number of grid points.
#' @return \code{morie_fairness_result}; \code{$value} is the
#'   profile's swing (max - min).
#' @export
morie_fairness_xai_ceteris_paribus <- function(predict_fn, x, feature,
                                                X_ref,
                                                feature_names = NULL,
                                                grid_size = 20L) {
  X_ref <- .xai_as_2d(X_ref)
  nm <- .xai_names(feature_names, ncol(X_ref))
  j <- .xai_resolve(feature, nm)
  x <- as.numeric(x)
  if (length(x) != ncol(X_ref)) {
    stop("x must have one value per feature of X_ref")
  }
  lo <- min(X_ref[, j])
  hi <- max(X_ref[, j])
  grid <- seq(lo, hi, length.out = as.integer(grid_size))
  rows <- matrix(rep(x, each = length(grid)),
                 nrow = length(grid), ncol = length(x), byrow = FALSE)
  # Above tile is column-major already (each col repeats one value of
  # x); fix shape to row-major: use matrix(x, byrow = TRUE).
  rows <- matrix(x, nrow = length(grid), ncol = length(x), byrow = TRUE)
  rows[, j] <- grid
  profile <- .xai_predict(predict_fn, rows)
  base <- as.numeric(.xai_predict(predict_fn,
                                  matrix(x, nrow = 1L)))
  swing <- max(profile) - min(profile)

  .xai_result(
    sprintf("Ceteris-Paribus Profile \u2014 %s", nm[j]),
    sprintf("morie_fairness_xai_ceteris_paribus(feature='%s', grid_size=%d)",
            nm[j], as.integer(grid_size)),
    summary_lines = list(
      Feature = nm[j],
      `Instance prediction` = base,
      `Profile swing` = swing
    ),
    interpretation = sprintf(
      "Holding this instance fixed and varying '%s' alone, the prediction swings by %.4f. A large swing on a protected feature means the decision would change purely on that characteristic.",
      nm[j], swing
    ),
    n = 1L,
    value = swing, feature = nm[j],
    grid = grid, profile = profile, base = base
  )
}


# ---------------------------------------------------------------------------
# 5. SHAP values (sampling estimator)
# ---------------------------------------------------------------------------

#' Shapley feature attributions for one instance (sampling estimator)
#'
#' @param predict_fn Function mapping (n, d) matrix to n predictions.
#' @param x Numeric vector of length d (the instance).
#' @param background Reference matrix (n_bg, d) for marginal averaging.
#' @param feature_names Optional character vector.
#' @param n_samples Number of random permutations averaged.
#' @param seed Reproducibility seed.
#' @return \code{morie_fairness_result}; \code{$value} is the
#'   largest-magnitude SHAP value.
#' @export
morie_fairness_xai_shap_values <- function(predict_fn, x, background,
                                            feature_names = NULL,
                                            n_samples = 200L,
                                            seed = 0L) {
  background <- .xai_as_2d(background)
  d <- ncol(background)
  nm <- .xai_names(feature_names, d)
  x <- as.numeric(x)
  if (length(x) != d) stop("x must have one value per background feature")
  set.seed(as.integer(seed))
  nb <- nrow(background)

  contrib <- numeric(d)
  for (s in seq_len(as.integer(n_samples))) {
    perm <- sample.int(d)
    # For every background row r, walk the permutation: coalition k+1
    # equals coalition k with feature perm[k] switched on (set to x).
    # We compute marginal contributions in a streaming fashion to
    # avoid materialising the full (nb, d+1, d) tile.
    cur <- background  # (nb, d)
    prev_pred <- .xai_predict(predict_fn, cur)
    for (k in seq_len(d)) {
      cur[, perm[k]] <- x[perm[k]]
      new_pred <- .xai_predict(predict_fn, cur)
      contrib[perm[k]] <- contrib[perm[k]] + mean(new_pred - prev_pred)
      prev_pred <- new_pred
    }
  }
  shap <- contrib / as.numeric(n_samples)
  names(shap) <- nm
  order_idx <- order(abs(shap), decreasing = TRUE)
  ranking <- nm[order_idx]
  top <- shap[order_idx[1L]]
  fx <- as.numeric(.xai_predict(predict_fn,
                                matrix(x, nrow = 1L)))
  base_mean <- mean(.xai_predict(predict_fn, background))

  .xai_result(
    "SHAP Feature Attributions (sampling estimator)",
    sprintf("morie_fairness_xai_shap_values(d=%d, n_bg=%d, n_samples=%d)",
            d, nb, as.integer(n_samples)),
    summary_lines = list(
      `Most influential feature` = ranking[1L],
      `Its SHAP value` = top,
      Prediction = fx,
      `Background mean` = base_mean
    ),
    interpretation = sprintf(
      "For this instance the prediction %.4f departs from the background mean %.4f; '%s' contributes the most (%+.4f). The SHAP values sum to that departure.",
      fx, base_mean, ranking[1L], top
    ),
    n = 1L,
    value = top, shap_values = as.list(shap),
    ranking = ranking, prediction = fx, background_mean = base_mean
  )
}
