# SPDX-License-Identifier: AGPL-3.0-or-later

#' Random Forest ensemble (R parity)
#'
#' Native random forest (ESL Algorithm 15.1).  Auto-detects task from y
#' (factor / integer-like -> classification, otherwise regression).
#'
#' @param x Numeric predictor matrix.
#' @param y Response.
#' @param n_estimators Number of trees.
#' @param max_depth Max tree depth (NULL -> unrestricted).
#' @param task "auto", "classification", or "regression".
#' @param seed RNG seed.
#' @param deterministic_seed Integer or NULL.  If supplied, the RNG state
#'   is derived from the SHA-keyed [morie_det_rng()] so Py<->R streams
#'   agree on the canonical fixture.  When `NULL` (default), behaviour
#'   is unchanged: `seed` drives `set.seed()` directly.
#' @return Named list: estimate, train_score, oob_score, feature_importances,
#'   n_estimators, task, n, method.
#' @importFrom stats predict
#' @examples
#' morie_random_forest_ensemble(x = rnorm(50), y = rnorm(50))
#' @export
morie_random_forest_ensemble <- function(x, y, n_estimators = 100L,
                                   max_depth = NULL, task = "auto",
                                   seed = 0L,
                                   deterministic_seed = NULL) {
  x <- .morie_ensure_design_matrix(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  if (identical(task, "auto")) {
    task <- if (is.factor(y) || all(y %in% c(0L, 1L)) || is.integer(y)) {
      "classification"
    } else {
      "regression"
    }
  }
  if (!is.null(deterministic_seed)) {
    morie_det_rng("rfens", deterministic_seed)
  } else {
    set.seed(seed)
  }
  fit <- .morie_rf_fit(
    x, y, task = task, n_estimators = as.integer(n_estimators),
    max_depth = if (is.null(max_depth)) 30L else as.integer(max_depth)
  )
  if (task == "classification") {
    yf <- as.factor(y)
    train_score <- mean(as.character(fit$fitted) == as.character(yf))
    oob <- mean(as.character(fit$oob) == as.character(yf))
  } else {
    yv <- as.numeric(y)
    denom <- sum((yv - mean(yv))^2)
    train_score <- 1 - sum((fit$fitted - yv)^2) / max(denom, 1e-12)
    oob <- 1 - sum((fit$oob - yv)^2) / max(denom, 1e-12)
  }
  list(
    estimate            = as.numeric(train_score),
    train_score         = as.numeric(train_score),
    oob_score           = as.numeric(oob),
    feature_importances = as.numeric(fit$importance),
    n_estimators        = as.integer(n_estimators),
    task                = task,
    n                   = nrow(x),
    method              = sprintf("Random Forest (%s)", task)
  )
}
