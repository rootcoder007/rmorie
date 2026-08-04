# SPDX-License-Identifier: AGPL-3.0-or-later

#' Gradient boosting ensemble (R parity)
#'
#' Native gradient tree boosting (ESL Algorithm 10.3 with shrinkage);
#' no boosted-trees package is loaded or called.
#'
#' @param x Numeric predictor matrix.
#' @param y Response.
#' @param n_estimators Number of boosting iterations.
#' @param learning_rate Shrinkage nu.
#' @param max_depth Depth of each tree.
#' @param task "auto", "classification", or "regression".
#' @param seed RNG seed.
#' @param deterministic_seed Integer or NULL.  If supplied, the RNG state
#'   is derived from the SHA-keyed [morie_det_rng()] so Py<->R streams
#'   agree on the canonical fixture.  When `NULL` (default), behaviour
#'   is unchanged: `seed` drives `set.seed()` directly.
#' @return Named list: estimate, train_score, feature_importances,
#'   n_estimators, learning_rate, max_depth, task, n, method.
#' @importFrom stats predict
#' @examples
#' morie_gradient_boosting_ensemble(x = rnorm(50), y = rnorm(50))
#' @export
morie_gradient_boosting_ensemble <- function(x, y, n_estimators = 100L,
                                             learning_rate = 0.1,
                                             max_depth = 3L,
                                             task = "auto", seed = 0L,
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
  n <- nrow(x)
  if (!is.null(deterministic_seed)) {
    morie_det_rng("gbens", deterministic_seed)
  } else {
    set.seed(seed)
  }
  fit <- .morie_gb_fit(
    x, y,
    task = task, n_estimators = as.integer(n_estimators),
    learning_rate = learning_rate, max_depth = as.integer(max_depth)
  )
  if (task == "classification") {
    yv <- as.numeric(as.factor(y)) - 1
    train_score <- mean(as.integer(fit$fitted > 0.5) == yv)
  } else {
    yv <- as.numeric(y)
    train_score <- 1 - sum((fit$fitted - yv)^2) /
      max(sum((yv - mean(yv))^2), 1e-12)
  }
  list(
    estimate            = as.numeric(train_score),
    train_score         = as.numeric(train_score),
    feature_importances = as.numeric(fit$importance),
    n_estimators        = as.integer(n_estimators),
    learning_rate       = as.numeric(learning_rate),
    max_depth           = as.integer(max_depth),
    task                = task,
    backend             = "native",
    n                   = n,
    method              = sprintf("Gradient Boosting (%s, native)", task)
  )
}
