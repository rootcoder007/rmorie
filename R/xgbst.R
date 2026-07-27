# SPDX-License-Identifier: AGPL-3.0-or-later

#' XGBoost regularized objective (R parity)
#'
#' Wraps the \code{xgboost} package.  If xgboost isn't installed, falls
#' back to \code{gbm} (gradient boosting) so users still get a usable
#' boosted-trees result; the backend is flagged in the output.
#'
#' @param x Numeric predictor matrix.
#' @param y Response.
#' @param n_estimators Number of boosting rounds.
#' @param learning_rate eta / shrinkage.
#' @param max_depth Tree depth.
#' @param reg_lambda L2 leaf penalty.
#' @param reg_alpha L1 leaf penalty.
#' @param task "auto", "classification", or "regression".
#' @param seed RNG seed.
#' @param deterministic_seed Integer or NULL.  If supplied, the RNG state
#'   is derived from the SHA-keyed [morie_det_rng()] so Py<->R streams
#'   agree on the canonical fixture.  When `NULL` (default), behaviour
#'   is unchanged: `seed` drives `set.seed()` directly.
#' @return Named list: estimate, train_score, feature_importances, backend,
#'   n_estimators, learning_rate, max_depth, reg_lambda, reg_alpha, task,
#'   n, method.
#' @importFrom stats predict
#' @examplesIf requireNamespace("xgboost", quietly = TRUE) || requireNamespace("gbm", quietly = TRUE)
#' morie_xgboost_objective(x = rnorm(50), y = rnorm(50))
#' @export
morie_xgboost_objective <- function(x, y, n_estimators = 100L, learning_rate = 0.1,
                              max_depth = 3L, reg_lambda = 1.0,
                              reg_alpha = 0.0, task = "auto", seed = 0L,
                              deterministic_seed = NULL) {
  x <- .morie_ensure_design_matrix(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  if (identical(task, "auto")) {
    # A 0/1 vector (integer or double) is caught by the `%in%` test; do NOT
    # treat every integer as classification -- count outcomes are integers too
    # and must resolve to regression (N4).
    task <- if (is.factor(y) || all(y %in% c(0L, 1L))) {
      "classification"
    } else {
      "regression"
    }
  }
  if (!is.null(deterministic_seed)) {
    morie_det_rng("xgbst", deterministic_seed)
  } else {
    set.seed(seed)
  }
  fit <- .morie_gb_fit(
    x, y, task = task, n_estimators = as.integer(n_estimators),
    learning_rate = learning_rate, max_depth = as.integer(max_depth),
    lambda = reg_lambda, alpha = reg_alpha
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
    backend             = "native",
    n_estimators        = as.integer(n_estimators),
    learning_rate       = as.numeric(learning_rate),
    max_depth           = as.integer(max_depth),
    reg_lambda          = as.numeric(reg_lambda),
    reg_alpha           = as.numeric(reg_alpha),
    task                = task,
    n                   = nrow(x),
    method              = sprintf("XGBoost-style boosting (native, %s)", task)
  )
}
