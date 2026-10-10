# SPDX-License-Identifier: AGPL-3.0-or-later

#' Random search hyperparameter optimisation (R parity)
#'
#' Native k-fold cross-validated random search: \code{n_iter} candidate
#' hyperparameter settings are drawn at random (caret's random-search
#' distributions: \code{alpha ~ U(0, 1)}, \code{lambda = 2^U(-10, 3)} for
#' \code{"glmnet"}; \code{lambda = 10^U(-5, 1)} for \code{"ridge"}), each is
#' scored by cross-validation with rmorie's own learners, and the best is
#' returned. Folds, metrics, tie-breaking and the supported learners
#' (\code{"lm"}, \code{"glm"}, \code{"glmnet"}, \code{"ridge"}) are those of
#' [morie_grid_search_cv()], which documents them; the RNG protocol is
#' caret's \code{train(search = "random")}: one draw seeds the
#' (isolated) fold assignment, then the candidates are drawn from the
#' main stream, so a given seed reproduces caret's candidates and folds.
#'
#' @param x Numeric predictor matrix.
#' @param y Response.
#' @param method Learner id, one of \code{"lm"}, \code{"glm"},
#'   \code{"glmnet"}, \code{"ridge"} (default \code{"glmnet"} for
#'   classification, \code{"ridge"} for regression).
#' @param n_iter Number of random draws.
#' @param cv CV folds.
#' @param task "auto" / "classification" / "regression".
#' @param seed RNG seed.
#' @param deterministic_seed Integer or NULL.  If supplied, the RNG state
#'   is derived from the SHA-keyed [morie_det_rng()] so Py<->R streams
#'   agree on the canonical fixture.  When `NULL` (default), behaviour
#'   is unchanged: `seed` drives `set.seed()` directly.
#' @return Named list: estimate (CV score of the best draw), best_params,
#'   best_score (same as estimate), sampled_params (the drawn parameters,
#'   sorted, with the fold standard deviations of the metrics),
#'   sampled_scores (mean CV RMSE for regression, accuracy for
#'   classification, aligned with sampled_params), n_iter, task, n, method.
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(120), 40, 3)
#' y <- drop(x %*% c(1, -0.5, 0)) + rnorm(40)
#' morie_random_search_cv(x, y, n_iter = 4L, cv = 3L, task = "regression")
#' @export
morie_random_search_cv <- function(x, y, method = NULL, n_iter = 20L, cv = 5L,
                             task = "auto", seed = 0L,
                             deterministic_seed = NULL) {
  x <- .morie_ensure_design_matrix(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  colnames(x) <- colnames(x) %||% paste0("x", seq_len(ncol(x)) - 1L)
  task <- .gs_resolve_task(task, y)
  if (!is.null(deterministic_seed)) {
    .rmorie_local_det_rng("rndsr", deterministic_seed)
  } else {
    .rmorie_local_seed(seed)
  }
  if (is.null(method)) {
    method <- if (task == "classification") "glmnet" else "ridge"
  }
  res <- .gs_cv_search(x, y, method = method, task = task,
                       tune_grid = NULL, tune_length = n_iter,
                       search = "random", cv = cv)
  results <- res$results
  list(
    estimate        = res$best_score,
    best_params     = as.list(res$best),
    best_score      = res$best_score,
    sampled_params  = results[, setdiff(colnames(results), c("Accuracy", "Kappa", "RMSE", "Rsquared", "MAE")), drop = FALSE],
    sampled_scores  = res$scores,
    n_iter          = as.integer(n_iter),
    task            = task,
    n               = nrow(x),
    method          = sprintf("Random search CV (%s)", method)
  )
}
