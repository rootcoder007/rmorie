# SPDX-License-Identifier: AGPL-3.0-or-later

#' Random-forest genomic predictor
#'
#' Native random forest (ESL Algorithm 15.1) over combined covariate
#' and marker
#' fallback (regression CART approximation).
#'
#' @param x Optional fixed features.
#' @param y Numeric response.
#' @param markers Genotype matrix (n x m).
#' @param n_trees Number of trees.
#' @param max_depth Max depth (fallback only).
#' @param min_samples Min samples per node.
#' @param mtry Features sampled per split (default sqrt(p)).
#' @param seed Seed.
#' @return list(estimate, y_hat, oob_score, feature_importance, se, n, method).
#' @references Breiman (2001); Montesinos Lopez Ch 8.
#' @examples
#' morie_random_forest_genomic(
#'   x = rnorm(50), y = rnorm(50),
#'   markers = matrix(sample(0:2, 200, TRUE), 50, 4)
#' )
#' @export
morie_random_forest_genomic <- function(x, y, markers, n_trees = 100,
                                  max_depth = 10, min_samples = 2,
                                  mtry = NULL, seed = 0) {
  set.seed(seed)
  y <- as.numeric(y)
  n <- length(y)
  M <- as.matrix(markers)
  feats <- if (is.null(x) || (is.numeric(x) && length(x) == 0)) {
    M
  } else {
    cbind(as.matrix(x), M)
  }
  p <- ncol(feats)
  if (is.null(mtry)) mtry <- max(floor(sqrt(p)), 1L)
  fit <- .morie_rf_fit(
    feats, y, task = "regression", n_estimators = as.integer(n_trees),
    mtry = mtry, max_depth = as.integer(max_depth),
    min_node = as.integer(min_samples)
  )
  y_hat <- fit$fitted
  oob <- as.numeric(1 - sum((y - fit$oob)^2) / max(sum((y - mean(y))^2), 1e-12))
  resid <- y - y_hat
  list(
    estimate = mean(y_hat), y_hat = y_hat, oob_score = oob,
    feature_importance = as.numeric(fit$importance),
    se = sqrt(mean(resid^2)),
    n = n, method = "Random Forest (native, regression)"
  )
}

# CANONICAL TEST
# set.seed(13); M <- matrix(rnorm(200), 40, 5)
# y <- M[,1] + 0.5*M[,2]^2 + 0.2*rnorm(40)
# morie_random_forest_genomic(rep(0,40), y, M, n_trees=20, seed=13)
