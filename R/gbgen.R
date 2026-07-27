# SPDX-License-Identifier: AGPL-3.0-or-later

#' Gradient-boosting genomic predictor (Friedman 2001)
#'
#' Uses gbm if available; otherwise base-R boosted stumps.
#'
#' @param x Optional fixed features.
#' @param y Numeric response.
#' @param markers (n x m) genotype matrix.
#' @param n_estimators Boosting rounds.
#' @param learning_rate Shrinkage.
#' @param max_depth Tree depth (gbm only).
#' @param seed Seed.
#' @return list(estimate, y_hat, train_loss, se, n, method).
#' @references Friedman (2001); Montesinos Lopez Ch 9.
#' @examples
#' morie_gradient_boosting_genomic(
#'   x = rnorm(50), y = rnorm(50),
#'   markers = matrix(sample(0:2, 200, TRUE), 50, 4)
#' )
#' @export
morie_gradient_boosting_genomic <- function(x, y, markers, n_estimators = 100,
                                      learning_rate = 0.1, max_depth = 3,
                                      seed = 0) {
  set.seed(seed)
  y <- as.numeric(y)
  n <- length(y)
  M <- as.matrix(markers)
  feats <- if (is.null(x) || (is.numeric(x) && length(x) == 0)) {
    M
  } else {
    cbind(as.matrix(x), M)
  }
  # A zero-variance predictor carries no signal and cannot produce a split;
  # drop any constant columns first.
  if (ncol(feats) > 1L) {
    keep <- apply(feats, 2L, function(col) stats::var(col) > 0)
    if (any(keep) && !all(keep)) feats <- feats[, keep, drop = FALSE]
  }
  fit <- .morie_gb_fit(
    feats, y, task = "regression", n_estimators = as.integer(n_estimators),
    learning_rate = learning_rate, max_depth = as.integer(max_depth),
    min_node = 2L
  )
  y_hat <- fit$fitted
  # Training MSE after each boosting round, so callers can see the path.
  train_loss <- .morie_gb_loss_path(fit, feats, y)
  resid <- y - y_hat
  list(
    estimate = mean(y_hat), y_hat = y_hat, train_loss = train_loss,
    se = sqrt(mean(resid^2)), n = n,
    method = "Gradient Boosting (native, regression)"
  )
}

# CANONICAL TEST
# set.seed(14); M <- matrix(rnorm(160), 40, 4); y <- sign(M[,1])+0.3*rnorm(40)
# morie_gradient_boosting_genomic(rep(0,40), y, M, n_estimators=20, seed=14)
