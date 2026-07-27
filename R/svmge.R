# SPDX-License-Identifier: AGPL-3.0-or-later

#' Support-vector regression for genomic prediction
#'
#' @param x Optional fixed-effect features.
#' @param y Numeric response.
#' @param markers (n x m) genotype matrix.
#' @param C Cost (default 1).
#' @param epsilon SVR tube width (default 0.1).
#' @param gamma RBF kernel scale ("scale" = 1/(m * var(M)) or numeric).
#' @return list(estimate, y_hat, alpha, support_indices, se, n, method).
#' @references Vapnik (1995); Montesinos Lopez Ch 7.
#' @examples
#' morie_svm_genomic(x = rnorm(50), y = rnorm(50), markers = matrix(sample(0:2, 200, TRUE), 50, 4))
#' @export
morie_svm_genomic <- function(x, y, markers, C = 1, epsilon = 0.1,
                        gamma = "scale") {
  y <- as.numeric(y)
  n <- length(y)
  M <- as.matrix(markers)
  feats <- if (is.null(x) || (is.numeric(x) && length(x) == 0)) {
    M
  } else {
    cbind(as.matrix(x), M)
  }
  # A zero-variance predictor carries no signal; drop constant columns.
  if (ncol(feats) > 1L) {
    keep <- apply(feats, 2L, function(col) stats::var(col) > 0)
    if (any(keep) && !all(keep)) feats <- feats[, keep, drop = FALSE]
  }
  g <- if (identical(gamma, "scale")) {
    v <- stats::var(as.numeric(M))
    if (!is.finite(v) || v <= 0) v <- 1
    1 / (ncol(M) * v)
  } else if (identical(gamma, "auto")) {
    1 / ncol(feats)
  } else {
    as.numeric(gamma)
  }
  fit <- morie_svr_train_cpp(feats, y, as.numeric(C), as.numeric(epsilon),
                             2L, g, 0, 3, 1e-3, 1000000L)
  sv_idx <- which(abs(fit$coef) > 1e-8)
  y_hat <- morie_svm_decision_cpp(feats[sv_idx, , drop = FALSE],
                                  fit$coef[sv_idx], fit$rho, feats,
                                  2L, g, 0, 3)
  resid <- y - y_hat
  list(
    estimate = mean(y_hat), y_hat = y_hat, alpha = fit$coef,
    support_indices = sv_idx, intercept = -fit$rho,
    se = sqrt(mean(resid^2)), n = n,
    method = "eps-SVR (native SMO, RBF)"
  )
}

# CANONICAL TEST
# set.seed(12); M <- matrix(rnorm(100), 25, 4); y <- sin(M[,1])+0.2*rnorm(25)
# morie_svm_genomic(rep(0, 25), y, M)
