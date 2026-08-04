# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparse GP with M inducing points, FITC and DTC side by side
#'
#' Snelson and Ghahramani (2006), NIPS 18, 1257-1264 (FITC), and
#' Quinonero-Candela and Rasmussen (2005), A unifying view of sparse
#' approximate Gaussian process regression, JMLR 6, 1939-1959, whose
#' section 6 puts DTC and FITC in one family: both replace K_nn by the
#' Nystrom term Q_nn, differing only in the diagonal -- DTC uses Q_nn,
#' FITC uses Q_nn + diag(K_nn - Q_nn), so FITC alone reproduces the exact
#' marginal variances.  Neither was retrievable here as a full text; both
#' covariances are quoted in their standard published form.  Inducing
#' points default to an even subsample taken BY INDEX, not by a draw.
#'
#' @param X,y training data.
#' @param M number of inducing points.
#' @param X_test test inputs.
#' @param gamma RBF width.
#' @param sigma2 noise variance.
#' @return list: estimate, pred_fitc, pred_dtc, var_fitc, var_dtc,
#'   inducing, method.
#' @keywords internal
#' @examples
#' Sparsegp(matrix(c(0, 1, 2, 3), 4, 1), c(0, 1, 0, 1), 2)$pred_fitc
#' @export
Sparsegp <- function(X, y, M = 3, X_test = NULL, gamma = 1, sigma2 = 1e-2) {
  Xm <- .s03mat(X); n <- nrow(Xm); m <- as.integer(M)
  if (m > n) m <- n
  idx <- integer(m)
  for (t in seq_len(m)) {
    idx[t] <- if (m > 1L) as.integer(round((t - 1) * (n - 1) / (m - 1))) else 0L
  }
  Z <- Xm[idx + 1L, , drop = FALSE]
  f <- Fitcgp(X, y, X_test, Z, gamma, sigma2, 1e-8, "fitc")
  d <- Fitcgp(X, y, X_test, Z, gamma, sigma2, 1e-8, "dtc")
  list(estimate = if (length(f$pred)) f$pred[1] else NaN,
       pred_fitc = f$pred, pred_dtc = d$pred, var_fitc = f$var,
       var_dtc = d$var, inducing = idx,
       method = "FITC and DTC sparse GP (Snelson and Ghahramani 2006; Quinonero-Candela and Rasmussen 2005)")
}
