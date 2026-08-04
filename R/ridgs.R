# SPDX-License-Identifier: AGPL-3.0-or-later
#' Closed-form ridge regression estimator.
#'
#' Formula: beta(lambda) = (X'X + lambda D)^-1 X'y,  D = diag(0, 1, ..., 1)
#'
#' @param X Design matrix, one record per row.
#' @param y Response vector of length n.
#' @param lam Regularization parameter lambda; must be non-negative.
#' @param add_intercept Prepend a column of ones to X and leave its coefficient unpenalized.
#'
#' @return List with ``beta``, ``fitted``, ``resid``, ``rss``, ``penalty``, ``prss``, ``lambda``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 3, Sect. 3.6.1 p. 82: solving grad PRSS_lambda(beta) = 0 gives beta-hat(lambda) = (X'X + lambda D)^-1 X'y with D the identity carrying a zero in its first entry.  Delegates to the chapter-3 ridge routine already verified against the book for this shelf.  Read from the chapter PDF, not recalled.
#' @export
Ridgesol <- function(X, y, lam, add_intercept = TRUE) {
  if (as.numeric(lam) < 0) stop("lambda must be non-negative")
  out <- morie_ridge(X, y, as.numeric(lam), add_intercept = isTRUE(add_intercept))
  Xm <- if (isTRUE(add_intercept)) .t1_cbind1(.t1_mat(X)) else .t1_mat(X)
  .t1_result(beta = out$beta, fitted = out$fitted,
             resid = .t1_vec(y) - out$fitted,
             rss = out$rss, penalty = out$penalty, prss = out$prss,
             lambda = as.numeric(lam), n = nrow(Xm), p = ncol(Xm),
             method = "Ridge closed-form solution, MVSML Sect. 3.6.1")
}
