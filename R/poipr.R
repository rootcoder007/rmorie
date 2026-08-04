# SPDX-License-Identifier: AGPL-3.0-or-later
#' Penalized Poisson log-linear regression.
#'
#' Formula: l_p = sum_i y_i eta_i - sum_i exp(eta_i) - sum_i log(y_i!) - (lambda/2) sum_j beta_j^2,  eta_i = beta_0 + x_i'beta
#'
#' @param X Design matrix without an intercept column.
#' @param y Observed counts.
#' @param lam Penalty strength lambda.
#' @param penalty 'ridge' or 'lasso'.
#' @param n_iter Fixed number of iteratively reweighted least squares iterations.
#' @param add_intercept Prepend a column of ones and leave its coefficient unpenalized.
#'
#' @return List with ``beta``, ``fitted``, ``loglik``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 7, Sect. 7.5 p. 232.  The book fits this by the second-order approximation of the log-likelihood solved as a weighted least squares problem; the intercept is unpenalized.  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.  A FIXED iteration count is used rather than a tolerance stop, so both language arms perform identically many updates.
#' @export
Poispen <- function(X, y, lam = 1, penalty = "ridge", n_iter = 100L, add_intercept = TRUE) {
  out <- morie_penalized_poisson(X, as.numeric(y), lambda = as.numeric(lam),
                                 penalty = penalty, n_iter = as.integer(n_iter),
                                 tol = 0, add_intercept = isTRUE(add_intercept))
  Xm <- .t1_mat(X)
  .t1_result(beta = out$beta, fitted = out$fitted, loglik = out$loglik,
             n = nrow(Xm), p = ncol(Xm),
             method = "Penalized Poisson regression, MVSML Sect. 7.5")
}
