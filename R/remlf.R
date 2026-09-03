# SPDX-License-Identifier: AGPL-3.0-or-later
#' Restricted (residual) maximum likelihood log-likelihood of a mixed model
#'
#' Formula: l_R(theta; y) = -0.5 log|X'V^-1 X| - 0.5 log|V| - 0.5 (y - X betatilde)' V^-1
#' (y - X betatilde)
#'
#' @param X Fixed-effect design matrix.
#' @param Z Random-effect design matrix.
#' @param y Response vector of length n.
#' @param D Variance-covariance matrix of the random effects.
#' @param R Residual variance-covariance matrix; None uses the identity.
#'
#' @return List with ``loglik``, ``beta``, ``n``, ``p``, ``q``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 5, Sect. 5.2.1.2 p. 146.  REML differs from
#' the ML log-likelihood of Eq. (5.2) by the -0.5 log|X'V^-1 X| term, which is what
#' removes the downward bias of the ML variance estimate; betatilde is the generalized
#' least squares estimator.  Delegates to the chapter routine in morie.fn._gp_core, which
#' was verified against this book in the earlier tranches of this shelf recorded in
#' ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own,
#' re-read against the chapter PDF here.
#' @export
Remlik <- function(X, Z, y, D, R = NULL) {
  out <- morie_reml_loglik(X, Z, y, D, R = R)
  Xm <- .t1_mat(X)
  Zm <- .t1_mat(Z)
  .t1_result(loglik = as.numeric(out$loglik), beta = as.numeric(out$beta),
             n = nrow(Xm), p = ncol(Xm), q = ncol(Zm),
             method = "REML log-likelihood, MVSML Sect. 5.2.1.2")
}
