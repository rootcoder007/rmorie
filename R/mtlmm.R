# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-trait linear mixed model.
#'
#' Formula: Y = (1 (x) I_T) mu + X beta + Z b + eps,  b ~ N(0, G (x) Sigma_T),  eps ~ N(0, I_J (x) R_T)
#'
#' @param Y Lines by traits.
#' @param Z Design matrix of lines.
#' @param G Genomic relationship matrix.
#' @param Sigma_T Genetic covariance between traits.
#' @param R_T Residual covariance between traits.
#' @param X Extra fixed-effect columns; None uses only the trait intercepts.
#'
#' @return List with ``mu``, ``beta``, ``b``, ``J``, ``T``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 5, Eq. (5.5)/(5.5a) p. 153: the traits of each line are stacked, giving a mixed model with Kronecker-structured covariances.  The book notes on p. 153 that when Sigma_T and R_T are diagonal this is equivalent to fitting each trait separately.  The solution is in the stacked ordering (line 1 traits, line 2 traits, ...).  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
Mtlmmfit <- function(Y, Z, G, Sigma_T, R_T, X = NULL) {
  out <- morie_multitrait(Y, Z, G, Sigma_T, R_T, X = X)
  Ym <- .t1_mat(Y)
  .t1_result(mu = out$mu, beta = out$beta, b = out$b,
             J = nrow(Ym), T = ncol(Ym),
             method = "Multi-trait linear mixed model, MVSML Eq. (5.5)")
}
