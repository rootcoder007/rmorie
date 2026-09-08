# SPDX-License-Identifier: AGPL-3.0-or-later
#' Penalized log-likelihood of the multinomial logistic model
#'
#' Formula: l_p = l(beta; y) - lambda sum_c beta_c'beta_c (ridge)  or  - lambda sum_c
#' sum_j |beta_cj| (lasso)
#'
#' @param X Design matrix without an intercept column.
#' @param y Observed category index per record, 1-based.
#' @param beta0 Category intercepts.
#' @param beta Category slope coefficients.
#' @param lam Penalty strength lambda.
#' @param penalty 'ridge' or 'lasso'.
#'
#' @return List with ``loglik``, ``penalty``, ``penalized_loglik``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 7, Eq. (7.7) p. 226 for the ridge penalty and
#' Eq. (7.10) p. 227 for the lasso penalty.  The book states on p. 226 that only the
#' slopes are penalized, never the intercepts.  Delegates to the chapter routine in
#' morie.fn._gp_core, which was verified against this book in the earlier tranches of
#' this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are
#' that routine's own, re-read against the chapter PDF here.
#' @export
Mnpenlik <- function(X, y, beta0, beta, lam, penalty = "ridge") {
  out <- morie_penalized_multinomial(X, as.integer(y) - 1L, beta0, beta,
                                     as.numeric(lam), penalty = penalty)
  pl <- if (!is.null(out$penalized_loglik)) out$penalized_loglik else
          out$loglik - out$penalty
  .t1_result(loglik = out$loglik, penalty = out$penalty,
             penalized_loglik = pl, n = nrow(.t1_mat(X)),
             method = "Penalized multinomial log-likelihood, MVSML Eq. (7.7)/(7.10)")
}
