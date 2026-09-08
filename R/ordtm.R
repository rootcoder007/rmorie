# SPDX-License-Identifier: AGPL-3.0-or-later
#' Category probabilities of the ordinal threshold model
#'
#' Formula: p_ic = F(gamma_c + eta_i) - F(gamma_\{c-1\} + eta_i),  gamma_0 = -inf, gamma_C = +inf
#'
#' @param eta Linear predictor x_i'beta for each record.
#' @param thresholds The C - 1 finite thresholds gamma_1 < ... < gamma_\{C-1\}.
#' @param link 'probit' for the standard normal CDF or 'logistic' for the standard logistic CDF.
#'
#' @return List with ``probabilities``, ``n``, ``C``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 7, Eq. (7.1) p. 210, which gives both the
#' ordinal probit and the ordinal logistic link on the same page.  Delegates to the
#' chapter routine in morie.fn._gp_core, which was verified against this book in the
#' earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and
#' equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ordprobs(V, V)
Ordprobs <- function(eta, thresholds, link = "probit") {
  P <- morie_ordinal_probs(eta, thresholds, link = link)
  .t1_result(probabilities = P, n = nrow(P), C = ncol(P),
             method = "Ordinal threshold model probabilities, MVSML Eq. (7.1)")
}
