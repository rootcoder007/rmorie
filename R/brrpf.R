# SPDX-License-Identifier: AGPL-3.0-or-later
#' Default hyperparameters of the Bayesian ridge regression prior.
#'
#' Formula: S = Var(Y)(1 - R2)(nu + 2);  S_beta = Var(Y) R2 (nu_beta + 2)
#'
#' @param y Response vector of length n.
#' @param R2 Prior proportion of the phenotypic variance explained by the markers.
#' @param nu Degrees of freedom of the scaled inverse chi-square prior on the residual variance.
#' @param nu_beta Degrees of freedom of the prior on the marker-effect variance.
#'
#' @return List with ``S``, ``S_beta``, ``nu``, ``nu_beta``, ``var_y``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 6, the BGLR default rules quoted on pp. 175 and 184: the scale of the residual prior is Var(Y)(1 - R2)(nu + 2) and, for the BRR, the scale of the marker-effect prior is Var(Y) R2 (nu_beta + 2).  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
Brrhyper <- function(y, R2 = 0.5, nu = 5, nu_beta = 5) {
  out <- morie_brr_hyper(y, R2 = as.numeric(R2), nu = as.numeric(nu),
                         nu_beta = as.numeric(nu_beta))
  yv <- .t1_vec(y)
  .t1_result(S = out$S, S_beta = out$S_beta, nu = out$nu,
             nu_beta = out$nu_beta, var_y = stats::var(yv), n = length(yv),
             method = "BRR prior hyperparameters, MVSML Chap. 6 pp. 175, 184")
}
