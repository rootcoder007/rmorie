# SPDX-License-Identifier: AGPL-3.0-or-later
#' SNP-BLUP (ridge regression BLUP) marker effects and breeding values
#'
#' Formula: Z = M (scaled markers), Sigma = sigma2_M I in Henderson's equations; GEBV = M uhat
#'
#' @param X Fixed-effect design matrix.
#' @param y Response vector of length n.
#' @param M Scaled marker matrix.
#' @param sigma2_m Marker-effect variance component.
#' @param sigma2_e Residual variance component.
#'
#' @return List with ``beta``, ``marker_effects``, ``gebv``, ``n``, ``m``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 2, Eq. (2.4) p. 53: SNP-BLUP replaces Z by the scaled marker matrix M and Sigma by sigma2_M I in Eq. (2.2); the genomic estimated breeding value is M uhat.  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
#' @examples
#' Snpblup(X = 5L, y = 5L, M = 5L, sigma2_m = c(1, 2, 3, 4, 5, 6, 7, 8))
Snpblup <- function(X, y, M, sigma2_m, sigma2_e = 1) {
  out <- morie_snp_blup(X, y, M, as.numeric(sigma2_m), as.numeric(sigma2_e))
  Mm <- .t1_mat(M)
  fit <- morie_blue_blup_v(X, Mm, y, diag(as.numeric(sigma2_m), ncol(Mm)),
                           diag(as.numeric(sigma2_e), length(as.numeric(y))))
  .t1_result(beta = fit$blue, marker_effects = out$marker_effects,
             gebv = out$gebv, n = nrow(Mm), m = ncol(Mm),
             method = "SNP-BLUP marker effects, MVSML Eq. (2.4)")
}
