# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kernel covariance of a replicated-line predictor.
#'
#' Formula: K_* = Var(Z u) = sigma2_u * Z K Z'
#'
#' @param Z Incidence matrix mapping records to lines.
#' @param K Kernel (relationship) matrix between lines.
#' @param sigma2_u Variance component of the line effects.
#'
#' @return List with ``K_star``, ``n``, ``J``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8, Eq. (8.9) p. 282: with replicated individuals the model is Y = 1 mu + Z u + e; BGLR cannot take that predictor directly, so the covariance of the predictor, Z K Z', is precomputed and used as the kernel.  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
Kernblup <- function(Z, K, sigma2_u = 1) {
  Ks <- morie_kernel_blup_replicated(Z, K, as.numeric(sigma2_u))
  Zm <- .t1_mat(Z)
  .t1_result(K_star = Ks, n = nrow(Zm), J = ncol(Zm),
             method = "Replicated-line kernel covariance, MVSML Eq. (8.9)")
}
