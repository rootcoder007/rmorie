# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian kernel BLUP: the closed-form conditional mean of the kernel effects.
#'
#' Formula: Ktilde = (K^-1/sigma2_u + I/sigma2_e)^-1;  utilde = Ktilde (y - 1 mu)/sigma2_e
#'
#' @param y Response vector of length n.
#' @param K Kernel matrix.
#' @param sigma2_u Variance component of the kernel effects.
#' @param sigma2_e Residual variance component.
#' @param mu Intercept; None uses the mean of y.
#'
#' @return List with ``u``, ``K_tilde``, ``mu``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8, Eq. (8.8) p. 281 and its full conditionals on p. 282: y = 1 mu + u + e with u ~ N(0, sigma2_u K), which is kernel ridge regression with lambda = sigma2_e/sigma2_u.  Only the CLOSED-FORM conditional mean is computed here, not the Gibbs sampler: a sampler would make the two language arms depend on matching random number streams, and the conditional mean is the quantity the equation defines.  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
Rkhsbayes <- function(y, K, sigma2_u = 1, sigma2_e = 1, mu = NULL) {
  out <- morie_bayesian_kernel_blup(y, K, as.numeric(sigma2_u),
                                    as.numeric(sigma2_e), mu = mu)
  yv <- .t1_vec(y)
  m <- if (is.null(mu)) mean(yv) else as.numeric(mu)
  .t1_result(u = out$u, K_tilde = out$K_tilde, mu = m, n = length(yv),
             method = "Bayesian kernel BLUP conditional mean, MVSML Eq. (8.8)")
}
