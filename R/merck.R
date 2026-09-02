# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mercer check: is a Gram matrix a valid kernel matrix?
#'
#' Formula: K is a kernel iff the Gram matrix is symmetric positive semi-definite: min eigenvalue(K) >= 0
#'
#' @param K Candidate Gram matrix.
#' @param tol Negative eigenvalues larger in size than tol fail the check.
#'
#' @return List with ``is_kernel``, ``min_eigenvalue``, ``eigenvalues``, ``symmetry_gap``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8, Sect. 8.2.1 p. 255, property 2 and property 3: the Gram matrix K with entries K(x_i, x_j) must be positive semi-definite for every choice of x_1, ..., x_n, and the book states that Mercer's theorem -- the integral condition on square-integrable g -- is an equivalent formulation of that finitely positive semi-definite property.  The finite check is therefore the one implemented.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' Mercerchk(K = 5L)
Mercerchk <- function(K, tol = 1e-9) {
  Km <- as.matrix(K); n <- nrow(Km)
  if (n == 0L || ncol(Km) != n) stop("K must be a non-empty square matrix")
  gap <- max(abs(Km - t(Km)))
  out <- morie_is_psd(Km, tol = as.numeric(tol))
  .t1_result(is_kernel = out$psd, min_eigenvalue = min(out$eigenvalues),
             eigenvalues = out$eigenvalues, symmetry_gap = gap, n = n,
             method = "Mercer / positive semi-definiteness check, MVSML Sect. 8.2.1")
}
