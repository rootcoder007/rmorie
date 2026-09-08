# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ridge regression carried out on inner products instead of features
#'
#' The dual form is what makes a kernel method possible: the fit depends
#' on the data only through the Gram matrix, so the implied feature space
#' never has to be built. The penalty earns its place twice over -- it
#' controls smoothness, and it is what keeps the near-singular Gram matrix
#' of any smooth kernel invertible.
#'
#' An alias. The solver is \code{\link{Krreg}};
#' \code{ledger/wave2/DUPMAP.tsv} records \code{krrFDA} as a duplicate of
#' \code{krreg} and it is the same dual solve, so only the argument order
#' and the penalty's name differ here.
#'
#' Formula: \code{alpha = (K + lambda I)^{-1} y}.
#'
#' @param X Predictor of length n.
#' @param y Response of length n.
#' @param kernel One of \code{"gaussian"}, \code{"epanechnikov"},
#'   \code{"uniform"}.
#' @param lam Ridge penalty, strictly positive.
#' @param x_eval Evaluation points; defaults to \code{X}.
#' @param bandwidth Kernel bandwidth; Silverman's rule when NULL.
#' @return Whatever \code{\link{Krreg}} returns, unchanged.
#' @references Saunders, C., Gammerman, A. and Vovk, V. (1998).
#'   Proceedings of the 15th International Conference on Machine
#'   Learning, 515-521.
#' @seealso \code{\link{Krreg}}
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' KrrFDA(V, V)
KrrFDA <- function(X, y, kernel = "gaussian", lam = 1, x_eval = NULL,
                   bandwidth = NULL) {
  Krreg(X, y, x_eval, bandwidth = bandwidth, penalty = lam, kernel = kernel)
}
