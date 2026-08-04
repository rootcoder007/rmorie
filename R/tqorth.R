# SPDX-License-Identifier: AGPL-3.0-or-later
#' Replace a Gaussian sketch by one with orthonormal rows
#'
#' Independent Gaussian rows partly repeat one another, so some of the
#' sketch width buys nothing. Orthogonalising the rows removes that
#' redundancy and, in the paper measurements, almost always improves the
#' quantizer.
#'
#' Formula: \code{Q, R = QR(S^T)}; return \code{S_orth = Q^T}.
#'
#' @param S_mat Gaussian sketch matrix, m by d with m <= d.
#' @return List with \code{S_orth}, \code{estimate}, \code{m}, \code{d}, \code{orth_err}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   section 4.1 (orthogonalized JL transform).
#' @export
Tqorth <- function(S_mat) {
  Sm <- as.matrix(S_mat); m <- nrow(Sm); d <- ncol(Sm)
  Q <- .s4_qr_mgs(t(Sm))$Q
  So <- t(Q)
  err <- max(abs(tcrossprod(So) - diag(1, m)))
  .t1_result(S_orth = So, estimate = So[1, 1], m = m, d = d, orth_err = err,
             method = "Orthogonalized JL sketch matrix")
}
