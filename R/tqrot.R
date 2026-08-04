# SPDX-License-Identifier: AGPL-3.0-or-later
#' Draw a rotation that spreads outlier energy over all coordinates
#'
#' Quantizers do badly when a few coordinates carry most of the norm, and
#' key embeddings are like that. Rotating first makes the coordinates
#' exchangeable. The rotation must be genuinely orthogonal, not merely
#' Gaussian, or it would change the norms it is meant to preserve.
#'
#' Determinism: Gaussian entries come from the shared Lehmer minstd
#' stream and the factorisation is modified Gram-Schmidt, whose R
#' diagonal is non-negative by construction, so both arms return the
#' same Q. A LAPACK-versus-LINPACK QR would not.
#'
#' Formula: \code{A ~ N(0,1)^{d x d}}, \code{Q, R = QR(A)}, return Q.
#'
#' @param d Dimension.
#' @param seed Seed for the shared generator.
#' @return List with \code{Q}, \code{estimate}, \code{d}, \code{orth_err}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   section 4.1 (orthogonalized JL transform).
#' @export
Tqrot <- function(d, seed = 1) {
  d <- as.integer(d)
  g <- .t1_lcg(seed)
  A <- matrix(vapply(seq_len(d * d), function(i) g$norm(), 0), d, d, byrow = TRUE)
  Q <- .s4_qr_mgs(A)$Q
  err <- max(abs(crossprod(Q) - diag(1, d)))
  .t1_result(Q = Q, estimate = Q[1, 1], d = d, orth_err = err,
             method = "Random orthogonal rotation, QR of a Gaussian matrix")
}
