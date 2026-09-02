# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kabsch optimal superposition
#'
#' Both sets are centred, the cross-covariance is formed and the
#' rotation read off its singular value decomposition.  The sign
#' correction on the last singular direction is what stops the routine
#' returning a reflection, which fits the points equally well but is not
#' a rotation.  The SVD comes from the Jacobi eigendecomposition of
#' H'H, so no external linear algebra is used.
#'
#' Formula: H = P'Q, H = U S V', R = V diag(1, ..., d) U' with
#'   d = sign(det(V U')).
#'
#' @param coords1 Matrix of points, one per row.
#' @param coords2 Matrix of the same shape.
#' @return List with \code{estimate} (RMSD), \code{rmsd},
#'   \code{rotation}, \code{det_rotation}, \code{singular_values},
#'   \code{n}, \code{method}.
#' @references Kabsch (1976), Acta Crystallographica A32(5):922-923,
#'   \doi{10.1107/S0567739476001873}; Kabsch (1978), Acta
#'   Crystallographica A34(5):827-828. \doi{10.1107/S0567739478001680}
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Kabsch(V, V)
Kabsch <- function(coords1, coords2) {
  P <- .s03mat(coords1)
  Q <- .s03mat(coords2)
  if (nrow(P) == 0L || nrow(Q) == 0L) stop("kabsch_superpose: coordinate set is empty")
  if (nrow(P) != nrow(Q)) stop("kabsch_superpose: coordinate sets have different point counts")
  d <- ncol(P)
  if (ncol(Q) != d) stop("kabsch_superpose: coordinate sets have different dimensions")
  n <- nrow(P)
  A <- sweep(P, 2, colMeans(P))
  B <- sweep(Q, 2, colMeans(Q))
  H <- t(A) %*% B
  ei <- .s03jacobi(t(H) %*% H)
  idx <- rev(seq_len(d))
  V <- ei$vectors[, idx, drop = FALSE]
  sing <- sqrt(pmax(ei$values[idx], 0))
  if (sing[d] <= 1e-12 * max(sing[1], 1e-300))
    stop("kabsch_superpose: degenerate configuration, the cross-covariance is rank deficient")
  U <- matrix(0, d, d)
  for (j in seq_len(d)) U[, j] <- as.numeric(H %*% V[, j]) / sing[j]
  dd <- if (det(V %*% t(U)) >= 0) 1 else -1
  scal <- rep(1, d)
  scal[d] <- dd
  R <- V %*% diag(scal, d) %*% t(U)
  ss <- sum((A %*% t(R) - B)^2)
  rmsd <- sqrt(ss / n)
  .t1_result(estimate = rmsd, rmsd = rmsd, rotation = R,
             det_rotation = det(R), singular_values = sing, n = n,
             method = "R = V diag(1,...,d) U' from H = P'Q, Kabsch (1976, 1978)")
}
