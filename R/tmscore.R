# SPDX-License-Identifier: AGPL-3.0-or-later
#' Structure similarity whose scale does not drift with chain length
#'
#' RMSD has no fixed meaning across lengths: 3 angstrom is excellent for a
#' 300-residue protein and poor for a 40-residue one. The d0
#' normalisation fixes that, so a TM-score above 0.5 means the same fold
#' at any size. The distance weighting also caps the damage a few badly
#' placed loops can do.
#'
#' Formula: \code{TM = (1/L_ref) sum_i 1/(1 + (d_i/d_0)^2)},
#' \code{d_0 = 1.24 (L_ref - 15)^(1/3) - 1.8}.
#'
#' @param coords1,coords2 Aligned, already superposed residue coordinates.
#' @param l_ref Reference length; number of aligned residues by default.
#' @return List with \code{estimate}, \code{d0}, \code{rmsd}, \code{L}, \code{L_ref}.
#' @references Zhang, Y. & Skolnick, J. (2004). Proteins 57:702-710,
#'   equations (2) and (3).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Tmscore(V, V)
Tmscore <- function(coords1, coords2, l_ref = NULL) {
  A <- as.matrix(coords1); B <- as.matrix(coords2); L <- nrow(A)
  Lr <- if (is.null(l_ref)) L else as.numeric(l_ref)
  d0 <- if (Lr > 15) 1.24 * (Lr - 15)^(1 / 3) - 1.8 else 0.5
  d2 <- rowSums((A - B)^2)
  .t1_result(estimate = sum(1 / (1 + d2 / (d0 * d0))) / Lr, d0 = d0,
             rmsd = sqrt(sum(d2) / L), L = L, L_ref = Lr,
             method = "TM-score structural similarity")
}
