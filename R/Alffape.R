# SPDX-License-Identifier: AGPL-3.0-or-later

#' Frame aligned point error (FAPE), the main AlphaFold structural loss
#'
#' Algorithm 28 of the Supplementary Information to Jumper et al. (2021),
#' p. 34.  Every predicted atom is expressed in the local frame of every
#' predicted residue, the same is done for the ground truth, and the two
#' clouds are compared pointwise.  Scoring each atom under every frame
#' makes the loss sensitive to global arrangement as well as local
#' geometry, and needs no superposition; it also distinguishes mirror
#' images (supplement eq. 16-17).
#'
#' @param frames_pred,frames_true Lists of frames, each \code{list(R, t)}.
#' @param x,x_true Matrices of atom positions, \code{natoms x 3}.
#' @param Z Length scale, 10 angstrom in the spec.
#' @param dclamp Distance clamp, 10 angstrom in the spec.
#' @param eps Added under the square root of line 3.  It makes a perfect
#'   prediction score \code{sqrt(eps) / Z} rather than exactly zero; pass
#'   \code{eps = 0} for the unregularised form.
#' @return A list with the loss \code{estimate}, the distance matrix
#'   \code{d}, \code{nframes}, \code{natoms} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 28
Alffape <- function(frames_pred, x, frames_true, x_true, Z = 10, dclamp = 10,
                    eps = 1e-4) {
  nf <- length(frames_pred)
  na <- nrow(x)
  d <- matrix(0, nf, na)
  tot <- 0
  for (i in seq_len(nf)) for (j in seq_len(na)) {
    xi <- alfRinvap(frames_pred[[i]], x[j, ])
    xt <- alfRinvap(frames_true[[i]], x_true[j, ])
    dij <- sqrt(alfVn2(xi - xt) + eps)
    d[i, j] <- dij
    tot <- tot + min(dclamp, dij)
  }
  list(estimate = tot / (nf * na) / Z, d = d, nframes = nf, natoms = na,
       method = "AlphaFold frame aligned point error")
}
