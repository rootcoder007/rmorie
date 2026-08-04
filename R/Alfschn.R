# SPDX-License-Identifier: AGPL-3.0-or-later

#' All-atom coordinates from backbone frames and torsion angles (AlphaFold)
#'
#' Algorithms 24 and 25 of the Supplementary Information to Jumper et al.
#' (2021), pp. 30-31.  Side chain atoms are placed by walking the torsion
#' hierarchy: each torsion frame is its parent frame composed with a
#' literature transform and a rotation about the torsion axis, which the
#' construction places on the x-axis.  Bond lengths and angles come from
#' the caller-supplied literature tables, so only the torsions are free.
#'
#' This implements the algebra, not the amino-acid tables: the idealised
#' geometry is an argument, never baked in.
#'
#' @param frames List of backbone frames, each \code{list(R, t)}.
#' @param angles Torsion angles, an \code{n x nframe x 2} array of
#'   unnormalised (cos, sin) pairs as the network emits them.
#' @param littf List of literature transforms into the parent frame.
#' @param parent Parent frame index per torsion frame; 0 means the frame
#'   hangs directly off the backbone frame.
#' @param litx Matrix of idealised atom positions, \code{natoms x 3}.
#' @param frameof Torsion frame index for each atom.
#' @return A list with atom coordinates \code{x}, the composed torsion
#'   \code{frames}, \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithms 24-25
Alfschn <- function(frames, angles, littf, parent, litx, frameof) {
  n <- length(frames)
  nf <- length(littf)
  na <- nrow(litx)

  # Algorithm 25: rotation about the x-axis from a unit 2-vector.  The
  # network emits an unnormalised (cos, sin) pair; normalising it here is
  # what makes the result a rotation.
  rotx <- function(a) {
    nrm <- sqrt(a[1]^2 + a[2]^2)
    cs <- a[1] / nrm; sn <- a[2] / nrm
    list(R = matrix(c(1, 0, 0, 0, cs, -sn, 0, sn, cs), 3, 3, byrow = TRUE),
         t = c(0, 0, 0))
  }

  allf <- vector("list", n)
  allx <- array(0, c(n, na, 3))
  for (i in seq_len(n)) {
    tf <- vector("list", nf)
    for (f in seq_len(nf)) {
      base <- if (parent[f] < 1) frames[[i]] else tf[[parent[f]]]
      if (is.null(base)) stop("frame ", f, " referenced before its parent")
      tf[[f]] <- alfRcomp(alfRcomp(base, littf[[f]]), rotx(angles[i, f, ]))
    }
    allf[[i]] <- tf
    for (a in seq_len(na)) allx[i, a, ] <- alfRap(tf[[frameof[a]]], litx[a, ])
  }

  list(x = allx, frames = allf, estimate = mean(allx), n = n,
       method = "AlphaFold all-atom coordinates from torsion angles")
}
