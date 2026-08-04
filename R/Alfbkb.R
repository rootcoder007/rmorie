# SPDX-License-Identifier: AGPL-3.0-or-later

#' Backbone frame update from a predicted quaternion (AlphaFold)
#'
#' Algorithm 23 of the Supplementary Information to Jumper et al. (2021),
#' p. 29.  Six numbers are read off each residue's single representation:
#' three quaternion components and a translation.  Fixing the leading
#' quaternion component to 1 before normalisation guarantees a valid unit
#' quaternion without a constraint and biases the layer towards small
#' rotations, since zero input gives the identity.
#'
#' @param s Single representation, an \code{n x cs} matrix.
#' @param w Projection to (b, c, d, t1, t2, t3), a \code{6 x cs} matrix.
#' @param b Optional bias for that projection.
#' @param frames Optional existing frames to compose with, giving
#'   \code{T_i o BackboneUpdate(s_i)} as in Algorithm 20 line 10.
#' @return A list with \code{frames}, the normalised \code{quat},
#'   \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 23
Alfbkb <- function(s, w, b = NULL, frames = NULL) {
  n <- nrow(s)
  out <- vector("list", n)
  quats <- matrix(0, n, 4)
  for (i in seq_len(n)) {
    p <- alfLin(s[i, ], w, b)
    R <- alfQ2rot(p[1], p[2], p[3])
    tv <- c(p[4], p[5], p[6])
    nq <- sqrt(1 + p[1]^2 + p[2]^2 + p[3]^2)
    quats[i, ] <- c(1, p[1], p[2], p[3]) / nq
    Tf <- list(R = R, t = tv)
    if (!is.null(frames)) Tf <- alfRcomp(frames[[i]], Tf)
    out[[i]] <- Tf
  }
  est <- mean(vapply(out, function(Tf) mean(Tf$t), numeric(1)))
  list(frames = out, quat = quats, estimate = est, n = n,
       method = "AlphaFold backbone update (quaternion to rigid frame)")
}
