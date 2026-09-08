# SPDX-License-Identifier: AGPL-3.0-or-later

#' Recycling embedder of AlphaFold
#'
#' Algorithms 30 and 32 of the Supplementary Information to Jumper et al.
#' (2021), pp. 42-43.  Recycling feeds the previous iteration's outputs
#' back into the inputs: pairwise distances between predicted beta-carbon
#' positions are discretised, projected and added to the layer-normalised
#' pair representation, and the first MSA row is layer-normalised.  This is
#' the only channel through which a previous prediction reaches the
#' network; everything else is recomputed each cycle.
#'
#' The iteration count is fixed and there is no tolerance-based early exit.
#'
#' The supplement describes the bins twice and the two descriptions
#' disagree: "15 bins of equal width 1.25 A" implies an upper bin at
#' 20.875, while "precise bin values range from 3 3/8 A to 21 3/8 A"
#' implies a width of 18/14 = 1.2857.  The default follows the stated
#' endpoints; pass \code{bins} explicitly for the other reading.
#'
#' @param m1 First row of the MSA representation, an \code{n x cm} matrix.
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param x Predicted beta-carbon positions, an \code{n x 3} matrix.
#' @param wd Projection of the one-hot distance encoding.
#' @param bins Distance bins; see above for the default.
#' @param ncycle Number of recycling iterations.
#' @return A list with \code{z}, \code{m1}, the distance matrix \code{d},
#'   \code{estimate}, \code{n}, \code{ncycle} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithms 30, 32
Alfrecyc <- function(m1, z, x, wd, bins = NULL, ncycle = 1) {
  if (is.null(bins)) bins <- 3.375 + (21.375 - 3.375) / 14 * (seq_len(15) - 1)
  n <- dim(z)[1]
  cz <- dim(z)[3]
  zc <- z
  mc <- m1
  d <- matrix(0, n, n)
  for (cyc in seq_len(ncycle)) {
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        d[i, j] <- sqrt(alfVn2(x[i, ] - x[j, ]))
      }
    }
    zn <- array(0, c(n, n, cz))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        zn[i, j, ] <- alfLin(alfOnehot(d[i, j], bins), wd) + alfLnorm(zc[i, j, ])
      }
    }
    zc <- zn
    mn <- matrix(0, n, ncol(mc))
    for (i in seq_len(n)) mn[i, ] <- alfLnorm(mc[i, ])
    mc <- mn
  }
  list(
    z = zc, m1 = mc, d = d, estimate = mean(zc), n = n, ncycle = ncycle,
    method = "AlphaFold recycling embedder"
  )
}
