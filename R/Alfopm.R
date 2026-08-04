# SPDX-License-Identifier: AGPL-3.0-or-later

#' Outer product mean: MSA-to-pair communication in AlphaFold
#'
#' Algorithm 10 of the Supplementary Information to Jumper et al. (2021),
#' p. 17.  This is the only path by which the MSA stack writes into the
#' pair stack.  Rows \code{i} and \code{j} of the MSA representation are
#' projected, their outer product is averaged over sequences, flattened and
#' projected to the pair channel width.
#'
#' @param m MSA representation, an \code{s x n x cm} array.
#' @param wa,wb The two projections of line 2, each \code{c x cm}.
#' @param wo Output projection, \code{cz x (c * c)} (line 4).
#' @param layernorm Apply the layer normalisation of line 1.  FALSE exposes
#'   the degenerate single-channel reduction to a Gram matrix.
#' @return A list with the pair update \code{z}, the flattened outer product
#'   means \code{o}, \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 10
Alfopm <- function(m, wa, wb, wo, layernorm = TRUE) {
  s <- dim(m)[1]
  n <- dim(m)[2]
  cm <- dim(m)[3]
  cc <- nrow(wa)

  av <- array(0, c(s, n, cc))
  bv <- array(0, c(s, n, cc))
  for (si in seq_len(s)) for (i in seq_len(n)) {
    x <- if (layernorm) alfLnorm(m[si, i, ]) else as.numeric(m[si, i, ])
    av[si, i, ] <- alfLin(x, wa)
    bv[si, i, ] <- alfLin(x, wb)
  }

  # line 3: outer product averaged over sequences, flattened in the same
  # (p, q) order as the Python arm
  o <- array(0, c(n, n, cc * cc))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    f <- numeric(cc * cc)
    idx <- 1L
    for (p in seq_len(cc)) for (q in seq_len(cc)) {
      f[idx] <- sum(av[, i, p] * bv[, j, q]) / s
      idx <- idx + 1L
    }
    o[i, j, ] <- f
  }

  cz <- nrow(wo)
  z <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) z[i, j, ] <- alfLin(o[i, j, ], wo)

  list(z = z, o = o, estimate = mean(z), n = n,
       method = "AlphaFold outer product mean (pair representation update)")
}
