# SPDX-License-Identifier: AGPL-3.0-or-later

#' Initial representation embeddings of AlphaFold
#'
#' Algorithms 3-5 of the Supplementary Information to Jumper et al. (2021),
#' p. 13.  The pair representation starts as an outer sum of two
#' projections of the target features plus a relative position encoding;
#' the MSA representation starts as a projection of the MSA features with
#' the target projection broadcast across sequences.
#'
#' The relative position encoding clips the separation at 32 residues, so
#' nothing beyond that is distinguished.  That is deliberate: it
#' de-emphasises primary sequence distance and lets the network run on
#' chains longer than those it was trained on.
#'
#' @param target_feat Per-residue target features, an \code{n x ctf} matrix.
#' @param residue_index Residue index per position; only differences matter.
#' @param msa_feat MSA features, an \code{s x n x cmf} array.
#' @param wa,wb The two target projections of line 1, each \code{cz x ctf}.
#' @param wrel Relative position projection, \code{cz x length(bins)}.
#' @param wmsa MSA feature projection, \code{cm x cmf}.
#' @param wtgt Target projection added to every MSA row, \code{cm x ctf}.
#' @param bins Relative position bins; defaults to -32, ..., 32.
#' @return A list with the initial \code{z} and \code{m}, the relative
#'   position contribution \code{pos}, \code{estimate}, \code{n} and
#'   \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithms 3-5
Alfembed <- function(target_feat, residue_index, msa_feat, wa, wb, wrel,
                     wmsa, wtgt, bins = NULL) {
  if (is.null(bins)) bins <- -32:32
  n <- nrow(target_feat)
  cz <- nrow(wa)
  a <- t(vapply(seq_len(n), function(i) alfLin(target_feat[i, ], wa), numeric(cz)))
  b <- t(vapply(seq_len(n), function(i) alfLin(target_feat[i, ], wb), numeric(cz)))

  pos <- array(0, c(n, n, cz))
  z <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    p <- alfLin(alfOnehot(residue_index[i] - residue_index[j], bins), wrel)
    pos[i, j, ] <- p
    z[i, j, ] <- a[i, ] + b[j, ] + p
  }

  s <- dim(msa_feat)[1]
  cm <- nrow(wmsa)
  m <- array(0, c(s, n, cm))
  for (si in seq_len(s)) for (i in seq_len(n))
    m[si, i, ] <- alfLin(msa_feat[si, i, ], wmsa) + alfLin(target_feat[i, ], wtgt)

  list(z = z, m = m, pos = pos, estimate = mean(z), n = n,
       method = "AlphaFold initial representation embeddings")
}
