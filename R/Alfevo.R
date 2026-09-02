# SPDX-License-Identifier: AGPL-3.0-or-later

#' Evoformer block stack (AlphaFold trunk)
#'
#' Algorithm 6 of the Supplementary Information to Jumper et al. (2021),
#' p. 14.  One block runs, in order: MSA row attention with pair bias, MSA
#' column attention, MSA transition, outer product mean into the pair
#' stack, the two triangular multiplicative updates, the two triangular
#' attentions, and the pair transition.  Every sublayer is a residual
#' update.
#'
#' All weights come from the caller and are shared across blocks, as in the
#' published model.  Dropout is applied only if the caller passes explicit
#' masks, so inference is deterministic.
#'
#' @param m MSA representation, an \code{s x n x cm} array.
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param w Named list of weights; see the Python mirror for the key list.
#' @param nblock Number of blocks; a fixed count, with no early exit.
#' @param drop Optional named list of multiplicative dropout masks.
#' @return A list with \code{m}, \code{z}, the single representation
#'   \code{s}, \code{estimate}, \code{nblock} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 6
Alfevo <- function(m, z, w, nblock = 1, drop = NULL) {
  s <- dim(m)[1]
  n <- dim(m)[2]

  # Two-layer transition MLP -- Algorithms 9 and 15.
  trans <- function(x, w1, w2) alfLin(alfRelu(alfLin(alfLnorm(x), w1)), w2)

  dropm <- function(name, upd) {
    if (is.null(drop) || is.null(drop[[name]])) upd else upd * drop[[name]]
  }

  for (blk in seq_len(nblock)) {
    # lines 2-4: MSA stack
    m <- m + dropm("row", Alfmsaat(m, w$rowq, w$rowk, w$rowv, w$rowg,
      w$rowo,
      z = z, wb = w$rowb, mode = "row"
    )$m)
    m <- m + Alfmsaat(m, w$colq, w$colk, w$colv, w$colg, w$colo,
      mode = "column"
    )$m
    u <- array(0, dim(m))
    for (si in seq_len(s)) {
      for (i in seq_len(n)) {
        u[si, i, ] <- trans(m[si, i, ], w$mt1, w$mt2)
      }
    }
    m <- m + u
    # line 5: communication
    z <- z + Alfopm(m, w$opa, w$opb, w$opo)$z
    # lines 6-9: pair stack
    z <- z + dropm(
      "trimulout",
      Alftrimu(z, w$tmoag, w$tmoav, w$tmobg, w$tmobv, w$tmog,
        w$tmoo,
        mode = "outgoing"
      )$z
    )
    z <- z + dropm(
      "trimulin",
      Alftrimu(z, w$tmiag, w$tmiav, w$tmibg, w$tmibv, w$tmig,
        w$tmio,
        mode = "incoming"
      )$z
    )
    z <- z + dropm(
      "triattnstart",
      Alftriat(z, w$tasq, w$task, w$tasv, w$tasb, w$tasg,
        w$taso,
        mode = "starting"
      )$z
    )
    z <- z + dropm(
      "triattnend",
      Alftriat(z, w$taeq, w$taek, w$taev, w$taeb, w$taeg,
        w$taeo,
        mode = "ending"
      )$z
    )
    # line 10: pair transition
    u <- array(0, dim(z))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        u[i, j, ] <- trans(z[i, j, ], w$pt1, w$pt2)
      }
    }
    z <- z + u
  }

  # line 12: the single representation is a projection of the first MSA row
  srep <- matrix(0, n, nrow(w$sout))
  for (i in seq_len(n)) srep[i, ] <- alfLin(m[1, i, ], w$sout)

  list(
    m = m, z = z, s = srep,
    estimate = (sum(m) + sum(z)) / (length(m) + length(z)),
    nblock = nblock, method = "AlphaFold Evoformer stack"
  )
}
