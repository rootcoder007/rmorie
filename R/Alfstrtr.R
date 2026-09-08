# SPDX-License-Identifier: AGPL-3.0-or-later

#' Transition layer of the AlphaFold structure module
#'
#' Algorithm 20 lines 7-9 of the Supplementary Information to Jumper et al.
#' (2021), p. 26.  A three-layer residual MLP applied to the single
#' representation between invariant point attention and the backbone
#' update, followed by layer normalisation.  Unlike the Evoformer
#' transitions this one keeps the channel width constant.
#'
#' @param s Single representation, an \code{n x cs} matrix.
#' @param w1,w2,w3 The three projections of line 8, each \code{cs x cs}.
#' @param layernorm Apply the layer normalisation of line 9.
#' @param drop Optional multiplicative dropout mask, \code{n x cs}.
#' @return A list with \code{s}, \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 20
Alfstrtr <- function(s, w1, w2, w3, layernorm = TRUE, drop = NULL) {
  n <- nrow(s)
  out <- matrix(0, n, ncol(s))
  for (i in seq_len(n)) {
    h <- alfLin(s[i, ], w1)
    h <- alfLin(alfRelu(h), w2)
    h <- alfLin(alfRelu(h), w3)
    u <- as.numeric(s[i, ]) + h
    if (!is.null(drop)) u <- u * as.numeric(drop[i, ])
    out[i, ] <- if (layernorm) alfLnorm(u) else u
  }
  list(
    s = out, estimate = mean(out), n = n,
    method = "AlphaFold structure module transition"
  )
}
