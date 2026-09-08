# SPDX-License-Identifier: AGPL-3.0-or-later

#' Template pointwise attention of AlphaFold
#'
#' Algorithm 17 of the Supplementary Information to Jumper et al. (2021),
#' p. 21.  For each residue pair the pair representation queries the stack
#' of template features at that same pair, and the templates are pooled by
#' attention.  The attention runs over templates only, never across pairs,
#' which is why it is pointwise: pair \code{ij} never sees pair \code{kl}.
#'
#' @param t Template pair features, an \code{ntempl x n x n x ct} array.
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param wq List of per-head query projections from \code{z}, \code{c x cz}.
#' @param wk,wv Lists of per-head key and value projections from \code{t},
#'   each \code{c x ct}.
#' @param wo Output projection, \code{cz x (nhead * c)}.
#' @return A list with \code{z}, \code{attn}, \code{estimate}, \code{n},
#'   \code{ntempl} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 17
Alftmpl <- function(t, z, wq, wk, wv, wo) {
  nt <- dim(t)[1]
  n <- dim(z)[1]
  nh <- length(wq)
  cc <- nrow(wq[[1]])
  scale <- 1 / sqrt(cc)

  attn <- array(0, c(nh, n, n, nt))
  o <- vector("list", nh)
  for (h in seq_len(nh)) {
    o[[h]] <- array(0, c(n, n, cc))
    for (i in seq_len(n)) for (j in seq_len(n)) {
      q <- alfLin(z[i, j, ], wq[[h]])
      logits <- numeric(nt)
      vv <- matrix(0, nt, cc)
      for (st in seq_len(nt)) {
        logits[st] <- scale * alfVdot(q, alfLin(t[st, i, j, ], wk[[h]]))
        vv[st, ] <- alfLin(t[st, i, j, ], wv[[h]])
      }
      a <- alfSmax(logits)
      attn[h, i, j, ] <- a
      for (u in seq_len(cc)) o[[h]][i, j, u] <- sum(a * vv[, u])
    }
  }

  cz <- nrow(wo)
  out <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    cat_ <- numeric(0)
    for (h in seq_len(nh)) cat_ <- c(cat_, o[[h]][i, j, ])
    out[i, j, ] <- alfLin(cat_, wo)
  }

  list(z = out, attn = attn, estimate = mean(out), n = n, ntempl = nt,
       method = "AlphaFold template pointwise attention")
}
