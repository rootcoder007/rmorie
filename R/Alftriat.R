# SPDX-License-Identifier: AGPL-3.0-or-later

#' Triangular gated self-attention over the AlphaFold pair representation
#'
#' Algorithms 13 and 14 of the Supplementary Information to Jumper et al.
#' (2021), pp. 19-20.  Edge \code{ij} attends over the edges sharing its
#' starting node \code{i} or its ending node \code{j}.  The attention logit
#' is a scaled query-key product modulated by a bias taken from the third
#' edge of the triangle, which is what makes it triangular rather than
#' ordinary attention.
#'
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param wq,wk,wv Lists of per-head projection matrices, each \code{c x cz}.
#' @param wb Per-head bias projection, \code{nhead x cz} (line 3).
#' @param wg List of per-head gate projections, each \code{c x cz} (line 4).
#' @param wo Output projection, \code{cz x (nhead * c)} (line 7).
#' @param mode Either "starting" (Algorithm 13) or "ending" (Algorithm 14).
#' @return A list with \code{z}, the attention array \code{attn},
#'   \code{estimate}, \code{n}, \code{mode} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithms 13-14
Alftriat <- function(z, wq, wk, wv, wb, wg, wo, mode = "starting") {
  if (!mode %in% c("starting", "ending"))
    stop("mode must be 'starting' or 'ending'")
  n <- dim(z)[1]
  cz <- dim(z)[3]
  nh <- length(wq)
  cc <- nrow(wq[[1]])
  scale <- 1 / sqrt(cc)

  zn <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) zn[i, j, ] <- alfLnorm(z[i, j, ])

  q <- k <- v <- g <- vector("list", nh)
  b <- vector("list", nh)
  for (h in seq_len(nh)) {
    q[[h]] <- array(0, c(n, n, cc)); k[[h]] <- array(0, c(n, n, cc))
    v[[h]] <- array(0, c(n, n, cc)); g[[h]] <- array(0, c(n, n, cc))
    b[[h]] <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      x <- zn[i, j, ]
      q[[h]][i, j, ] <- alfLin(x, wq[[h]])
      k[[h]][i, j, ] <- alfLin(x, wk[[h]])
      v[[h]][i, j, ] <- alfLin(x, wv[[h]])
      g[[h]][i, j, ] <- alfSigm(alfLin(x, wg[[h]]))
      b[[h]][i, j] <- alfVdot(as.numeric(wb[h, ]), x)
    }
  }

  attn <- array(0, c(nh, n, n, n))
  o <- vector("list", nh)
  for (h in seq_len(nh)) {
    o[[h]] <- array(0, c(n, n, cc))
    for (i in seq_len(n)) for (j in seq_len(n)) {
      logits <- numeric(n)
      for (kk in seq_len(n)) {
        logits[kk] <- if (mode == "starting")
          scale * alfVdot(q[[h]][i, j, ], k[[h]][i, kk, ]) + b[[h]][j, kk]
        else
          scale * alfVdot(q[[h]][i, j, ], k[[h]][kk, j, ]) + b[[h]][kk, i]
      }
      a <- alfSmax(logits)
      attn[h, i, j, ] <- a
      ov <- numeric(cc)
      for (t in seq_len(cc)) {
        tot <- 0
        for (kk in seq_len(n)) {
          tot <- tot + a[kk] * (if (mode == "starting") v[[h]][i, kk, t]
                                else v[[h]][kk, j, t])
        }
        ov[t] <- tot
      }
      o[[h]][i, j, ] <- g[[h]][i, j, ] * ov
    }
  }

  out <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    cat_ <- numeric(0)
    for (h in seq_len(nh)) cat_ <- c(cat_, o[[h]][i, j, ])
    out[i, j, ] <- alfLin(cat_, wo)
  }

  list(z = out, attn = attn, estimate = mean(out), n = n, mode = mode,
       method = paste0("AlphaFold triangular self-attention (", mode,
                       " node)"))
}
