# SPDX-License-Identifier: AGPL-3.0-or-later

#' MSA row-wise and column-wise gated self-attention (AlphaFold)
#'
#' Algorithms 7 and 8 of the Supplementary Information to Jumper et al.
#' (2021), pp. 15-16.  The row-wise form attends over residues within one
#' sequence and adds a bias read off the pair representation, which is how
#' the pair stack speaks back to the MSA stack.  The column-wise form
#' attends over sequences at a fixed residue and carries no pair bias.
#'
#' @param m MSA representation, an \code{s x n x cm} array.
#' @param wq,wk,wv,wg Lists of per-head projection matrices, each \code{c x cm}.
#' @param wo Output projection, \code{cm x (nhead * c)}.
#' @param z Pair representation \code{n x n x cz}; required when mode is "row".
#' @param wb Per-head pair-bias projection \code{nhead x cz}; required when
#'   mode is "row" (line 3 of Algorithm 7).
#' @param mode Either "row" (Algorithm 7) or "column" (Algorithm 8).
#' @return A list with the MSA update \code{m}, the attention \code{attn},
#'   \code{estimate}, \code{n}, \code{s}, \code{mode} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithms 7-8
Alfmsaat <- function(m, wq, wk, wv, wg, wo, z = NULL, wb = NULL,
                     mode = "row") {
  if (!mode %in% c("row", "column")) stop("mode must be 'row' or 'column'")
  if (mode == "row" && (is.null(z) || is.null(wb))) {
    stop("mode='row' needs the pair representation z and wb")
  }
  s <- dim(m)[1]
  n <- dim(m)[2]
  nh <- length(wq)
  cc <- nrow(wq[[1]])
  scale <- 1 / sqrt(cc)

  mn <- array(0, dim(m))
  for (si in seq_len(s)) for (i in seq_len(n)) mn[si, i, ] <- alfLnorm(m[si, i, ])

  q <- k <- v <- g <- vector("list", nh)
  for (h in seq_len(nh)) {
    q[[h]] <- array(0, c(s, n, cc))
    k[[h]] <- array(0, c(s, n, cc))
    v[[h]] <- array(0, c(s, n, cc))
    g[[h]] <- array(0, c(s, n, cc))
    for (si in seq_len(s)) {
      for (i in seq_len(n)) {
        x <- mn[si, i, ]
        q[[h]][si, i, ] <- alfLin(x, wq[[h]])
        k[[h]][si, i, ] <- alfLin(x, wk[[h]])
        v[[h]][si, i, ] <- alfLin(x, wv[[h]])
        g[[h]][si, i, ] <- alfSigm(alfLin(x, wg[[h]]))
      }
    }
  }

  bias <- NULL
  if (mode == "row") {
    bias <- vector("list", nh)
    for (h in seq_len(nh)) {
      bias[[h]] <- matrix(0, n, n)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          bias[[h]][i, j] <- alfVdot(as.numeric(wb[h, ]), alfLnorm(z[i, j, ]))
        }
      }
    }
  }

  nk <- if (mode == "row") n else s
  attn <- array(0, c(nh, s, n, nk))
  o <- vector("list", nh)
  for (h in seq_len(nh)) {
    o[[h]] <- array(0, c(s, n, cc))
    for (si in seq_len(s)) {
      for (i in seq_len(n)) {
        logits <- numeric(nk)
        if (mode == "row") {
          for (j in seq_len(n)) {
            logits[j] <- scale * alfVdot(q[[h]][si, i, ], k[[h]][si, j, ]) +
              bias[[h]][i, j]
          }
        } else {
          for (t2 in seq_len(s)) {
            logits[t2] <- scale * alfVdot(q[[h]][si, i, ], k[[h]][t2, i, ])
          }
        }
        a <- alfSmax(logits)
        attn[h, si, i, ] <- a
        ov <- numeric(cc)
        for (t in seq_len(cc)) {
          tot <- 0
          for (u in seq_len(nk)) {
            tot <- tot + a[u] * (if (mode == "row") {
              v[[h]][si, u, t]
            } else {
              v[[h]][u, i, t]
            })
          }
          ov[t] <- tot
        }
        o[[h]][si, i, ] <- g[[h]][si, i, ] * ov
      }
    }
  }

  cm <- nrow(wo)
  out <- array(0, c(s, n, cm))
  for (si in seq_len(s)) {
    for (i in seq_len(n)) {
      cat_ <- numeric(0)
      for (h in seq_len(nh)) cat_ <- c(cat_, o[[h]][si, i, ])
      out[si, i, ] <- alfLin(cat_, wo)
    }
  }

  list(
    m = out, attn = attn, estimate = mean(out), n = n, s = s, mode = mode,
    method = paste0("AlphaFold MSA gated self-attention (", mode, "-wise)")
  )
}
