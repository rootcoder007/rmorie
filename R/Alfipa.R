# SPDX-License-Identifier: AGPL-3.0-or-later

#' Invariant point attention (AlphaFold structure module)
#'
#' Algorithm 22 of the Supplementary Information to Jumper et al. (2021),
#' p. 28.  Attention over residues whose logit carries, besides the usual
#' query-key term and a pair bias, a squared distance between query and key
#' points placed in each residue's own rigid frame.  Because those points
#' are compared in the global frame and the output points are mapped back
#' through the inverse of the receiving frame, the operation is invariant
#' under a global rigid motion of all frames (supplement eq. 3-6).
#'
#' @param s Single representation, an \code{n x cs} matrix.
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param frames List of backbone frames, each \code{list(R = 3x3, t = 3)}.
#' @param wq,wk,wv Lists of per-head scalar projections, each \code{c x cs}.
#' @param wqp,wkp Lists over heads of lists over points of \code{3 x cs}
#'   projections (line 2).
#' @param wvp Lists over heads of lists over points of \code{3 x cs}
#'   projections (line 3).
#' @param wb Per-head pair-bias projection, \code{nhead x cz} (line 4).
#' @param gamma Per-head scalar for the point term of line 7, passed after
#'   the model's softplus so nothing is implicit.
#' @param wo Output projection of line 11.
#' @return A list with the update \code{s}, \code{attn}, the local output
#'   \code{points}, \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 22
Alfipa <- function(s, z, frames, wq, wk, wv, wqp, wkp, wvp, wb, gamma, wo) {
  n <- nrow(s)
  cz <- dim(z)[3]
  nh <- length(wq)
  cc <- nrow(wq[[1]])
  nqp <- length(wqp[[1]])
  npv <- length(wvp[[1]])
  scale <- 1 / sqrt(cc)
  wC <- sqrt(2 / (9 * nqp))
  wL <- sqrt(1 / 3)

  q <- k <- v <- vector("list", nh)
  gq <- gk <- gv <- vector("list", nh)
  b <- vector("list", nh)
  for (h in seq_len(nh)) {
    q[[h]] <- t(vapply(seq_len(n), function(i) alfLin(s[i, ], wq[[h]]), numeric(cc)))
    k[[h]] <- t(vapply(seq_len(n), function(i) alfLin(s[i, ], wk[[h]]), numeric(cc)))
    v[[h]] <- t(vapply(seq_len(n), function(i) alfLin(s[i, ], wv[[h]]), numeric(cc)))
    gq[[h]] <- array(0, c(n, nqp, 3)); gk[[h]] <- array(0, c(n, nqp, 3))
    gv[[h]] <- array(0, c(n, npv, 3))
    for (i in seq_len(n)) {
      for (p in seq_len(nqp)) {
        gq[[h]][i, p, ] <- alfRap(frames[[i]], alfLin(s[i, ], wqp[[h]][[p]]))
        gk[[h]][i, p, ] <- alfRap(frames[[i]], alfLin(s[i, ], wkp[[h]][[p]]))
      }
      for (p in seq_len(npv))
        gv[[h]][i, p, ] <- alfRap(frames[[i]], alfLin(s[i, ], wvp[[h]][[p]]))
    }
    b[[h]] <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n))
      b[[h]][i, j] <- alfVdot(as.numeric(wb[h, ]), z[i, j, ])
  }

  attn <- vector("list", nh)
  pts <- vector("list", nh)
  for (h in seq_len(nh)) {
    attn[[h]] <- matrix(0, n, n)
    pts[[h]] <- array(0, c(n, npv, 3))
    for (i in seq_len(n)) {
      logits <- numeric(n)
      for (j in seq_len(n)) {
        dsq <- 0
        for (p in seq_len(nqp))
          dsq <- dsq + alfVn2(gq[[h]][i, p, ] - gk[[h]][j, p, ])
        logits[j] <- wL * (scale * alfVdot(q[[h]][i, ], k[[h]][j, ]) +
                             b[[h]][i, j] - 0.5 * gamma[h] * wC * dsq)
      }
      a <- alfSmax(logits)
      attn[[h]][i, ] <- a
      for (p in seq_len(npv)) {
        acc <- numeric(3)
        for (t in seq_len(3)) acc[t] <- sum(a * gv[[h]][, p, t])
        pts[[h]][i, p, ] <- alfRinvap(frames[[i]], acc)
      }
    }
  }

  csout <- nrow(wo)
  out <- matrix(0, n, csout)
  for (i in seq_len(n)) {
    cat_ <- numeric(0)
    for (h in seq_len(nh)) {
      a <- attn[[h]][i, ]
      zi <- numeric(cz)
      for (t in seq_len(cz)) zi[t] <- sum(a * z[i, , t])
      cat_ <- c(cat_, zi)
      oi <- numeric(cc)
      for (t in seq_len(cc)) oi[t] <- sum(a * v[[h]][, t])
      cat_ <- c(cat_, oi)
      for (p in seq_len(npv)) cat_ <- c(cat_, pts[[h]][i, p, ])
      for (p in seq_len(npv)) cat_ <- c(cat_, sqrt(alfVn2(pts[[h]][i, p, ])))
    }
    out[i, ] <- alfLin(cat_, wo)
  }

  list(s = out, attn = attn, points = pts, estimate = mean(out), n = n,
       method = "AlphaFold invariant point attention")
}
