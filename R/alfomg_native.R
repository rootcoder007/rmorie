# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of alfomg -- the OpenFold MSA-pair head. Mirrors
# src/morie/fn/alfomg.py operation for operation on the shared numerics
# in R/aaa_helpers_w3num.R.
#
# An alignment is a stack of sequences; a pair representation is a
# matrix over residue positions. Neither is much use alone. The
# alignment knows which positions covary across evolution and nothing
# about geometry; the pair representation carries the geometry and has
# no memory of the alignment that implied it. The head is the pipe
# between them, and it runs both ways in every Evoformer block.
#
#   MSA -> pair, the OUTER PRODUCT MEAN (AlphaFold 2 Supplementary
#   Algorithm 10):
#
#       o_ij[a][b] = (1/S) sum_s  m[s][i][a] * m[s][j][b]
#
#   The average over sequences is the whole idea. A single sequence
#   contributes a rank one matrix that says nothing; it is the variation
#   ACROSS the alignment that makes the entry informative.
#
#   pair -> MSA, ROW-WISE ATTENTION WITH A PAIR BIAS (Supplementary
#   Algorithm 7): within one sequence, position i attends over j with
#
#       logit_ij = (q_i . k_j) / sqrt(c) + b_ij
#
#   The bias is the only channel through which geometry reaches the
#   alignment, so it is the part worth being able to test.
#
# Everything in the block that is a TRAINED WEIGHT -- the query, key and
# value projections, the sigmoid gate, the linear that turns a pair
# vector into a scalar bias, and the linear that projects the c*c outer
# product back to the pair width -- is a parameter of these functions,
# not a constant of them. The papers give the architecture; the weights
# are a download, not a formula.
#
# References
#   Jumper, J. et al. (2021) "Highly accurate protein structure
#     prediction with AlphaFold." Nature 596, 583-589.
#     doi:10.1038/s41586-021-03819-2. Supplementary Algorithms 7 and 10.
#   Ahdritz, G. et al. (2022) "OpenFold: retraining AlphaFold2 yields
#     new insights into its learning mechanisms and capacity for
#     generalization." bioRxiv 2022.11.20.517210.
#   Vaswani, A. et al. (2017) "Attention is all you need." NeurIPS 30,
#     5998-6008.

.alfomg_shape <- function(msa) {
  s <- length(msa)
  if (s == 0L) stop("an alignment with no sequences has nothing to say")
  r <- length(msa[[1]])
  if (r == 0L) stop("an alignment with no positions has nothing to say")
  cc <- length(msa[[1]][[1]])
  for (row in msa) {
    if (length(row) != r) stop("every sequence must have the same length")
    for (v in row) if (length(v) != cc)
      stop("every position must have the same width")
  }
  c(s, r, cc)
}

#' Softmax, shifted by the maximum before exponentiating
#'
#' The shift is not a nicety. Attention logits carrying a large pair
#' bias overflow the exponential without it, and the shift is exactly
#' cancelled by the normalisation, so it costs nothing.
#'
#' @param logits A numeric vector.
#' @return A vector of the same length summing to one.
#' @export
morie_alfomg_softmax <- function(logits) {
  m <- logits[1]
  for (v in logits) if (v > m) m <- v
  ex <- exp(logits - m)
  ex / .w3_csum(ex)
}

#' MSA to pair: the outer product mean
#'
#' AlphaFold 2 Supplementary Algorithm 10. Returns an r by r list of
#' length-(c*c) vectors, the outer product of the two positions'
#' channel vectors averaged over the alignment. The published block
#' applies a layer normalisation and two learned projections first and
#' one more to the result; with no weights in hand those are the
#' identity, and the caller who has them applies them on either side.
#'
#' Flattened in row-major order, so entry a*c+b is channel a of
#' position i against channel b of position j.
#'
#' @param msa The alignment, a list of sequences, each a list of
#'   channel vectors.
#' @return A list of lists of numeric vectors.
#' @export
morie_alfomg_opm <- function(msa) {
  sh <- .alfomg_shape(msa); s <- sh[1]; r <- sh[2]; cc <- sh[3]
  out <- vector("list", r)
  for (i in seq_len(r)) {
    row <- vector("list", r)
    for (j in seq_len(r)) {
      cell <- numeric(cc * cc)
      p <- 0L
      for (a in seq_len(cc)) for (b in seq_len(cc)) {
        p <- p + 1L
        terms <- vapply(seq_len(s),
                        function(k) msa[[k]][[i]][a] * msa[[k]][[j]][b],
                        numeric(1))
        cell[p] <- .w3_csum(terms) / s
      }
      row[[j]] <- cell
    }
    out[[i]] <- row
  }
  out
}

#' The attention bias read off the pair representation
#'
#' In the trained network this is a linear map with no bias term from
#' the pair channel down to one scalar per attention head. With no
#' weights, the mean over the pair channels stands in for it -- that is
#' this module's choice, not AlphaFold's, and it is the linear map with
#' every weight equal to 1/c, which keeps the bias on the scale of the
#' representation instead of growing with its width.
#'
#' @param pair The pair representation, a list of rows of vectors.
#' @param w The learned map, or NULL.
#' @return An r by r numeric matrix.
#' @export
morie_alfomg_bias <- function(pair, w = NULL) {
  r <- length(pair)
  out <- matrix(0, r, r)
  for (i in seq_len(r)) for (j in seq_len(r)) {
    z <- pair[[i]][[j]]
    out[i, j] <- if (is.null(w)) .w3_csum(z) / length(z) else .w3_dot(z, w)
  }
  out
}

#' Pair to MSA: row-wise attention with a pair bias
#'
#' AlphaFold 2 Supplementary Algorithm 7, one head. Within each
#' sequence, every position attends over every position of the same
#' sequence, with the pair bias added to the logit before the softmax.
#' Query, key and value are the identity here for the reason given in
#' the file header.
#'
#' Multi-head attention is this operation on a slice of the channel
#' axis, so a caller wanting h heads calls it h times on the slices and
#' concatenates -- there is no head-specific arithmetic to hide.
#'
#' @param msa The alignment.
#' @param bias The r by r bias matrix.
#' @param scale The attention temperature; NULL is one over root c.
#' @param gate A per-channel output multiplier, or NULL for ungated.
#' @return A list with the attention weights and the attended output.
#' @export
morie_alfomg_row_attention <- function(msa, bias, scale = NULL,
                                       gate = NULL) {
  sh <- .alfomg_shape(msa); s <- sh[1]; r <- sh[2]; cc <- sh[3]
  if (is.null(scale)) scale <- 1 / sqrt(as.numeric(cc))
  scale <- as.numeric(scale)
  if (nrow(bias) != r || ncol(bias) != r)
    stop("the bias must be one scalar per ordered pair of positions")
  if (!is.null(gate) && length(gate) != cc)
    stop("the gate must be one multiplier per channel")
  attn <- vector("list", s); out <- vector("list", s)
  for (k in seq_len(s)) {
    sq <- msa[[k]]
    A <- vector("list", r); O <- vector("list", r)
    for (i in seq_len(r)) {
      logits <- vapply(seq_len(r),
                       function(j) .w3_dot(sq[[i]], sq[[j]]) * scale +
                         bias[i, j], numeric(1))
      a <- morie_alfomg_softmax(logits)
      A[[i]] <- a
      row <- vapply(seq_len(cc), function(d)
        .w3_csum(vapply(seq_len(r), function(j) a[j] * sq[[j]][d],
                        numeric(1))), numeric(1))
      if (!is.null(gate)) row <- row * as.numeric(gate)
      O[[i]] <- row
    }
    attn[[k]] <- A; out[[k]] <- O
  }
  list(attn = attn, out = out)
}

#' One Evoformer MSA-pair head, both directions
#'
#' @param msa The alignment, indexed sequence, position, channel.
#' @param pair The pair representation, indexed i, j, channel.
#' @param w_bias The learned map from a pair vector to the attention
#'   bias. NULL means the mean over the pair channels.
#' @param w_opm The learned projection from the c*c outer product back
#'   to the pair width, one row per output channel. NULL means the pair
#'   representation is returned unchanged and \code{pair_updated} is
#'   FALSE -- because inventing that projection would be inventing the
#'   model.
#' @param scale The attention temperature; NULL is one over root c.
#' @param gate A per-channel output gate; NULL is ungated.
#' @return A list with the outer product mean, the attention bias and
#'   weights, the updated alignment, and the pair representation.
#' @export
morie_alfomg <- function(msa, pair, w_bias = NULL, w_opm = NULL,
                         scale = NULL, gate = NULL) {
  sh <- .alfomg_shape(msa); s <- sh[1]; r <- sh[2]; cc <- sh[3]
  if (length(pair) != r || any(vapply(pair, length, integer(1)) != r))
    stop("the pair representation must be square over the alignment's ",
         "positions")
  cz <- length(pair[[1]][[1]])

  opm <- morie_alfomg_opm(msa)
  b <- morie_alfomg_bias(pair, w_bias)
  ra <- morie_alfomg_row_attention(msa, b, scale, gate)
  attn <- ra$attn

  if (is.null(w_opm)) {
    pair_out <- pair
    updated <- FALSE
  } else {
    if (any(vapply(w_opm, length, integer(1)) != cc * cc))
      stop("each projection row must span the whole outer product")
    if (length(w_opm) != cz)
      stop("the projection must land on the pair width")
    pair_out <- vector("list", r)
    for (i in seq_len(r)) {
      row <- vector("list", r)
      for (j in seq_len(r))
        row[[j]] <- vapply(seq_len(cz), function(d)
          pair[[i]][[j]][d] + .w3_dot(w_opm[[d]], opm[[i]][[j]]),
          numeric(1))
      pair_out[[i]] <- row
    }
    updated <- TRUE
  }

  # The mass the alignment puts on the diagonal: how much each position
  # attends to itself rather than to its neighbours. It is the one
  # summary of the attention that reads the same regardless of how many
  # sequences or positions there are.
  dg <- numeric(0)
  for (k in seq_len(s)) for (i in seq_len(r))
    dg <- c(dg, attn[[k]][[i]][i])
  list(opm = opm, bias = b, attn = attn, msa_out = ra$out,
       pair_out = pair_out, pair_updated = updated,
       self_attention = .w3_csum(dg) / as.numeric(s * r),
       n_seq = s, n_pos = r, n_channel = cc, n_pair_channel = cz,
       scale = if (is.null(scale)) 1 / sqrt(as.numeric(cc))
               else as.numeric(scale),
       gated = !is.null(gate),
       method = "OpenFold Evoformer MSA-pair head")
}

#' One-line summary of the alfomg module
#'
#' @return A character scalar.
#' @export
morie_alfomg_cheatsheet <- function()
  paste0("alfomg: OpenFold MSA-pair head. Outer product mean for ",
         "MSA->pair, row attention with a pair bias for pair->MSA; ",
         "trained weights are parameters, not constants")
