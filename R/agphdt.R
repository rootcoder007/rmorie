# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero policy head
#'
#' Silver et al. (2017), Nature 550, 354-359, methods, "Neural network
#' architecture": a 1x1 convolution to two planes, batch normalisation, a
#' rectifier, then a fully connected layer to the action space plus one,
#' whose outputs are the logits of the move distribution.  Silver et al.
#' (2018), arXiv:1712.01815 (FETCHED), keeps the same head.  The Nature
#' paper is paywalled; the layer list is reproduced identically
#' everywhere, and its only numeric content -- logits softmaxed over legal
#' moves -- is unambiguous.
#'
#' @param x the feature planes, flattened.
#' @param action_space number of actions.
#' @param W optional fully connected weights, one row per action.
#' @param legal optional legal-move mask.
#' @return list: estimate, p, logits, entropy, method.
#' @keywords internal
#' @examples
#' Policyhead(c(1, 2, 0), 3)$p
#' @export
Policyhead <- function(x, action_space = NULL, W = NULL, legal = NULL) {
  f <- .s03vec(x)
  if (!is.null(W)) {
    logits <- .s03matvec(.s03mat(W), f)
  } else {
    m <- if (!is.null(action_space)) as.integer(action_space) else length(f)
    logits <- if (length(f) < m) c(f, rep(0, m - length(f))) else f[seq_len(m)]
  }
  m <- length(logits)
  shifted <- logits
  if (!is.null(legal)) {
    mask <- as.numeric(as.logical(legal))
    shifted[mask <= 0] <- -Inf
  }
  p <- .s03softmax(shifted[is.finite(shifted)])
  out <- numeric(m); j <- 1L
  for (i in seq_len(m)) {
    if (!is.finite(shifted[i])) {
      out[i] <- 0
    } else {
      out[i] <- p[j]; j <- j + 1L
    }
  }
  h <- 0
  for (q in out) if (q > 0) h <- h - q * log(q)
  list(estimate = if (m > 0L) out[1] else NaN, p = out, logits = logits,
       entropy = h,
       method = "AlphaZero policy head: linear logits, mask, softmax")
}
