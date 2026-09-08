# SPDX-License-Identifier: AGPL-3.0-or-later

#' Per-residue model confidence (pLDDT) head of AlphaFold
#'
#' Algorithm 29 of the Supplementary Information to Jumper et al. (2021),
#' p. 37.  A small MLP on the single representation produces a distribution
#' over lDDT bins; the reported confidence is that distribution's mean.
#'
#' Line 4 of the published pseudocode writes the confidence loss without a
#' leading minus sign, which would make it something to maximise while
#' equation (7) adds it to a total that is minimised.  The reference
#' implementation uses the negative log likelihood, so that is what is
#' computed here; the sign in the supplement is a typo.
#'
#' @param s Single representation, an \code{n x cs} matrix.
#' @param w1,w2 The two projections of line 1, each followed by a relu.
#' @param w3 Projection to the bin logits (line 2).
#' @param bins Bin centres; defaults to the spec's 1, 3, ..., 99.
#' @param rtrue Optional ground-truth lDDT per residue, giving the loss.
#' @return A list with \code{plddt}, the bin distributions \code{p}, the
#'   \code{loss}, \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 29
Alfplddt <- function(s, w1, w2, w3, bins = NULL, rtrue = NULL) {
  if (is.null(bins)) bins <- 1 + 2 * (seq_len(50) - 1)
  n <- nrow(s)
  nb <- length(bins)
  ps <- matrix(0, n, nb)
  r <- numeric(n)
  for (i in seq_len(n)) {
    a <- alfRelu(alfLin(alfLnorm(s[i, ]), w1))
    a <- alfRelu(alfLin(a, w2))
    p <- alfSmax(alfLin(a, w3))
    ps[i, ] <- p
    r[i] <- sum(p * bins)
  }
  loss <- NULL
  if (!is.null(rtrue)) {
    loss <- mean(vapply(
      seq_len(n),
      function(i) alfXent(alfOnehot(rtrue[i], bins), ps[i, ]),
      numeric(1)
    ))
  }
  list(
    plddt = r, p = ps, loss = loss, estimate = mean(r), n = n,
    method = "AlphaFold per-residue confidence (pLDDT)"
  )
}
