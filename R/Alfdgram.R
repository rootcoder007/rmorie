# SPDX-License-Identifier: AGPL-3.0-or-later

#' Distogram prediction head of AlphaFold
#'
#' Supplement section 1.9.8 of Jumper et al. (2021), p. 39.  The pair
#' representation is symmetrised, projected to distance bins and passed
#' through a softmax.  Symmetrising is the point: a distance is symmetric,
#' and feeding the head \code{z_ij + z_ji} guarantees the prediction
#' respects that whatever the trunk produced.
#'
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param w Projection to the bin logits.
#' @param bins Bin centres; defaults to 64 bins from 2 to 22 angstrom.
#' @param dtrue Optional ground-truth distances, giving the cross-entropy
#'   of equation (41).
#' @return A list with the distributions \code{p}, expected distances
#'   \code{dist}, the \code{loss}, \code{estimate}, \code{n} and
#'   \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. section 1.9.8
Alfdgram <- function(z, w, bins = NULL, dtrue = NULL) {
  if (is.null(bins)) {
    nbins <- 64L
    wdt <- (22 - 2) / (nbins - 1)
    bins <- 2 + wdt * (seq_len(nbins) - 1)
  }
  n <- dim(z)[1]
  nb <- length(bins)
  ps <- array(0, c(n, n, nb))
  dd <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    sym <- as.numeric(z[i, j, ]) + as.numeric(z[j, i, ])
    p <- alfSmax(alfLin(sym, w))
    ps[i, j, ] <- p
    dd[i, j] <- sum(p * bins)
  }
  loss <- NULL
  if (!is.null(dtrue)) {
    tot <- 0
    for (i in seq_len(n)) for (j in seq_len(n))
      tot <- tot + alfXent(alfOnehot(dtrue[i, j], bins), ps[i, j, ])
    loss <- tot / (n * n)
  }
  list(p = ps, dist = dd, loss = loss, estimate = mean(dd), n = n,
       method = "AlphaFold distogram prediction")
}
