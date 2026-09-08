# SPDX-License-Identifier: AGPL-3.0-or-later

#' Predicted aligned error (PAE) head of AlphaFold
#'
#' Supplement section 1.9.7 of Jumper et al. (2021), p. 38.  The error in
#' the position of residue \code{j} when prediction and truth are aligned
#' on residue \code{i} is predicted as a distribution over distance bins;
#' the reported PAE is that distribution's mean.
#'
#' Unlike the distogram head this one does not symmetrise the pair
#' representation: aligning on \code{i} and reading off \code{j} is a
#' different question from the reverse, so the matrix is deliberately
#' non-symmetric.
#'
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param w Projection to the bin logits.
#' @param bins Bin centres; defaults to 64 bins of width 0.5 A over 0-31.5 A.
#' @return A list with the \code{pae} matrix, the distributions \code{p},
#'   \code{estimate}, \code{n} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. section 1.9.7
Alfpae <- function(z, w, bins = NULL) {
  if (is.null(bins)) bins <- 0.25 + 0.5 * (seq_len(64) - 1)
  n <- dim(z)[1]
  nb <- length(bins)
  pae <- matrix(0, n, n)
  ps <- array(0, c(n, n, nb))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      p <- alfSmax(alfLin(z[i, j, ], w))
      ps[i, j, ] <- p
      pae[i, j] <- sum(p * bins)
    }
  }
  list(
    pae = pae, p = ps, estimate = mean(pae), n = n,
    method = "AlphaFold predicted aligned error"
  )
}
