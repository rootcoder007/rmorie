# SPDX-License-Identifier: AGPL-3.0-or-later

#' Triangular multiplicative update of the AlphaFold pair representation
#'
#' Algorithms 11 and 12 of the Supplementary Information to Jumper et al.
#' (2021), p. 18.  Each edge \code{ij} is updated from the other two edges
#' of every triangle it belongs to: the "outgoing" form contracts over
#' \code{a_ik * b_jk}, the "incoming" form over \code{a_ki * b_kj}.  Those
#' are the only lines that differ between the two algorithms.
#'
#' All weights are supplied by the caller, so the result is a fixed
#' function of its arguments.
#'
#' @param z Pair representation, an \code{n x n x cz} array.
#' @param wag,wav,wbg,wbv Gate and value projections for the left and
#'   right edges, each \code{c x cz} (line 2).
#' @param wg Output gate projection, \code{cz x cz} (line 3).
#' @param wo Output projection, \code{cz x c} (line 4).
#' @param mode Either "outgoing" (Algorithm 11) or "incoming" (Algorithm 12).
#' @param layernorm Apply the layer normalisations of lines 1 and 4.
#'   Setting it to FALSE exposes the degenerate single-channel reduction to
#'   a plain matrix product.
#' @return A list with the updated pair tensor \code{z}, its mean
#'   \code{estimate}, \code{n}, \code{mode} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithms 11-12
Alftrimu <- function(z, wag, wav, wbg, wbv, wg, wo, mode = "outgoing",
                     layernorm = TRUE) {
  if (!mode %in% c("outgoing", "incoming"))
    stop("mode must be 'outgoing' or 'incoming'")
  n <- dim(z)[1]
  cz <- dim(z)[3]
  cc <- nrow(wag)

  zn <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n))
    zn[i, j, ] <- if (layernorm) alfLnorm(z[i, j, ]) else as.numeric(z[i, j, ])

  a <- array(0, c(n, n, cc))
  b <- array(0, c(n, n, cc))
  g <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    v <- zn[i, j, ]
    a[i, j, ] <- alfSigm(alfLin(v, wag)) * alfLin(v, wav)
    b[i, j, ] <- alfSigm(alfLin(v, wbg)) * alfLin(v, wbv)
    g[i, j, ] <- alfSigm(alfLin(v, wg))
  }

  out <- array(0, c(n, n, cz))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    s <- numeric(cc)
    for (q in seq_len(cc)) {
      tot <- 0
      for (k in seq_len(n)) {
        tot <- tot + if (mode == "outgoing") a[i, k, q] * b[j, k, q]
                     else a[k, i, q] * b[k, j, q]
      }
      s[q] <- tot
    }
    if (layernorm) s <- alfLnorm(s)
    out[i, j, ] <- g[i, j, ] * alfLin(s, wo)
  }

  list(z = out, estimate = mean(out), n = n, mode = mode,
       method = paste0("AlphaFold triangular multiplicative update (",
                       mode, ")"))
}
