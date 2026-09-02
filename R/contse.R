# SPDX-License-Identifier: AGPL-3.0-or-later

#' SimCSE contrastive sentence embedding
#'
#' Formula: InfoNCE on dropout-augmented pairs
#'
#' l_i = -log exp(sim(h_i, h_i+)/tau) / sum_j exp(sim(h_i, h_j+)/tau).
#' The positive is the SAME sentence encoded twice under different
#' dropout masks, so the objective needs no labelled pairs at all.  When
#' every similarity is identical the loss is exactly log N, the value it
#' must return on a degenerate batch.
#'
#' @param sentences An n x d matrix of sentence embeddings.
#' @param tau Temperature, strictly positive.
#' @param dropout Dropout rate in [0, 1) used to build the two views.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{loss}, \code{per_item},
#'   \code{alignment}, \code{uniformity}, \code{n}, \code{d},
#'   \code{method}.
#' @references Gao, Yao & Chen (2021), SimCSE, EMNLP 2021:6894-6910.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Contse(M)
Contse <- function(sentences, tau = 0.05, dropout = 0.1, seed = 42) {
  H <- .s03mat(sentences)
  n <- nrow(H)
  if (n == 0L) stop("empty input: sentences has no rows")
  d <- ncol(H)
  if (!(tau > 0)) stop("tau must be strictly positive")
  if (!(dropout >= 0 && dropout < 1)) stop("dropout must lie in [0, 1)")
  e <- .ghc_rng(seed)
  keep <- 1 - dropout
  A <- matrix(0, n, d); B <- matrix(0, n, d)
  for (i in seq_len(n)) {
    ra <- numeric(d); rb <- numeric(d)
    for (k in seq_len(d)) {
      ma <- if (.ghc_unif(e, 1L) < dropout) 0 else 1 / keep
      mb <- if (.ghc_unif(e, 1L) < dropout) 0 else 1 / keep
      ra[k] <- H[i, k] * ma
      rb[k] <- H[i, k] * mb
    }
    A[i, ] <- .clip_l2norm(ra)
    B[i, ] <- .clip_l2norm(rb)
  }
  per <- numeric(n)
  for (i in seq_len(n)) {
    s <- numeric(n)
    for (j in seq_len(n)) {
      acc <- 0
      for (k in seq_len(d)) acc <- acc + A[i, k] * B[j, k]
      s[j] <- acc / tau
    }
    mx <- max(s)
    tot <- 0
    for (v in s) tot <- tot + exp(v - mx)
    per[i] <- mx + log(tot) - s[i]
  }
  loss <- 0
  for (v in per) loss <- loss + v
  loss <- loss / n
  align <- 0
  for (i in seq_len(n)) for (k in seq_len(d))
    align <- align + (A[i, k] - B[i, k])^2
  align <- align / n
  unif <- 0; cnt <- 0L
  if (n > 1L) for (i in seq_len(n - 1L)) for (j in seq(i + 1L, n)) {
    dd <- 0
    for (k in seq_len(d)) dd <- dd + (A[i, k] - A[j, k])^2
    unif <- unif + exp(-2 * dd)
    cnt <- cnt + 1L
  }
  unif <- if (cnt > 0L) log(unif / cnt) else NaN
  .t1_result(estimate = loss, loss = loss, per_item = per,
             alignment = align, uniformity = unif, n = n, d = d,
             method = "SimCSE contrastive sentence objective")
}
