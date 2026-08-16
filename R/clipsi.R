# SPDX-License-Identifier: AGPL-3.0-or-later

#' .clip_l2norm
#'
#' A step of the clipsi implementation. Called by \code{Clipxi}, \code{Colbrt}, \code{Contse} and 1 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.clip_l2norm <- function(v) {
  n <- sqrt(sum(v * v))
  if (n <= 0) stop("cannot normalise a zero-norm embedding")
  v / n
}

#' CLIP image-text similarity
#'
#' Formula: cos(I_emb, T_emb) / tau
#'
#' Both embeddings are L2-normalised first, so the inner product IS the
#' cosine and the logit is bounded by +/- 1/tau.  The diagonal of the
#' logit matrix holds the matched pairs; retrieval takes the row argmax,
#' and the reported accuracy is how often that argmax is the diagonal.
#'
#' @param I_emb An n x d matrix of image embeddings.
#' @param T_emb An n x d matrix of text embeddings, paired row by row.
#' @param tau Temperature, strictly positive.
#' @return List with \code{estimate}, \code{logits}, \code{cosine},
#'   \code{retrieved}, \code{accuracy}, \code{n}, \code{d},
#'   \code{method}.
#' @references Radford et al. (2021), Learning Transferable Visual
#'   Models From Natural Language Supervision, ICML 139:8748-8763.
#' @export
Clipsi <- function(I_emb, T_emb, tau = 0.01) {
  I <- .s03mat(I_emb); TT <- .s03mat(T_emb)
  n <- nrow(I)
  if (n == 0L) stop("empty input: I_emb has no rows")
  if (nrow(TT) != n)
    stop("I_emb and T_emb must have the same number of rows")
  d <- ncol(I)
  if (ncol(TT) != d)
    stop("image and text embeddings must share a dimension")
  if (!(tau > 0)) stop("tau must be strictly positive")
  In <- t(apply(I, 1, .clip_l2norm))
  Tn <- t(apply(TT, 1, .clip_l2norm))
  if (d == 1L) { In <- matrix(In, n, 1L); Tn <- matrix(Tn, n, 1L) }
  cosm <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    s <- 0
    for (k in seq_len(d)) s <- s + In[i, k] * Tn[j, k]
    cosm[i, j] <- s
  }
  logits <- cosm / tau
  retrieved <- integer(n)
  for (i in seq_len(n)) {
    b <- 1L
    for (j in seq_len(n)) if (cosm[i, j] > cosm[i, b]) b <- j
    retrieved[i] <- b
  }
  acc <- sum(retrieved == seq_len(n)) / n
  est <- 0
  for (i in seq_len(n)) est <- est + cosm[i, i]
  .t1_result(estimate = est / n, logits = logits, cosine = cosm,
             retrieved = retrieved - 1L, accuracy = acc, n = n, d = d,
             method = "CLIP image-text cosine similarity")
}
