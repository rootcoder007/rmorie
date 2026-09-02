# SPDX-License-Identifier: AGPL-3.0-or-later
#' BERTScore precision, recall and F1
#'
#' With pre-normalised embeddings, R = mean_i max_j x_i' xhat_j,
#' P = mean_j max_i x_i' xhat_j and F = 2PR/(P+R); idf weights re-weight
#' the recall sum.
#'
#' @param reference Reference token embeddings, k x d.
#' @param candidate Candidate token embeddings, l x d.
#' @param idf Importance weight per reference token, or NULL.
#'
#' @return List with P, R, F, recallmatch, precmatch, k, l, d.
#' @references Zhang, Kishore, Wu, Weinberger and Artzi (2020), ICLR;
#'   arXiv:1904.09675, Sect. 3.  Read from the ar5iv rendering.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Bertscore(V, V)
Bertscore <- function(reference, candidate, idf = NULL) {
  X <- .t1_mat(reference)
  Y <- .t1_mat(candidate)
  k <- nrow(X)
  d <- ncol(X)
  l <- nrow(Y)
  if (ncol(Y) != d) stop("embeddings must share their dimension")
  nx <- sqrt(rowSums(X^2))
  ny <- sqrt(rowSums(Y^2))
  if (any(nx == 0) || any(ny == 0)) stop("embeddings must be non-zero")
  Xn <- X / nx
  Yn <- Y / ny
  dim(Xn) <- c(k, d)
  dim(Yn) <- c(l, d)
  Sim <- Xn %*% t(Yn)
  dim(Sim) <- c(k, l)
  rm <- apply(Sim, 1L, max)
  pm <- apply(Sim, 2L, max)
  w <- if (is.null(idf)) rep(1, k) else .t1_vec(idf)
  if (length(w) != k) stop("idf must have one weight per reference token")
  if (sum(w) <= 0) stop("idf weights must not all be zero")
  R <- sum(w * rm) / sum(w)
  P <- sum(pm) / l
  Fv <- if (P + R == 0) 0 else 2 * P * R / (P + R)
  .t1_result(P = P, R = R, F = Fv, recallmatch = rm, precmatch = pm,
             k = k, l = l, d = d,
             method = "BERTScore greedy cosine matching (Zhang et al. 2020)")
}
