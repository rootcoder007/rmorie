# SPDX-License-Identifier: AGPL-3.0-or-later
#' ViT self-attention: A = softmax(q k^T / sqrt(D_h)), SA(z) = A v
#'
#' SOURCE.  Dosovitskiy et al. (2021), "An Image is Worth 16x16 Words:
#' Transformers for Image Recognition at Scale", ICLR 2021; arXiv:2010.11929v2,
#' Appendix A "Multihead Self-attention", p. 13.  Read from the PDF rendered as
#' a page image.
#'
#' \[q, k, v\] = z U_qkv, U_qkv in R^\{D x 3 D_h\} (5); A = softmax(q k^T /
#' sqrt(D_h)), A in R^\{N x N\} (6); SA(z) = A v (7); MSA(z) = \[SA_1(z);
#' SA_2(z); ...; SA_k(z)\] U_msa, U_msa in R^\{(k . D_h) x D\} (8), with D_h
#' "typically set to D/k" (text under Eq. (8), p. 13).  The underlying
#' construction is Vaswani et al. (2017), "Attention Is All You Need",
#' NeurIPS 30, which the appendix cites; the paper reproduces it unchanged.
#'
#' This module is Eqs. (6) and (7): it takes q, k, v already projected and
#' returns the attention matrix and A v.  The projection (5) and the multi-head
#' concatenation (8) are done by Vitfwd, which is where U_qkv and U_msa live.
#'
#' The softmax in Eq. (6) is over the key axis, i.e. row-wise on q k^T, so that
#' each row of A is a set of weights summing to one -- "a weighted sum over all
#' values v in the sequence" (p. 13).  The scale is 1/sqrt(D_h) where D_h is
#' the query/key width, not the value width; q and k must therefore share a
#' width, while v need not.
#'
#' The mask is not in the paper -- ViT attends over the whole sequence -- and is
#' carried because the stub's signature declares it.  A zero (or FALSE) entry
#' means "this key is not visible to this query" and is set to -Inf before the
#' softmax; an all-zero mask row is an error rather than a silent NaN.
#'
#' @param q N-by-D_h queries.
#' @param k M-by-D_h keys; D_h must match q.
#' @param v M-by-D_v values.
#' @param mask N-by-M of 0/1 (or FALSE/TRUE), or NULL.  Zero entries excluded.
#' @return list: estimate, attn, output, scale, n_query, n_key, d_head,
#'   d_value, n, method.
#' @keywords internal
#' @examples
#' z <- matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE)
#' rowSums(Vitatt(z, z, z)$attn)
#' @export
Vitatt <- function(q, k, v, mask = NULL) {
  Q <- .s03mat(q)
  K <- .s03mat(k)
  V <- .s03mat(v)
  n <- nrow(Q)
  m <- nrow(K)
  if (n < 1L || m < 1L) {
    stop("vit_self_attention: q and k must have at least one row")
  }
  dh <- ncol(Q)
  if (ncol(K) != dh) {
    stop("vit_self_attention: q and k must have the same width D_h")
  }
  if (nrow(V) != m) {
    stop("vit_self_attention: k and v must have the same number of rows")
  }
  dv <- ncol(V)
  scale <- 1 / sqrt(dh)
  M <- NULL
  if (!is.null(mask)) {
    M <- .s03mat(mask)
    if (nrow(M) != n || ncol(M) != m) {
      stop("vit_self_attention: mask must be N-by-M")
    }
  }
  A <- matrix(0, n, m)
  for (i in seq_len(n)) {
    logits <- numeric(m)
    for (j in seq_len(m)) {
      s <- 0
      for (t in seq_len(dh)) s <- s + Q[i, t] * K[j, t]
      logits[j] <- s * scale
    }
    if (!is.null(M)) {
      allowed <- 0L
      for (j in seq_len(m)) {
        if (M[i, j] != 0) allowed <- allowed + 1L else logits[j] <- -Inf
      }
      if (allowed == 0L) {
        stop("vit_self_attention: a mask row excludes every key")
      }
    }
    A[i, ] <- .s03softmax(logits)
  }
  out <- matrix(0, n, dv)
  for (i in seq_len(n)) {
    for (c in seq_len(dv)) {
      s <- 0
      for (j in seq_len(m)) s <- s + A[i, j] * V[j, c]
      out[i, c] <- s
    }
  }
  tot <- 0
  for (i in seq_len(n)) for (c in seq_len(dv)) tot <- tot + out[i, c]
  list(estimate = tot / (n * dv), attn = A, output = out, scale = scale,
       n_query = n, n_key = m, d_head = dh, d_value = dv, n = n,
       method = paste0("A = softmax(q k^T / sqrt(D_h)); SA(z) = A v ",
                       "(Dosovitskiy et al. 2021, Eqs. (6)-(7) p. 13)"))
}
