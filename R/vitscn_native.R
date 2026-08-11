# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Scaled cosine attention, Swin Transformer V2 (Vitscn). Bit-identical
# mirror of src/morie/fn/vitscn.py. The stub's "ViT-2" citation was a
# misattribution (recorded in FABRICATIONS.md); real source is Swin V2.

#' Scaled cosine attention (Swin Transformer V2)
#'
#' Liu et al. (2022), "Swin Transformer V2: Scaling Up Capacity and
#' Resolution", CVPR 2022, arXiv:2111.09883, Section 3.2:
#' Sim(q_i, k_j) = cos(q_i, k_j) / tau + B_ij, with tau a learnable
#' scalar constrained above 0.01 and B the relative position bias.
#'
#' @param q Query matrix (n_q x d).
#' @param k Key matrix (n_k x d).
#' @param v Value matrix (n_k x d_v).
#' @param tau Temperature, must exceed 0.01 (paper constraint).
#' @param B Optional (n_q x n_k) bias added after the scaled cosine.
#' @return List with \code{output}, \code{weights},
#'   \code{similarities}, \code{tau}, \code{estimate}, \code{n},
#'   \code{method}.
#' @references Liu, Z. et al. (2022), CVPR 2022, arXiv:2111.09883,
#'   Section 3.2. Local source:
#'   fetched-wave3/liu-etal-2022-swin-v2-arxiv2111.09883.pdf.
#' @export
Vitscn <- function(q, k, v, tau = 0.1, B = NULL) {
  Qa <- as.matrix(q); Ka <- as.matrix(k); Va <- as.matrix(v)
  storage.mode(Qa) <- "double"; storage.mode(Ka) <- "double"
  storage.mode(Va) <- "double"
  t <- as.numeric(tau)[1]
  if (!(t > 0.01)) stop(sprintf("Vitscn: tau must exceed 0.01 (paper constraint), got %g", t), call. = FALSE)
  if (ncol(Qa) != ncol(Ka)) stop(sprintf("Vitscn: q width %d != k width %d", ncol(Qa), ncol(Ka)), call. = FALSE)
  if (nrow(Ka) != nrow(Va)) stop(sprintf("Vitscn: k has %d rows but v has %d", nrow(Ka), nrow(Va)), call. = FALSE)
  for (nm in c("q", "k", "v")) {
    a <- switch(nm, q = Qa, k = Ka, v = Va)
    if (!all(is.finite(a))) stop(sprintf("Vitscn: %s contains non-finite values", nm), call. = FALSE)
  }
  nq <- nrow(Qa); nk <- nrow(Ka)
  if (is.null(B)) {
    Bm <- matrix(0, nq, nk)
  } else {
    Bm <- as.matrix(B); storage.mode(Bm) <- "double"
    if (nrow(Bm) != nq || ncol(Bm) != nk) {
      stop(sprintf("Vitscn: B must be (%d, %d)", nq, nk), call. = FALSE)
    }
  }
  qn <- sqrt(rowSums(Qa * Qa)); kn <- sqrt(rowSums(Ka * Ka))
  if (any(qn == 0) || any(kn == 0)) {
    stop("Vitscn: cosine similarity undefined for a zero row", call. = FALSE)
  }
  S <- (Qa %*% t(Ka)) / (qn %o% kn) / t + Bm
  W <- t(apply(S, 1L, function(row) {
    m <- max(row); e <- exp(row - m); e / sum(e)
  }))
  if (nk == 1L) W <- matrix(W, nrow = nq)
  out <- W %*% Va
  list(output = out, weights = W, similarities = S, tau = t,
       estimate = out[1, 1], n = nq,
       method = "scaled cosine attention cos(q,k)/tau + B (Liu et al. 2022, Sec 3.2)")
}
