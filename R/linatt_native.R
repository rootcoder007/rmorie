# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Linformer low-rank attention (Linatt). Bit-identical mirror of
# src/morie/fn/linatt.py.

#' Linformer linear-complexity attention via low-rank projection
#'
#' Wang, Li, Khabsa, Fang and Ma (2020), "Linformer: Self-Attention
#' with Linear Complexity", arXiv:2006.04768, Eq 7: with projection
#' matrices E, F (k x n) applied to the key and value layers,
#' head = softmax(Q (E K)' / sqrt(d_k)) (F V), so the attention matrix
#' is n x k and cost drops from O(n^2) to O(n k). With k = n and
#' E = F = I this reduces exactly to standard scaled dot-product
#' attention (the test anchor).
#'
#' @param Q Query matrix (n x d_k).
#' @param K Key matrix (n x d_k).
#' @param V Value matrix (n x d_v).
#' @param E Key projection (k x n).
#' @param F_ Value projection (k x n).
#' @return List with \code{output}, \code{weights}, \code{projected_K},
#'   \code{projected_V}, \code{k}, \code{estimate}, \code{n},
#'   \code{method}.
#' @references Wang, S., Li, B. Z., Khabsa, M., Fang, H. and Ma, H.
#'   (2020), arXiv:2006.04768, Section 4, Eq 7. Local source:
#'   fetched-wave3/wang-etal-2020-linformer-arxiv2006.04768.pdf.
#' @export
Linatt <- function(Q, K, V, E, F_) {
  Qa <- as.matrix(Q); Ka <- as.matrix(K); Va <- as.matrix(V)
  Ea <- as.matrix(E); Fa <- as.matrix(F_)
  storage.mode(Qa) <- "double"; storage.mode(Ka) <- "double"
  storage.mode(Va) <- "double"; storage.mode(Ea) <- "double"
  storage.mode(Fa) <- "double"
  n <- nrow(Qa); dk <- ncol(Qa)
  if (ncol(Ka) != dk) stop(sprintf("Linatt: K width %d != Q width %d", ncol(Ka), dk), call. = FALSE)
  if (nrow(Ka) != nrow(Va)) stop(sprintf("Linatt: K has %d rows but V has %d", nrow(Ka), nrow(Va)), call. = FALSE)
  if (ncol(Ea) != nrow(Ka)) stop(sprintf("Linatt: E must be (k, n) with n = %d", nrow(Ka)), call. = FALSE)
  if (ncol(Fa) != nrow(Va)) stop(sprintf("Linatt: F must be (k, n) with n = %d", nrow(Va)), call. = FALSE)
  if (nrow(Ea) != nrow(Fa)) stop(sprintf("Linatt: E and F must share k, got %d and %d", nrow(Ea), nrow(Fa)), call. = FALSE)
  for (nm in c("Q", "K", "V", "E", "F")) {
    a <- switch(nm, Q = Qa, K = Ka, V = Va, E = Ea, F = Fa)
    if (!all(is.finite(a))) stop(sprintf("Linatt: %s contains non-finite values", nm), call. = FALSE)
  }
  EK <- Ea %*% Ka
  FV <- Fa %*% Va
  S <- (Qa %*% t(EK)) * (1 / sqrt(dk))
  P <- t(apply(S, 1L, function(row) {
    m <- max(row); e <- exp(row - m); e / sum(e)
  }))
  if (nrow(Ea) == 1L) P <- matrix(P, nrow = n)
  out <- P %*% FV
  list(output = out, weights = P, projected_K = EK, projected_V = FV,
       k = nrow(Ea), estimate = out[1, 1], n = n,
       method = "Linformer low-rank attention (Wang et al. 2020, Eq 7)")
}
