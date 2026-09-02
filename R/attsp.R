# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparse (masked) scaled dot-product attention.
#'
#' softmax(mask_S(Q K' / sqrt(d))) V, with the score set to -Inf wherever
#' the connectivity matrix S is zero.
#'
#' @param Q Queries, nq x d.
#' @param K Keys, nk x d.
#' @param V Values, nk x dv.
#' @param S Connectivity mask, nq x nk; NULL is dense attention.
#'
#' @return List with out, weight, score, nq, nk, d, dv, density.
#' @references Child, Gray, Radford and Sutskever (2019),
#'   arXiv:1904.10509, Sects. 3-4 (in the local corpus); Beltagy, Peters
#'   and Cohan (2020), arXiv:2004.05150; scaled dot-product attention is
#'   Vaswani et al. (2017), Equation (1).
#' @export
#' @examples
#' Sparseattn(Q = 0.5, K = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
Sparseattn <- function(Q, K, V, S = NULL) {
  Qm <- .t1_mat(Q); Km <- .t1_mat(K); Vm <- .t1_mat(V)
  nq <- nrow(Qm); d <- ncol(Qm); nk <- nrow(Km)
  if (ncol(Km) != d) stop("Q and K must share their last dimension")
  if (nrow(Vm) != nk) stop("V must have one row per key")
  dv <- ncol(Vm)
  Sm <- if (is.null(S)) matrix(1, nq, nk) else .t1_mat(S)
  if (nrow(Sm) != nq || ncol(Sm) != nk) stop("S must be nq by nk")
  sco <- (Qm %*% t(Km)) / sqrt(d)
  dim(sco) <- c(nq, nk)
  Wt <- matrix(0, nq, nk)
  for (i in seq_len(nq)) {
    allow <- which(Sm[i, ] != 0)
    if (length(allow) == 0L) stop(sprintf("row %d of S allows no key", i - 1L))
    e <- rep(0, nk)
    e[allow] <- exp(sco[i, allow] - max(sco[i, allow]))
    Wt[i, ] <- e / sum(e)
  }
  out <- Wt %*% Vm
  dim(out) <- c(nq, dv)
  .t1_result(out = out, weight = Wt, score = sco, nq = nq, nk = nk,
             d = d, dv = dv, density = sum(Sm != 0) / (nq * nk),
             method = "Sparse scaled dot-product attention (Child et al. 2019)")
}
