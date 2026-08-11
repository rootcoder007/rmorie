# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Single-step GBLUP relationship matrix H and inverse (Singgw).
# Bit-identical mirror of src/morie/fn/singgw.py.

#' Single-step relationship matrix H (ssGBLUP)
#'
#' Given the pedigree numerator relationship matrix A over all
#' individuals, a genomic relationship matrix G over the genotyped
#' subset (block 1) and the non-genotyped remainder (block 2), the
#' joint matrix of Christensen and Lund (2010), eq. (4) (the H of
#' Legarra, Aguilar and Misztal 2009 and Aguilar et al. 2010), is
#' \eqn{H_{11} = G_w}, \eqn{H_{12} = G_w A_{11}^{-1} A_{12}},
#' \eqn{H_{21} = A_{21} A_{11}^{-1} G_w},
#' \eqn{H_{22} = A_{22} + A_{21} A_{11}^{-1} (G_w - A_{11}) A_{11}^{-1} A_{12}},
#' with \eqn{G_w = (1-w) G + w A_{11}} blending a polygenic fraction w
#' (their sec. The combined genetic effect; w = 0 gives raw eq. 4).
#' The inverse is the sparse update of their eq. (8) (eq. 6 at w = 0):
#' \eqn{H^{-1} = A^{-1} + [G_w^{-1} - A_{11}^{-1}, 0; 0, 0]}.
#'
#' Limiting cases exercised in the tests: G = A11 gives H = A exactly;
#' with every individual genotyped, H = Gw.
#'
#' @param A Pedigree numerator relationship matrix (n x n).
#' @param G Genomic relationship matrix over the genotyped subset, in
#'   the order of \code{genotyped}.
#' @param genotyped 1-based indices of genotyped individuals within A.
#' @param w Polygenic blending weight in [0, 1); eq. (8) requires
#'   w greater than 0 for guaranteed invertibility when G is singular.
#' @return List with \code{estimate} (H), \code{Hinv}, \code{Gw},
#'   \code{genotyped}, \code{w}, \code{n}, \code{n_genotyped},
#'   \code{method}.
#' @references Christensen, O. F. and Lund, M. S. (2010). Genomic
#'   prediction when some animals are not genotyped. Genetics
#'   Selection Evolution 42, 2; eqs. (4), (6), (8), p. 3
#'   (fetched-wave3 PDF). Legarra, A., Aguilar, I. and Misztal, I.
#'   (2009). Journal of Dairy Science 92(9), 4656-4663. Aguilar, I.,
#'   et al. (2010). Journal of Dairy Science 93(2), 743-752.
#' @export
Singgw <- function(A, G, genotyped, w = 0.0) {
  A <- as.matrix(A); storage.mode(A) <- "double"
  G <- as.matrix(G); storage.mode(G) <- "double"
  n <- nrow(A)
  if (ncol(A) != n) stop("A must be square", call. = FALSE)
  gset <- as.integer(genotyped)
  q <- length(gset)
  if (q == 0L) stop("need at least one genotyped individual", call. = FALSE)
  if (length(unique(gset)) != q || min(gset) < 1L || max(gset) > n) {
    stop("genotyped indices must be unique and within A", call. = FALSE)
  }
  if (!all(dim(G) == c(q, q))) {
    stop("G must be q x q in the order of genotyped", call. = FALSE)
  }
  w <- as.numeric(w)
  if (!(w >= 0 && w < 1)) stop("w must be in [0, 1)", call. = FALSE)
  others <- setdiff(seq_len(n), gset)
  if (length(others) == 0L) {
    # every individual genotyped: H = Gw exactly (limiting case of
    # eq. 4, Christensen and Lund 2010 p. 3)
    inv_idx <- integer(n)
    inv_idx[gset] <- seq_len(n)
    Gw <- (1 - w) * G + w * A[gset, gset, drop = FALSE]
    H <- Gw[inv_idx, inv_idx, drop = FALSE]
    Hinv <- solve(Gw)[inv_idx, inv_idx, drop = FALSE]
    return(list(
      estimate = H, Hinv = Hinv, Gw = Gw,
      genotyped = gset, w = w, n = as.integer(n),
      n_genotyped = as.integer(q),
      method = "Single-step H (Christensen-Lund 2010 eq. 4/8; ssGBLUP)"))
  }
  idx <- c(gset, others)
  Ap <- A[idx, idx, drop = FALSE]
  A11 <- Ap[seq_len(q), seq_len(q), drop = FALSE]
  A12 <- Ap[seq_len(q), -seq_len(q), drop = FALSE]
  A21 <- Ap[-seq_len(q), seq_len(q), drop = FALSE]
  A22 <- Ap[-seq_len(q), -seq_len(q), drop = FALSE]
  Gw <- (1 - w) * G + w * A11
  A11inv <- solve(A11)
  B <- A11inv %*% A12
  H11 <- Gw
  H12 <- Gw %*% B
  H21 <- t(B) %*% Gw
  H22 <- A22 + t(B) %*% (Gw - A11) %*% B
  Hp <- rbind(cbind(H11, H12), cbind(H21, H22))
  inv_idx <- integer(n)
  inv_idx[idx] <- seq_len(n)
  H <- Hp[inv_idx, inv_idx, drop = FALSE]
  Ainv <- solve(Ap)
  Gwinv <- solve(Gw)
  Hinvp <- Ainv
  Hinvp[seq_len(q), seq_len(q)] <-
    Hinvp[seq_len(q), seq_len(q), drop = FALSE] + Gwinv - A11inv
  Hinv <- Hinvp[inv_idx, inv_idx, drop = FALSE]
  list(
    estimate = H, Hinv = Hinv, Gw = Gw,
    genotyped = gset, w = w, n = as.integer(n), n_genotyped = as.integer(q),
    method = "Single-step H (Christensen-Lund 2010 eq. 4/8; ssGBLUP)")
}
