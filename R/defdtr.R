# SPDX-License-Identifier: AGPL-3.0-or-later

# Bilinear sample of a feature map at a real-valued location.
#' Bilinear sample of a feature map at a real-valued location
#'
#' A step of the defdtr implementation. Called by \code{Defdtr}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param F A matrix; indexed by row and column.
#' @param y Numeric; passed to \code{max}.
#' @param x Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.defdtr_bilinear <- function(F, y, x) {
  H <- nrow(F); W <- ncol(F)
  y <- min(max(y, 0), H - 1)
  x <- min(max(x, 0), W - 1)
  y0 <- floor(y); x0 <- floor(x)
  y1 <- min(y0 + 1, H - 1); x1 <- min(x0 + 1, W - 1)
  dy <- y - y0; dx <- x - x0
  F[y0 + 1, x0 + 1] * (1 - dy) * (1 - dx) +
    F[y0 + 1, x1 + 1] * (1 - dy) * dx +
    F[y1 + 1, x0 + 1] * dy * (1 - dx) +
    F[y1 + 1, x1 + 1] * dy * dx
}

#' Deformable DETR attention
#'
#' Formula: K reference points per query; sparse attention
#'
#' Each query attends to only K sampled locations around its reference
#' point instead of every position, so the cost is O(K) rather than
#' O(HW) and convergence no longer needs 500 epochs.  The samples are
#' read by bilinear interpolation, which is what makes the learned
#' offsets differentiable.  With zero offsets and equal weights the
#' output is exactly the feature at the reference point.
#'
#' @param x An H x W feature map.
#' @param queries A Q x 2 matrix of reference points in [0, 1].
#' @param K Sampling points per query.
#' @param offsets Q x K x 2 offsets in pixels, or NULL.
#' @param weights Q x K attention weights, or NULL for 1/K each.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{out}, \code{samples},
#'   \code{ref_pixels}, \code{Q}, \code{K}, \code{method}.
#' @references Zhu et al. (2021), Deformable DETR, ICLR 2021.
#' @export
Defdtr <- function(x, queries, K = 4, offsets = NULL, weights = NULL,
                   seed = 42) {
  F <- .s03mat(x)
  H <- nrow(F)
  if (H == 0L) stop("empty input: x has no rows")
  W <- ncol(F)
  Qm <- .s03mat(queries)
  Q <- nrow(Qm)
  if (Q == 0L || ncol(Qm) != 2L)
    stop("queries must be a Q x 2 matrix of reference points")
  K <- as.integer(K)
  if (K < 1L) stop("K must be at least 1")
  e <- .ghc_rng(seed)
  off <- array(0, c(Q, K, 2))
  if (is.null(offsets)) {
    for (q in seq_len(Q)) for (k in seq_len(K)) for (c in 1:2)
      off[q, k, c] <- .ghc_norm(e, 1L, 0, 1)
  } else {
    flat <- .s03vec(offsets)
    if (length(flat) != Q * K * 2)
      stop("offsets must hold Q x K x 2 values")
    for (q in seq_len(Q)) for (k in seq_len(K)) for (c in 1:2)
      off[q, k, c] <- flat[((q - 1L) * K + (k - 1L)) * 2L + c]
  }
  wt <- matrix(1 / K, Q, K)
  if (!is.null(weights)) {
    flat <- .s03vec(weights)
    if (length(flat) != Q * K) stop("weights must hold Q x K values")
    for (q in seq_len(Q)) for (k in seq_len(K))
      wt[q, k] <- flat[(q - 1L) * K + k]
  }
  out <- numeric(Q)
  samples <- matrix(0, Q, K)
  refs <- matrix(0, Q, 2)
  for (q in seq_len(Q)) {
    ry <- Qm[q, 2] * (H - 1)
    rx <- Qm[q, 1] * (W - 1)
    refs[q, ] <- c(ry, rx)
    s <- 0; tot <- 0
    for (k in seq_len(K)) tot <- tot + wt[q, k]
    for (k in seq_len(K)) {
      v <- .defdtr_bilinear(F, ry + off[q, k, 1], rx + off[q, k, 2])
      samples[q, k] <- v
      s <- s + wt[q, k] * v
    }
    out[q] <- if (tot != 0) s / tot else 0
  }
  tot <- 0
  for (v in out) tot <- tot + v
  .t1_result(estimate = tot / Q, out = out, samples = samples,
             ref_pixels = refs, Q = Q, K = K,
             method = "Deformable DETR sparse attention")
}
