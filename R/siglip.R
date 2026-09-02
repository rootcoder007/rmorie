# SPDX-License-Identifier: AGPL-3.0-or-later
#' Contrastive loss that scores each pair on its own
#'
#' CLIP softmax normalises over the whole batch, so every pair loss
#' depends on every other pair and the batch must be huge and globally
#' synchronised. A sigmoid per pair removes that coupling: the loss
#' decomposes and needs no all-gather. The learnable bias exists because
#' a batch is overwhelmingly negatives.
#'
#' Formula:
#' \code{L = -(1/|B|) sum_ij log sigmoid(z_ij (t x_i . y_j + b))},
#' \code{z_ij = 1} on the diagonal, \code{-1} elsewhere.
#'
#' @param image_emb Image embeddings; L2-normalised here.
#' @param text_emb Text embeddings; L2-normalised here.
#' @param t_prime Logit scale.
#' @param bias Logit bias.
#' @return List with \code{estimate}, \code{logits}, \code{acc}, \code{n}.
#' @references Zhai, Mustafa, Kolesnikov & Beyer (2023). ICCV 2023,
#'   equation (3).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Siglip(V, V)
Siglip <- function(image_emb, text_emb, t_prime = 1, bias = 0) {
  A <- as.matrix(image_emb)
  B <- as.matrix(text_emb)
  n <- nrow(A)
  unit <- function(M) M / sqrt(rowSums(M^2))
  A <- unit(A)
  B <- unit(B)
  logits <- t_prime * (A %*% t(B)) + bias
  Z <- matrix(-1, n, n)
  diag(Z) <- 1
  loss <- -sum(log(.s4_expit(Z * logits))) / n
  hit <- sum(apply(logits, 1, which.max) == seq_len(n))
  .t1_result(estimate = loss, logits = logits, acc = hit / n, n = n,
             method = "SigLIP pairwise sigmoid loss")
}
