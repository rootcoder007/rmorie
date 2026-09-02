# SPDX-License-Identifier: AGPL-3.0-or-later
#' Quantize a key embedding to one bit per sketch row
#'
#' The sign of a random projection keeps enough information to estimate
#' an inner product, so a stored key costs one bit per sketch row and
#' nothing else: no per-group scale, no zero point, no grouping. That is
#' what makes the overhead zero.
#'
#' Formula: \code{H_S(k) = sign(S k)} with S an m by d standard normal
#' matrix; zero maps to +1 so the range is \code{{-1, +1}^m}.
#'
#' @param k Key embedding.
#' @param S_mat JL sketch matrix, m by d.
#' @return List with \code{signs}, \code{m}, \code{d}, \code{estimate}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). QJL: 1-bit
#'   quantized JL transform for KV cache quantization with zero
#'   overhead. arXiv:2406.03482, definition 3.1.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Tqhs(V, V)
Tqhs <- function(k, S_mat) {
  kv <- as.numeric(k)
  Sm <- as.matrix(S_mat)
  signs <- .s4_sgn(as.numeric(Sm %*% kv))
  .t1_result(signs = signs, m = length(signs), d = length(kv),
             estimate = sum(signs) / length(signs),
             method = "QJL sign quantizer H_S(k) = sign(S k)")
}
