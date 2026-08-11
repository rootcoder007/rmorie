# SPDX-License-Identifier: AGPL-3.0-or-later
#
# FlashAttention IO-aware exact attention (Flsh2). Alias arm: delegates to
# morie_geron_flash_attention, the single implementation in this tree.
# Mirror of src/morie/fn/flsh2.py.

#' FlashAttention tiled exact attention
#'
#' Block-tiled streaming attention with online softmax: the running
#' row maximum, row sum and accumulator are rescaled by
#' exp(m_old - m_new) as key/value blocks stream through, so the output
#' equals softmax of Q K transpose over sqrt(d) times V exactly while
#' only one tile of scores is ever materialised. There is exactly one
#' implementation: this function delegates to
#' \code{morie_geron_flash_attention}.
#'
#' @param Q Query matrix, N by d.
#' @param K Key matrix, M by d.
#' @param V Value matrix, M by dv.
#' @param block_size Tile side, positive integer. Default 2.
#' @param causal Mask keys after the query position. Default FALSE.
#' @return List with \code{output}, \code{direct_output},
#'   \code{max_abs_error}, \code{row_max}, \code{row_sum},
#'   \code{n_blocks}, \code{peak_score_memory}, \code{naive_score_memory},
#'   \code{memory_ratio}, \code{estimate}, \code{n}, \code{method}.
#' @references Dao, T., Fu, D. Y., Ermon, S., Rudra, A. and Re, C.
#'   (2022), FlashAttention: Fast and Memory-Efficient Exact Attention
#'   with IO-Awareness, NeurIPS 35, arXiv:2205.14135, Algorithm 1.
#'   Source PDF: fetched-wave3/dao-etal-2022-flashattention-arxiv2205.14135.pdf.
#' @examples
#' Flsh2(matrix(0, 1, 1), matrix(c(1, 3), 2, 1), matrix(c(1, 3), 2, 1))$output
#' @export
Flsh2 <- function(Q, K, V, block_size = 2, causal = FALSE) {
  morie_geron_flash_attention(Q, K, V, block_size = block_size, causal = causal)
}
