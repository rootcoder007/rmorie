# SPDX-License-Identifier: AGPL-3.0-or-later
#
# GPT-2 decoder-only forward pass (Gpt2). Alias arm: delegates to
# morie_geron_gpt2, the single implementation in this tree. Mirror of
# src/morie/fn/gpt2.py.

#' GPT-2 decoder-only language model
#'
#' The decoder-only transformer forward pass at the four released
#' GPT-2 sizes (BPE vocabulary 50257, context 1024); the architecture
#' is the masked self-attention decoder of GPT scaled up. There is
#' exactly one implementation: this function delegates to
#' \code{morie_geron_gpt2}.
#'
#' @param X Input token embedding matrix.
#' @param n_layers Optional depth override.
#' @param n_heads Optional head-count override.
#' @param size One of small, medium, large, xl. Default small.
#' @param ... Further configuration passed through.
#' @return List as returned by \code{morie_geron_gpt2}.
#' @references Radford, A., Wu, J., Child, R., Luan, D., Amodei, D.
#'   and Sutskever, I. (2019), Language Models are Unsupervised
#'   Multitask Learners, OpenAI technical report, Section 2.3 and
#'   Table 2. Radford, A., Narasimhan, K., Salimans, T. and
#'   Sutskever, I. (2018), Improving Language Understanding by
#'   Generative Pre-Training, OpenAI. Source PDF:
#'   fetched-wave3/radford-etal-2019-gpt2-unsupervised-multitask-learners.pdf.
#' @examples
#' Gpt2(matrix(0.1, 2, 4), n_layers = 1, n_heads = 1)$estimate
#' @export
Gpt2 <- function(X, n_layers = NULL, n_heads = NULL, size = "small", ...) {
  morie_geron_gpt2(X, n_layers = n_layers, n_heads = n_heads, size = size, ...)
}
