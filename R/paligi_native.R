# SPDX-License-Identifier: AGPL-3.0-or-later
#
# ALiBi per-head linear-bias attention (Paligi). Alias arm: delegates to
# Atalib, the single implementation in this tree. Mirror of
# src/morie/fn/paligi.py.
#
# FABRICATED LEAD, RECORDED: the stub cited Faisal and Anastasopoulos
# (2022) for a parametric ALiBi with sigmoid-of-learnable slopes. No such
# paper exists (arXiv and ACL Anthology records checked 2026-08-09). The
# real source is Press, Smith and Lewis (2022), which REJECTS trainable
# slopes (Section 3) and fixes head k of n at m_k = 2^(-8k/n).

#' ALiBi attention with per-head linear positional bias
#'
#' Attention with linear biases: the pre-softmax score of query i and
#' key j is penalised by slope m times the distance between i and j, and
#' no position embeddings are used anywhere. Head k of n receives the
#' geometric slope 2^(-8k/n). Pass \code{slopes} for explicit per-head
#' slopes. There is exactly one implementation: this function delegates
#' to \code{Atalib}.
#'
#' @param y Optional scores input, see \code{Atalib}.
#' @param Q Query matrix.
#' @param K Key matrix.
#' @param V Value matrix.
#' @param slopes Optional explicit head slope or slopes.
#' @param causal Mask keys after the query position. Default FALSE.
#' @return List as returned by \code{Atalib}.
#' @references Press, O., Smith, N. A. and Lewis, M. (2022), Train
#'   Short, Test Long: Attention with Linear Biases Enables Input Length
#'   Extrapolation, ICLR 2022, arXiv:2108.12409, page 4 and Section 3.
#'   Source PDF:
#' fetched-wave3/press-smith-lewis-2022-alibi-train-short-test-long-arxiv2108.12409.pdf.
#' @examples
#' Paligi(Q = diag(2), K = diag(2), V = diag(2))$estimate
#' @export
Paligi <- function(y = NULL, Q = NULL, K = NULL, V = NULL, slopes = NULL,
                   causal = FALSE) {
  Atalib(y = y, Q = Q, K = K, V = V, slopes = slopes, causal = causal)
}
