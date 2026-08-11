# SPDX-License-Identifier: AGPL-3.0-or-later
#
# BART denoising encoder-decoder (Barte). Alias arm: delegates to
# morie_geron_bart, the single implementation in this tree. Mirror of
# src/morie/fn/barte.py.

#' BART text-infilling denoising step
#'
#' The BART text-infilling corruption: contiguous spans with Poisson
#' (lambda = 3) lengths are each replaced by a SINGLE mask token, so
#' the decoder must recover both content and span length; the score is
#' reconstruction cross-entropy of the target. There is exactly one
#' implementation: this function delegates to \code{morie_geron_bart}.
#'
#' @param src Source token sequence.
#' @param tgt Target token sequence.
#' @param mask_ratio Fraction of tokens corrupted. Default 0.3.
#' @param mean_span Poisson mean span length. Default 3.
#' @param permute Also permute sentences. Default FALSE.
#' @param model Optional scoring model.
#' @param seed RNG seed for the corruption draw. Default 0.
#' @return List as returned by \code{morie_geron_bart}.
#' @references Lewis, M., Liu, Y., Goyal, N., Ghazvininejad, M.,
#'   Mohamed, A., Levy, O., Stoyanov, V. and Zettlemoyer, L. (2020),
#'   BART: Denoising Sequence-to-Sequence Pre-training for Natural
#'   Language Generation, Translation, and Comprehension, ACL 2020,
#'   arXiv:1910.13461, Section 2.2. Source PDF:
#'   fetched-wave3/lewis-etal-2020-bart-denoising-seq2seq-arxiv1910.13461.pdf.
#' @examples
#' Barte(c("a", "b", "c", "d"), c("a", "b", "c", "d"))$estimate
#' @export
Barte <- function(src, tgt, mask_ratio = 0.3, mean_span = 3,
                  permute = FALSE, model = NULL, seed = 0) {
  morie_geron_bart(src, tgt, mask_ratio = mask_ratio, mean_span = mean_span,
                   permute = permute, model = model, seed = seed)
}
