# SPDX-License-Identifier: AGPL-3.0-or-later
#
# ViLBERT two-stream co-attention (Vilbrt). Alias arm: delegates to
# morie_geron_vilbert, the single implementation in this tree. Mirror
# of src/morie/fn/vilbrt.py.

#' ViLBERT two-stream co-attention
#'
#' Two transformer streams, one per modality, exchanging information
#' through co-attentional layers: image queries attend over text keys
#' and values while text queries attend over image keys and values.
#' There is exactly one implementation: this function delegates to
#' \code{morie_geron_vilbert}.
#'
#' @param image Image-region feature matrix.
#' @param text Text-token feature matrix.
#' @param d_model Common projection width. Default 8.
#' @param seed RNG seed for the deterministic projection draw. Default 0.
#' @return List as returned by \code{morie_geron_vilbert}.
#' @references Lu, J., Batra, D., Parikh, D. and Lee, S. (2019),
#'   ViLBERT: Pretraining Task-Agnostic Visiolinguistic Representations
#'   for Vision-and-Language Tasks, NeurIPS 32, arXiv:1908.02265,
#'   Section 3.1 and Figure 2. Source PDF:
#'   fetched-wave3/lu-etal-2019-vilbert-arxiv1908.02265.pdf.
#' @examples
#' Vilbrt(matrix(0.1, 2, 3), matrix(0.2, 2, 3))$estimate
#' @export
Vilbrt <- function(image, text, d_model = 8, seed = 0) {
  morie_geron_vilbert(image, text, d_model = d_model, seed = seed)
}
