# SPDX-License-Identifier: AGPL-3.0-or-later
#
# WordPiece tokenizer (Wpiece). Alias arm: delegates to
# morie_geron_wordpiece_tokenizer, the single implementation in this
# tree. Mirror of src/morie/fn/wpiece.py.

#' WordPiece subword tokenizer
#'
#' Trains subword merges by unigram-likelihood gain: the pair A, B
#' maximising freq(AB) / (freq(A) freq(B)) is merged, continuation
#' pieces carry the double-hash prefix, and segmentation is greedy
#' longest-match-first. There is exactly one implementation: this
#' function delegates to \code{morie_geron_wordpiece_tokenizer}.
#'
#' @param corpus Character vector of training text.
#' @param vocab_size Target vocabulary size. Default 50.
#' @return List with \code{vocab}, \code{merges}, \code{scores},
#'   \code{tokenize}, \code{alphabet}, \code{estimate}, \code{n},
#'   \code{method}.
#' @references Schuster, M. and Nakajima, K. (2012), Japanese and
#'   Korean voice search, IEEE ICASSP 2012, 5149-5152. Devlin, J.,
#'   Chang, M.-W., Lee, K. and Toutanova, K. (2019), BERT: Pre-training
#'   of Deep Bidirectional Transformers for Language Understanding,
#'   NAACL-HLT 2019, arXiv:1810.04805. Wu, Y. et al. (2016), Googles
#'   Neural Machine Translation System, arXiv:1609.08144, Section 4.1.
#'   Source PDFs: fetched-wave3/devlin-etal-2019-bert-arxiv1810.04805.pdf
#'   and fetched-wave3/wu-etal-2016-gnmt-wordpiece-arxiv1609.08144.pdf.
#' @examples
#' Wpiece("hug hug hugs pug pun", vocab_size = 14)$estimate
#' @export
Wpiece <- function(corpus, vocab_size = 50) {
  morie_geron_wordpiece_tokenizer(corpus, vocab_size = vocab_size)
}
