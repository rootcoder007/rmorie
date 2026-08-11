# SPDX-License-Identifier: AGPL-3.0-or-later
#
# LayerNorm -- per-token normalization (Layrnm). Alias arm: delegates to
# morie_geron_layer_normalization, the single implementation in this tree.
# Mirror of src/morie/fn/layrnm.py.

#' LayerNorm per-token normalization
#'
#' Normalises each instance across its features, then applies a learned
#' gain and bias: y equals gamma times (x - mu) / sqrt(sigma2 + eps)
#' plus beta, with eps inside the square root. Statistics come from a
#' single training case, which is what distinguishes layer norm from
#' batch norm. There is exactly one implementation: this function
#' delegates to \code{morie_geron_layer_normalization}.
#'
#' @param X Numeric vector or matrix, shape d or m by d.
#' @param gamma Learned gain, scalar or length d. Default 1.
#' @param beta Learned shift, scalar or length d. Default 0.
#' @param eps Positive stabiliser inside the square root. Default 1e-5.
#' @return List with \code{output}, \code{normalized}, \code{mean},
#'   \code{variance}, \code{estimate}, \code{n}, \code{method}.
#' @references Ba, J. L., Kiros, J. R. and Hinton, G. E. (2016),
#'   Layer Normalization, arXiv:1607.06450, Section 3. Source PDF:
#'   fetched-wave3/ba-kiros-hinton-2016-layer-normalization-arxiv1607.06450.pdf.
#' @examples
#' Layrnm(c(1, 3), eps = 0)$normalized
#' @export
Layrnm <- function(X, gamma = 1, beta = 0, eps = 1e-05) {
  morie_geron_layer_normalization(X, gamma = gamma, beta = beta, eps = eps)
}
