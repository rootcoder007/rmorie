# SPDX-License-Identifier: AGPL-3.0-or-later
#' Layer normalisation
#'
#' Formula: mu = (1/H) sum_i a_i, sigma = sqrt((1/H) sum_i (a_i - mu)^2), y = gamma (a - mu)/sigma + beta
#'
#' @param x One sample per row; a flat sequence is treated as one sample.
#' @param gamma Per-unit gain; ones if omitted.
#' @param beta Per-unit bias; zeros if omitted.

#' @param x See Usage.
#' @param gamma See Usage.
#' @param beta See Usage.
#' @return List with ``y``, ``mu``, ``sigma``, ``n``, ``H``.
#' @references Ba, Kiros and Hinton (2016), Layer Normalization, arXiv:1607.06450, equation (3). Verified against the paper.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Vitlnorm(V)
Vitlnorm <- function(x, gamma = NULL, beta = NULL) {
  X <- if (is.matrix(x)) x else matrix(.t1_vec(x), nrow = 1L)
  n <- nrow(X)
  H <- ncol(X)
  g <- if (is.null(gamma)) rep(1, H) else .t1_vec(gamma)
  b <- if (is.null(beta)) rep(0, H) else .t1_vec(beta)
  if (length(g) != H || length(b) != H) stop("gamma and beta must have length H")
  mus <- rowMeans(X)
  sds <- sqrt(rowSums((X - mus)^2) / H)
  if (any(sds <= 0)) stop("a row has zero variance; layer norm is undefined")
  Y <- sweep(sweep((X - mus) / sds, 2, g, "*"), 2, b, "+")
  .t1_result(y = Y, mu = mus, sigma = sds, n = n, H = H,
             method = "Layer normalisation")
}
