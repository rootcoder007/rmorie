# SPDX-License-Identifier: AGPL-3.0-or-later
#' Apply a dropout mask with inverted scaling.
#'
#' Formula: a_i = x_i * m_i / (1 - rate),  m_i in {0, 1} supplied by the caller
#'
#' @param x Activations of the layer being regularized.
#' @param mask Keep/drop indicator per unit: 1 keeps, 0 drops.  Supplied by the caller so the result is reproducible.
#' @param rate Dropout rate in [0, 1); the surviving activations are divided by 1 - rate.
#'
#' @return List with ``activation``, ``kept``, ``dropped``, ``rate``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.6 p. 404 describes dropout as setting a random fraction of the weights of the input or hidden neurons to zero, so their contribution is removed on the forward pass and they receive no update on the backward pass; it prints no formula and attributes the method to Srivastava, Hinton, Krizhevsky, Sutskever and Salakhutdinov (2014), Dropout: A Simple Way to Prevent Neural Networks from Overfitting, JMLR 15:1929-1958, which is where the 1/(1 - rate) inverted scaling comes from.  The mask is an argument rather than drawn internally so the function is deterministic.  Chapter read from the PDF; the scaling is from the paper the book names.
#' @export
Dropmask <- function(x, mask, rate) {
  x <- .t1_vec(x); m <- .t1_vec(mask); rate <- as.numeric(rate)
  if (length(x) != length(m)) stop("x and mask must have the same length")
  if (rate < 0 || rate >= 1) stop("rate must lie in [0, 1)")
  if (any(m != 0 & m != 1)) stop("mask entries must be 0 or 1")
  s <- 1 / (1 - rate); kept <- sum(m)
  .t1_result(activation = x * m * s, kept = kept, dropped = length(x) - kept,
             rate = rate, n = length(x),
             method = "Inverted dropout, MVSML Sect. 10.6")
}
