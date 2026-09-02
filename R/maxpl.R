# SPDX-License-Identifier: AGPL-3.0-or-later
#' Max pooling operation for CNNs
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 533-577\], Chapter 13, Section 13.4, pp. 542-543, read as a
#' rendered page.  "The max pooling operation summarizes the input as the
#' maximum within a rectangular neighborhood"; the window slides by the
#' stride and the output size in the l-th layer is
#' L^(l+1) = (L^(l) - P^(l))/S^(l) + 1.  The worked example of Fig. 13.6 is
#' quoted in the text: the first output is 7, "the maximum value of the four
#' elements that conform to the bounds of the filter (3, 3, 7, 4)", the second
#' is 5, "the max of 3, 4, 4, and 5", and the last is 6, "a max of 3, 2, 5,
#' and 6".  Those three printed windows are the anchor.
#'
#' This is the one-dimensional case, y\[i\] = max(x\[i*S : i*S+P\]).
#'
#' @param x the activation map to pool.
#' @param kernel window width P, a positive integer no wider than x.
#' @param stride step S, a positive integer.
#' @return list: estimate, pooled, argmax, n, method.
#' @keywords internal
#' @examples
#' Maxpl(c(3, 3, 7, 4), 4, 1)$estimate
#' @export
Maxpl <- function(x, kernel, stride) {
  v <- .s03vec(x)
  n <- length(v)
  if (n == 0L) stop("max_pooling: x is empty")
  P <- as.integer(kernel)
  S <- as.integer(stride)
  if (is.na(P) || P <= 0L) stop("max_pooling: kernel must be a positive integer")
  if (is.na(S) || S <= 0L) stop("max_pooling: stride must be a positive integer")
  if (P > n) stop("max_pooling: kernel is wider than the input")
  m <- (n - P) %/% S + 1L
  pooled <- numeric(m)
  where <- integer(m)
  for (i in seq_len(m)) {
    a <- (i - 1L) * S + 1L
    best <- a
    if (P > 1L) for (j in seq(a + 1L, a + P - 1L)) if (v[j] > v[best]) best <- j
    pooled[i] <- v[best]
    where[i] <- best - 1L
  }
  list(estimate = pooled[1], pooled = pooled, argmax = where, n = m,
       method = "y[i] = max(x[i*S : i*S+P]), Chapter 13 Sect. 13.4 with L' = (L-P)/S + 1")
}
