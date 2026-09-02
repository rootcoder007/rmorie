# SPDX-License-Identifier: AGPL-3.0-or-later
#' Centre of a compositional data set
#'
#' Formula: xi-hat = C(g_1, ..., g_D), g_i the geometric mean of the ith component
#'
#' @param X One composition per row; all entries strictly positive.
#' @param total Constant the closure sums to.

#' @param X See Usage.
#' @param total See Usage.
#' @return List with ``center``, ``geometric_mean``, ``n``, ``D``.
#' @references Aitchison, A Concise Guide to Compositional Data Analysis, Chapter 2. Verified against the text: the centre estimate is xi-hat = C(g_1, ..., g_D) with g_i the geometric mean of the ith component, and totvar(x) = trace(Gamma) = (1/D) sum_\{i<j\} var\{log(x_i/x_j)\}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compcen(V)
Compcen <- function(X, total = 1) {
  X <- as.matrix(X)
  if (any(X <= 0)) stop("compositions must be strictly positive")
  g <- exp(colMeans(log(X)))
  .t1_result(
    center = as.numeric(total) * g / sum(g), geometric_mean = g,
    n = nrow(X), D = ncol(X),
    method = "Compositional centre (closed geometric mean)"
  )
}
