# SPDX-License-Identifier: AGPL-3.0-or-later
#' Total variance of a compositional data set
#'
#' Formula: totvar(x) = trace(Gamma) = (1/D) sum_\{i<j\} var\{log(x_i/x_j)\}
#'
#' @param X One composition per row; strictly positive.

#' @param X See Usage.
#' @return List with ``totvar``, ``totvar_trace``, ``clr_var``, ``n``, ``D``.
#' @references Aitchison, A Concise Guide to Compositional Data Analysis, Chapter 2. Verified against the text: the centre estimate is xi-hat = C(g_1, ..., g_D) with g_i the geometric mean of the ith component, and totvar(x) = trace(Gamma) = (1/D) sum_\{i<j\} var\{log(x_i/x_j)\}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Comptotvar(V)
Comptotvar <- function(X) {
  X <- as.matrix(X)
  if (any(X <= 0)) stop("compositions must be strictly positive")
  L <- log(X)
  D <- ncol(X)
  n <- nrow(X)
  tot <- 0
  for (i in seq_len(D - 1)) {
    for (j in (i + 1):D) {
      tot <- tot + stats::var(L[, i] - L[, j])
    }
  }
  tot <- tot / D
  clr <- L - rowMeans(L)
  cv <- apply(clr, 2, stats::var)
  .t1_result(
    totvar = tot, totvar_trace = sum(cv), clr_var = cv,
    n = n, D = D, method = "Compositional total variance"
  )
}
