# SPDX-License-Identifier: AGPL-3.0-or-later
#' Optimal transport cost on the line, which needs no solver
#'
#' In one dimension the optimal coupling is always monotone: sort both
#' samples and match in order. No linear program is needed, which is why
#' this case is the building block for sliced Wasserstein.
#'
#' Formula: \code{W_1 = int |F(t) - G(t)| dt}, equal to
#' \code{mean |x_(i) - y_(i)|} for equal sample sizes.
#'
#' @param x,y Samples of equal length.
#' @return List with \code{W1}, \code{estimate}, \code{n}.
#' @references Vallender, S. S. (1973). Theory Probab Appl 18:784-786.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otws(V, V)
Otws <- function(x, y) {
  xs <- sort(as.numeric(x)); ys <- sort(as.numeric(y)); n <- length(xs)
  w <- sum(abs(xs - ys)) / n
  .t1_result(W1 = w, estimate = w, n = n,
             method = "One-dimensional Wasserstein-1 distance")
}
